(ns com.pwinckles.hcc
  (:require [clojure.math.combinatorics :as combo]
            [clojure.tools.cli :refer [parse-opts]]
            [clojure.pprint :as pp])
  (:gen-class)
  (:import (java.text DecimalFormat)))

(defrecord Card [suit rank])

(defn card-suit
  [^Card card]
  (.suit card))

(defn card-rank
  [^Card card]
  (.rank card))

(defn create-deck
  "Creates a vec of cards given an input map that specifies the number
  of suits, :suits, number of copies, :copies, and vec of ranks, :ranks."
  [{:keys [suits ranks copies]}]
  (into []
        (mapcat flatten)
        (for [suit (range suits)
              rank ranks]
          (take copies (repeat (->Card suit rank))))))

(defn deal
  "Shuffles the deck (seq of cards) and returns a lazy-seq of n cards."
  [deck n]
  (take n (shuffle deck)))

(defn group-by-rank
  "Groups the seq of cards by rank."
  [cards]
  (group-by card-rank cards))

(defn group-by-suit
  "Groups the sec of cards by suit."
  [cards]
  (group-by card-suit cards))

(defn sort-by-rank
  "Sorts the seq of cards by rank."
  [cards]
  (sort-by card-rank cards))

(defn sorted-and-grouped
  "Sorts the seq of cards by rank and then groups them by suit."
  [cards]
  (group-by-suit (sort-by-rank cards)))

(defn partition-by-unique
  "The input card seq MUST contain only a single suit and be sorted by rank.
  The output is a vec of vecs, where each inner vec contains a unique set of
  cards from the original seq. For example, [2 3 3 4 5 5] would produce
  [[2 3 4 5] [3 5]]."
  [cards]
  (loop [card          (first cards)
         cards         (rest cards)
         previous-rank nil
         n             0
         result        []]
    (if (nil? card)
      result
      (let [rank          (card-rank card)
            next-card     (first cards)
            next-cards    (rest cards)
            update-result (fn [suit]
                            (update result suit (fnil #(conj % card) [])))]
        (if (= rank previous-rank)
          (recur next-card
                 next-cards
                 previous-rank
                 (inc n)
                 (update-result (inc n)))
          (recur next-card next-cards rank 0 (update-result 0)))))))

(defn identify-sequences
  "The input card seq MUST contain only unique cards from a single suit
  and be sorted. The output is a vec of tuples, where the first element
  is the starting rank of a sequence and the second element is the sequence
  length. For example, [2 3 4 5 7 8 9] would produce [[2 4] [7 3]]."
  [cards]
  (loop [card    (first cards)
         cards   (rest cards)
         current []
         result  []]
    (if (nil? card)
      result
      (if (or (empty? current)
              (= (dec (card-rank card)) (card-rank (last current))))
        (recur (first cards) (rest cards) (conj current card) result)
        (let [result (if (< (count current) 2)
                       result
                       (conj result
                             [(card-rank (first current)) (count current)]))]
          (recur (first cards) (rest cards) [card] result))))))

(def sequence-permutations
  "Given a sequence tuple, first element is starting rank and second element
  is length, compute all possible sub-sequences of length 2 or more. For example,
  [2 4] would produce [[2 2] [2 3] [2 4] [3 2] [3 3] [4 2]]."
  (memoize (fn [[start length]]
             (for [i (range 2 (+ length 1))
                   s (range start (- (+ start length 1) i))]
               [s i]))))

(defn bomb?
  "Given a seq of cards return true if it's a bomb. Suit is NOT verified."
  [cards]
  (and (= (count cards) 4)
       (= [3 5 7 9] (sort (map card-rank cards)))))

(defn count-bombs
  "Given a seq of seqs of cards, determine how many of the inner seqs are bombs.
  For example, [[3 9 7 5] [3 3 3 3] [2 5 7 7]] would return 1."
  [sets]
  (reduce (fn [acc cards]
            (if (bomb? cards)
              (inc acc)
              acc))
          0
          sets))

(defn count-bombs-suited
  "Given a map of cards, grouped by suit, where the values are sorted odd
  cards in that suit, then return the number of suited bombs contained in
  the map."
  [odds-by-suit]
  (count-bombs (into []
                     (comp (map (fn [[_k v]] (partition-by-unique v)))
                           (mapcat (fn [sets]
                                     (if (= (count sets) 1)
                                       sets
                                       (apply combo/cartesian-product sets)))))
                     odds-by-suit)))

(defn count-bombs-rainbow
  "Given a map of cards, grouped by suit, where the values are sorted odd
  cards in that suite, then return the number or rainbow bombs contained
  in the map."
  [odds-by-suit]
  (if (< (count odds-by-suit) 4)
    0
    (count-bombs (mapcat #(apply combo/cartesian-product %)
                  (combo/combinations (vals odds-by-suit) 4)))))

(defn evaluate-sets
  "Given a seq of cards, return a map of the counts of sets in the seq.
  The map is keyed off the set and the value is the count, eg: {1 10, 2 2}."
  [cards]
  (reduce (fn [acc [_rank group]]
            (update acc (count group) (fnil inc 0)))
          {}
          (group-by-rank cards)))

(defn evaluate-sequences
  "Given a seq of cards, return a map of the counts of sequences in the seq.
  The map is keyed off the sequence type and the value is the count, eg:
  {\"1x3\" 2, \"2x2\" 1}."
  [cards]
  (let [sequences (into []
                        (comp (mapcat partition-by-unique)
                              (mapcat identify-sequences)
                              (mapcat sequence-permutations))
                        (vals (sorted-and-grouped cards)))]
    (reduce (fn [acc [[start length] c]]
              (if (and (= 1 c) (< length 3))
                acc
                (let [updated (update acc (str c "x" length) (fnil inc 0))]
                  (if (= 1 c)
                    updated
                    (recur updated [[start length] (dec c)])))))
            {}
            (frequencies sequences))))

(defn evaluate-bombs
  "Given a seq of cards, return a map of the counts of bombs in the seq.
  The map is structured as follows: {:suited {1 1}, :rainbow {1 1, 2 1}}.
  The key of the inner maps indicates the number of bombs in the seq and
  the value will always be 1. This is a silly way to represent it, but
  it makes for easy merging of results later where bombs are treated a
  little differently."
  [cards]
  (let [odds-by-suit  (sorted-and-grouped (filter #(odd? (card-rank %)) cards))
        suited-count  (count-bombs-suited odds-by-suit)
        rainbow-count (count-bombs-rainbow odds-by-suit)]
    {:suited  (reduce (fn [acc c] (assoc acc c 1))
                      {}
                      (range 1 (inc suited-count))),
     :rainbow (reduce (fn [acc c] (assoc acc c 1))
                      {}
                      (range 1 (inc rainbow-count)))}))

(defn evaluate-combinations
  "Given a seq of cards, return a map detailing the combinations found.
  The map has three keys, :sets, :sequences, and :bombs. See the other
  eval functions for a description of the values."
  [cards]
  (let [results {:sets      (evaluate-sets cards),
                 :sequences (evaluate-sequences cards),
                 :bombs     (evaluate-bombs cards)}]

    ;; useful for examining strange hands
    (comment
      (cond
        (> (count (get-in results [:bombs :rainbow])) 9)
        (do
          (println "Rainbow:" (count (get-in results [:bombs :rainbow])))
          (pp/pprint (sorted-and-grouped cards)))

        (> (count (get-in results [:bombs :suited])) 2)
        (do
          (println "Suited:" (count (get-in results [:bombs :suited])))
          (pp/pprint (sorted-and-grouped cards)))))

    results))

(defn merge-results
  "Merges combination result maps."
  [r1 r2]
  {:sets      (merge-with + (:sets r1) (:sets r2)),
   :sequences (merge-with + (:sequences r1) (:sequences r2)),
   :bombs     {:suited  (merge-with +
                                    (get-in r1 [:bombs :suited])
                                    (get-in r2 [:bombs :suited])),
               :rainbow (merge-with +
                                    (get-in r1 [:bombs :rainbow])
                                    (get-in r2 [:bombs :rainbow]))}})

(defn evaluate-hands
  "Given n, the number of hands to evaluate, and a deck, seq of cards,
  evaluate the combinations found in n random hands dealt from the deck,
  and return a merged result set, see evaluate-combinations."
  [n deck hand-size]
  (let [results (eduction (map evaluate-combinations)
                          (repeatedly n #(deal deck hand-size)))]
    (reduce (fn [acc result]
              (merge-results acc result))
            {}
            results)))

(defn run
  "Evaluate the combinations in the specified number of random deals from
  the deck described by the deck-composition map, {:suit n, :ranks [], :copies n},
  using the specified number of threads. Return a merged map of the results,
  see evaluate-combinations."
  [deck-composition hand-size deals threads]
  (let [deck    (create-deck deck-composition)
        n       (long (/ deals threads))
        r       (rem deals threads)
        futures (cond-> (vec (repeatedly threads
                                         #(future
                                           (evaluate-hands n deck hand-size))))
                  (> r 0) (conj (future (evaluate-hands r deck hand-size))))]
    (reduce (fn [acc result]
              (merge-results acc @result))
            {}
            futures)))

(defn display
  "Display a combination result map for the specified number of deals to sysout."
  [results deals]
  (let [df        (DecimalFormat.
                   (str "0.0" (.repeat "#" deals)))
        print-seq (fn [result]
                    (doseq [[s c] (into (sorted-map) result)]
                      (println (str " "  (format "%2s" s)
                                    ": " (.format df (double (/ c deals)))))))]
    (println "Deals:" deals)
    (println "Sets")
    (print-seq (:sets results))
    (println "Sequences")
    (print-seq (:sequences results))
    (println "Rainbow Bombs")
    (if (empty? (get-in results [:bombs :rainbow]))
      (println " None")
      (print-seq (get-in results [:bombs :rainbow])))
    (println "Suited Bombs")
    (if (empty? (get-in results [:bombs :suited]))
      (println " None")
      (print-seq (get-in results [:bombs :suited])))))

(defn run-and-display
  [deck-composition deals hand-size threads]
  (display (run deck-composition hand-size deals threads) deals))

(def cli-opts
  [["-d" "--deals DEALS" "Number of hands to deal"
    :parse-fn #(Long/parseLong %)
    :validate [#(< 0 %) "Must be a number greater than 0"]]
   ["-s" "--suits SUITS" "Number of suits in the deck"
    :default 4
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 10) "Must be a number between 0 and 10"]]
   ["-c" "--copies COPIES" "Number of copies of each card"
    :default 1
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 % 10) "Must be a number between 0 and 10"]]
   ["-H" "--hand-size SIZE" "Number of cards in a hand"
    :default 14
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 %) "Must be a number greather than 0"]]
   ["-t" "--threads THREADS" "Number of threads to run on"
    :default (.availableProcessors (Runtime/getRuntime))
    :parse-fn #(Integer/parseInt %)
    :validate [#(< 0 %) "Must be a number greater than 0"]]
   ["-h" "--help"]])

(defn -main
  [& args]
  (let [{:keys [options errors summary]} (parse-opts args cli-opts)
        errors (if-not (:deals options)
                 (conj errors "Missing required option \"-d DEALS\"")
                 errors)]
    (cond
      (:help options)
      (println summary)

      (seq errors)
      (doseq [error errors]
        (println error))

      :else
      (run-and-display {:suits  (:suits options),
                        :copies (:copies options),
                        :ranks  (vec (range 2 11))}
                       (:deals options)
                       (:hand-size options)
                       (:threads options)))

    (System/exit 0)))
