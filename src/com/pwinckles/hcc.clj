(ns com.pwinckles.hcc
  (:require [clojure.math.combinatorics :as combo])
  (:gen-class)
  (:import (java.text DecimalFormat)))

;; TODO move
(def deck-composition
  {:suits  4,
   :ranks  [2 3 4 5 6 7 8 9 10],
   :copies 1})

(defrecord Card [suit rank])

(defn card-suit
  [^Card card]
  (.suit card))

(defn card-rank
  [^Card card]
  (.rank card))

(defn create-deck
  [{:keys [suits ranks copies]}]
  (into []
        (mapcat flatten)
        (for [suit (range suits)
              rank ranks]
          (take copies (repeat (->Card suit rank))))))

(defn deal
  [deck n]
  (take n (shuffle deck)))

(defn group-by-rank
  [cards]
  (group-by card-rank cards))

(defn group-by-suit
  [cards]
  (group-by card-suit cards))

(defn sort-by-rank
  [cards]
  (sort-by card-rank cards))

(defn sorted-and-grouped
  [cards]
  (group-by-suit (sort-by-rank cards)))

(defn partition-by-unique
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
  (memoize (fn [[start length]]
             (for [i (range 2 (+ length 1))
                   s (range start (- (+ start length 1) i))]
               [s i]))))

(defn bomb?
  [cards]
  (and (= (count cards) 4)
       (= [3 5 7 9] (sort (map card-rank cards)))))

(defn count-bombs
  [sets]
  (reduce (fn [acc cards]
            (if (bomb? cards)
              (inc acc)
              acc))
          0
          sets))

(defn count-bombs-suited
  [odds-by-suit]
  (count-bombs (mapcat partition-by-unique (vals odds-by-suit))))

(defn count-bombs-rainbow
  [odds-by-suit]
  (if (< (count odds-by-suit) 4)
    0
    (count-bombs (into []
                       (mapcat #(apply combo/cartesian-product %))
                       (combo/combinations (vals odds-by-suit) 4)))))

(defn evaluate-sets
  [cards]
  (reduce (fn [acc [_rank group]]
            (update acc (count group) (fnil inc 0)))
          {}
          (group-by-rank cards)))

(defn evaluate-sequences
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
  [cards]
  {:sets      (evaluate-sets cards),
   :sequences (evaluate-sequences cards),
   :bombs     (evaluate-bombs cards)})

(defn merge-results
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
  [n deck]
  (let [results (eduction (map evaluate-combinations)
                          (repeatedly n #(deal deck 14)))]
    (reduce (fn [acc result]
              (merge-results acc result))
            {}
            results)))

(defn run
  [deck-composition deals threads]
  (let [deck    (create-deck deck-composition)
        n       (long (/ deals threads))
        futures (vec (repeatedly threads #(future (evaluate-hands n deck))))]
    (reduce (fn [acc result]
              (merge-results acc @result))
            {}
            futures)))

(defn display
  [results deals threads]
  (let [n         (* (long (/ deals threads)) threads)
        df        (DecimalFormat.
                   (str "0.0" (apply str (repeat (count (str n)) "#"))))
        print-seq (fn [result]
                    (doseq [[s c] (into (sorted-map) result)]
                      (println (str " "  s
                                    ": " (.format df (double (/ c n)))))))]
    (println "Deals:" n)
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
  [deck-composition deals threads]
  (display (run deck-composition deals threads) deals threads))

(defn -main
  [& args]
)
