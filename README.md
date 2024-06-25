# Haggis Combo Calculator

Attempts to compute the frequency of Haggis card combinations.

## Requirements

[Install Clojure.](https://clojure.org/guides/install_clojure)

## Usage

See `clojure -M:run-m --help` for all options.

```shell
# 2p deck
clojure -M:run-m --deals 1000000

# 3p deck
clojure -M:run-m --deals 1000000 --suits 5

# 4p deck
clojure -M:run-m --deals 1000000 --copies 2
```