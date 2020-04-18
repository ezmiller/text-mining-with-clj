(ns text-mining-with-clj.common
  (:require [notespace.v2.note :refer [note note-md note-hiccup note-as-hiccup]]))

(defn long-str [& strings]
  (clojure.string/join "\n" strings))
