(ns text-mining-with-clj.ch1
  (:require [clojisr.v1.r :as r :refer [r r+ ->code]]
            [clojisr.v1.rserve :as rserve]
            [clojisr.v1.require :refer [require-r]]
            [clojisr.v1.applications.plotting :refer [plot->svg]]
            [notespace.v2.note :refer [note note-md]])
  (:require [notespace.v2.live-reload]))


(note-md
 (str "## Setup"))

(note
 (rserve/set-as-default!)
 (r/discard-all-sessions))

(note
 (require-r '[dplyr :as d]
            '[tidytext :as t]
            '[janeaustenr :as j]
            '[stringr :as stringr]))

(note
 (defn long-str [& strings]
   (clojure.string/join "\n" strings)))

(note-md
 (str "## The unnest_tokens function"))

(note
 (def text ["Because I could not stop for Death -"
            "He kindly stopped for me -"
            "The Carriage held but just Ourselves -"
            "and Immortality"]))

(note
 (let [d (r.dplyr/tibble :lines (range 1 5) :text text)]
   (t/unnest_tokens d 'word 'text)))

(note-md
 (long-str "## Tidying the Works of Jane Austen"
           "First let's add a column indicating the chapter of each line in the text."))

(note
 (let [cumsum (r "cumsum")]
   (def original-books
     (-> (j/austen_books)
         (d/group_by 'book)
         (d/mutate :linenumber '(d/row_number)
                   :chapter  (-> '(stringr/str_detect
                                   'text
                                   (stringr/regex "^chapter [\\\\divxlc]" :ignore_case true))
                                cumsum))
         (d/ungroup)))))

(note-md
 (str "Next we will tokenize each line and remove common stop words."))

(note
 (def tidy-books (-> original-books
                     (t/unnest_tokens 'word 'text)
                     (d/anti_join 'stop_words))))

(note-md
 (str "Now we can count wrod occurences."))

(note
 (-> tidy-books (d/count 'word :sort true)))

(note-md
 (str "And plot!"))

(note
 (require-r '[ggplot2 :as gg]))

(note
 (let [reorder (r "reorder")]
   (-> tidy-books
       (d/count 'word :sort true)
       (d/filter '(> n 600))
       (d/mutate :word '(reorder word n))
       (gg/ggplot (gg/aes :x 'word :y 'n))
       (r+ (gg/geom_col) (gg/xlab nil))
       plot->svg
       )))

;; (notespace.v2.note/reset-state!)
