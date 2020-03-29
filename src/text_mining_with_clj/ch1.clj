(ns text-mining-with-clj.ch1
  (:require [clojisr.v1.r :as r :refer [r ->code]]
            [clojisr.v1.rserve :as rserve]
            [clojisr.v1.require :refer [require-r]]
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
 (str "## Tidying the Works of Jane Austen"))

(note
 (let [regex (r "regex")
       cumsum (r "cumsum")]
       (-> (j/austen_books)
           (d/group_by 'book)
           (d/mutate :linenumber (d/row_number)
                     :chapter (-> 'text
                                  ;; (stringr/str_detect (regex "^chapter [\\divxlc]"))
                                  ;; (cumsum)
                                  )))))



