(ns text-mining-with-clj.ch1
  (:require [clojisr.v1.r :as r :refer [r r+ ->code]]
            [clojisr.v1.rserve :as rserve]
            [clojisr.v1.require :refer [require-r]]
            [clojisr.v1.applications.plotting :refer [plot->svg plot->file]]
            [notespace.v2.note :refer [note note-md note-hiccup note-as-hiccup]])
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

(comment
  (notespace.v2.note/ns->out-dir *ns*))

(note
 ;;(-> (java.io.File. "/resources/") .getAbsolutePath)
 ;; (def target-path
 ;;   (-> (java.io.File. "/resources/") .getAbsolutePath))
 "resources"
 )

(note
 (defn backtick [string]
   (symbol (format "`%s`" string)))
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
 (long-str "## Tidying the Works of Jane Austen"           "First let's add a column indicating the chapter of each line in the text."))

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

(note-as-hiccup
 (let [reorder (r "reorder")]
   (plot->svg
    (-> tidy-books
        (d/count 'word :sort true)
        (d/filter '(> n 600))
        (d/mutate :word '(reorder word, n))
        (gg/ggplot (gg/aes 'word, 'n))
        (r+ (gg/geom_col)

            (gg/xlab nil)
            (gg/coord_flip))))))

(note-md
 (long-str "##The gutenbergr Package"
           "We'll retreieve a bunch of H.G. Wells' books."))

(note
 (require-r '[gutenbergr :as gbr]))

(note
 (def hgwells (gbr/gutenberg_download [35 36 5230 159])))

(note-md (str "Let's see what the most common words in his books are."))

(note
 (def tidy-hgwells (-> hgwells
                       (t/unnest_tokens 'word 'text)
                       (d/anti_join 'stop_words))))

(note
 (-> tidy-hgwells
     (d/count 'word :sort true)))


(note
 (def bronte (gbr/gutenberg_download [1260 768 969 9182 767])))

(note
 (def tidy-bronte (-> bronte
                      (t/unnest_tokens 'word 'text)
                      (d/anti_join 'stop_words))))

(note
 (-> tidy-bronte
     (d/count 'word :sort true)))


(note-md
 (long-str "We'll now calculate the frequency for each word in the works of Jane Austen"
           ", the Brönte sisters, and H.G. Wells by binding the data frames together."))

(note
 (require-r '[tidyr :as tdr]
            '[scales :as scales]))

(note
 (let [sum (r "sum")]
   (def frequency
     (-> (d/bind_rows
          (d/mutate tidy-bronte :author "Brönte Sisters")
          (d/mutate tidy-hgwells :author "H.G. Wells")
          (d/mutate tidy-books :author "Jane Austen"))
         (d/mutate :word '(stringr/str_extract word "[a-z']+")) ;; clean words
         (d/count 'author 'word)
         (d/group_by 'author)
         (d/mutate :proportion '(/ n (sum n)))
         (d/select '-n) ;; drop the n column
         (tdr/spread 'author 'proportion)
         (tdr/gather 'author 'proportion ["H.G. Wells" "Brönte Sisters"])))))

(note frequency)

(note
 (let [abs (r "abs")]
   (plot->file
    (str "frequencies.png")
    (-> frequency
        (gg/ggplot (gg/aes :x 'proportion :y (backtick "Jane Austen")
                           :color '(abs (- ~(backtick "Jane Austen") proportion))))
        (r+ (gg/geom_abline :color "gray40" :lty 2)
            (gg/geom_jitter :alpha 0.1 :size 2.5 :width 0.3 :height 0.3)
            (gg/geom_text (gg/aes :label 'word) :check_overlap true :vjust 1.5)
            (gg/scale_x_log10 :labels '(scales/percent_format))
            (gg/scale_y_log10 :labels '(scales/percent_format))
            (gg/scale_color_gradient :limits [0 0.001] :low "darkslategray4" :high "gray75")
            (gg/facet_wrap ''author :ncol 2)
            (gg/theme :legend.position "none")
            (gg/labs :y "Jane Austen" :x nil))))))
