(ns text-mining-with-clj.ch1
  (:require [clojisr.v1.r :as r :refer [r r+ ->code]]
            [clojisr.v1.rserve :as rserve]
            [clojisr.v1.require :refer [require-r]]
            [clojisr.v1.applications.plotting :refer [plot->svg plot->file]]
            [notespace.v2.note :refer [note note-md note-hiccup note-as-hiccup]]
            [text-mining-with-clj.common :refer [long-str]])
  (:require [notespace.v2.live-reload]))

(note-md
 (long-str "#Chapter 1: \"The tidy text format\", **Text Mining with R**"
           "#### (Exercises translated into Clojure R-interop from the original: [https://www.tidytextmining.com/tidytext.html](https://www.tidytextmining.com/tidytext.html))"))


(note-md
 (long-str "In what follows, you will find a translation of exaples found in the above chapter from"
           "the book **Text Mining with R: A Tidy Approach** by Julia Silge and David Robinson."
           "The goal of this document is to illustrate how do what they have demonstrated using"
           "R-interop in Clojure. For a full examination of the ideas, please see their original text."
           "To the degree that there is commentary in this text, it will largely relate to the"
           "nature of the Clojure interop with R."
           ))

(note-md
 (str "## Some Setup"))

(note
 (rserve/set-as-default!)
 (r/discard-all-sessions))

(note
 (require-r '[dplyr :as d]
            '[tidytext :as t]
            '[janeaustenr :as j]
            '[stringr :as stringr]))

(note-md
 (str "## 1.2: The `unnest_tokens` function"))

(note
 (def text ["Because I could not stop for Death -"
            "He kindly stopped for me -"
            "The Carriage held but just Ourselves -"
            "and Immortality"]))

(note-md
 (long-str "Now we have a text to analyze. Let's parse it with `unnest_tokens`."
           "First we create a tibble, note that instead of specifying arguments"
           "in the R way `text=text`, we can use Clojure symbols, e.g. `:text text`."
           "What is particularly exciting here is that `text` is a Clojure data structure."
           ""
           "Another thing to take note of is how we call `t/unnest_tokens`. In R, the"
           "function `unest_tokens` takes a dataframe or table as the first argument, "
           "and then an argument specifying output and input. The output and input arguments"
           "are \"passed by expression and support quasiquotation\", that means you can specify"
           "a column using a symbol.\""
           ""
           "In R, you would just write the word plainly and that's a"
           "symbol, i.e. `unnest_tokens(word, text)`. Because we are in Clojure and we do not"
           "want Clojure to evaluate `word` and `text` as symbols, we need to quote the expression"
           "with `'`. Another option would be to call `(r/rsymbol \"word\")`."
           ""
           "Lastly, note our use of a pipe macro `->`. This is like using R's pipe notation `%>%`."
           ))

(note
 (let [data (d/tibble :lines (range 1 5) :text text)]
   (-> data (t/unnest_tokens 'word 'text))))


(note-md
 (long-str "## 1.3: Tidying the Works of Jane Austen" ))

(note-md
 (long-str "First let's add a column indicating the chapter of each line in the text."
           "Note how we use `let` to capture a reference to the R function `cumsum`. Once"
           "we do that we can use the R function in Clojure, as if it were a Clojure function."))

(note
 (let [cumsum (r 'cumsum)]
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
 (str "Now we can count word occurences."))

(note
 (-> tidy-books (d/count 'word :sort true)))

(note-md
 (str "And then plot using R's `ggplot2` library!"))

(note
 (require-r '[ggplot2 :as gg]))

(note-md
 (str "Note that here we are using a helper function provided by clojisr `r/r+`."
      "In R, using in-fix notation, you would write: `ggplot(aes(word, n)) + geom_col() + ...`"))

(note-as-hiccup
 (let [reorder (r 'reorder)]
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
 (long-str "## 1.4-5: Gutenbergr Package & Word Frequencies"
           "First we pull books by H.G. Wells from the Gutenberg R package."))

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

(note-md (str "And the same for books by the Bronte sisters."))

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
         (tdr/gather 'author 'proportion ["H.G. Wells" "Brönte Sisters"])))
   frequency))

(note-md
 (long-str "And now we can plot comparison frequencies between the corpuses of each author"
           "as compared to Jane Austen. We'll export this chart as a file and then render it"
           "in a separate note because the number of data points will produce a large SVG"
           "and slow the page down considerably."))

(note
 (def static-resource-target-path (notespace.v2.note/ns->out-dir *ns*)))

(note
 (let [abs (r "abs")]
   (plot->file
    (str static-resource-target-path "frequencies.png")
    (-> frequency
        (gg/ggplot (gg/aes :x 'proportion :y (symbol "`Jane Austen`")
                           :color '(abs (- ~(symbol "`Jane Austen`") proportion))))
        (r+ (gg/geom_abline :color "gray40" :lty 2)
            (gg/geom_jitter :alpha 0.1 :size 2.5 :width 0.3 :height 0.3)
            (gg/geom_text (gg/aes :label 'word) :check_overlap true :vjust 1.5)
            (gg/scale_x_log10 :labels '(scales/percent_format))
            (gg/scale_y_log10 :labels '(scales/percent_format))
            (gg/scale_color_gradient :limits [0 0.001] :low "darkslategray4" :high "gray75")
            (gg/facet_wrap ''author :ncol 2)
            (gg/theme :legend.position "none")
            (gg/labs :y "Jane Austen" :x nil)))
    :width 800
    :height 700)))

(note-hiccup [:img {:src "frequencies.png"}])
