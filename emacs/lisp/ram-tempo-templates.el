;;* templates

;;* templates: org-mode

;;** templates/org-mode: code blocks

;;*** templates/org-mode/code blocks: copy previous block

(tempo-define-template
 "ram-org-prev-code-block-copy"
 '(
   (ram-org-prev-code-block-copy)
   )
 "<bc"
 "A copy of the previous Org code block."
 'ram-tempo-org-template-tags)

;;*** templates/org-mode/code blocks: R

;; R for plotting/visualizing
;;  - automatically
;;    + name the block (from #+name: or headline etc)
;;    + create a unique filename in /tmp directory
;;    + copy content of the previous block for plotting
;;      - if none such blocks exist, create empty block

(tempo-define-template
 "R-block-graph-with-content"
 '((tempo-save-named 'count (number-to-string (+ 1 (ram-get-org-code-block-img-count))))
   (tempo-save-named 'block-name (ram-make-code-block-name))
   ;; !!! TODO: fix two calls to the same function.
   ;; that returns '(headers block-content)
   ;; a quick fix to fetch #+header: :var ...
   ;; use same function for getting the same previous block
   ;; it seems that the template cannot access list elements.
   (tempo-save-named 'prev-block-content
                      (cadr (ram-get-prev-org-code-block-value-for-R-graph))
                     )
   (tempo-save-named 'prev-block-var_headers
                      (car (ram-get-prev-org-code-block-value-for-R-graph))
                      )
   "#+name: " (s block-name) "_img" (s count) n
   ~ (s prev-block-var_headers)
   "#+header: :file /tmp/" (s block-name) "_img" (s count) ".png" n
   "#+header: :results output graphics file" n
   "#+header: :width 1400 :height 900 :res 200 :units px" n
   "#+begin_src R :session my-R-session" n
   ~ (s prev-block-content)
   "#+end_src"  n n
   "#+ATTR_HTML: :width 300" n
   "#+ATTR_ORG: :width 800" n
   "#+RESULTS: " (s block-name) "_img" (s count) n
   ^
   )
 "<rgc"
 "R Org code block displaying graphics with a copy of previous R block."
 'ram-tempo-org-template-tags
 )

(tempo-define-template "R-block-with-previous-non-graph-block-content"
                       ;; R blocks which has no '((:results . graphics)) params
                       '(
                         (ram-org-prev-code-block-copy-for-lang-with-or-without-params
                          'R nil '((:results . graphics))))
                       "<rbc"
                       "Previous Org code block for R excluding ((:results . graphics))."
                       'ram-tempo-org-template-tags)

;;*** templates/org-mode/code blocks: python

;; Python for plotting/visualizing
;;  - automatically
;;    + name the block (from #+name: or headline etc)
;;    + create a unique filename in /tmp directory
;;    + copy content of the previous block for plotting
;;      - if none such blocks exist, create empty block

(tempo-define-template
 "Python-block-graph-with-content"
 '((tempo-save-named 'count (number-to-string (+ 1 (ram-get-org-code-block-img-count))))
   (tempo-save-named 'block-name (ram-make-code-block-name))
   ;; !!! TODO: fix two calls to the same function.
   ;; that returns '(headers block-content)
   ;; a quick fix to fetch #+header: :var ...
   ;; use same function for getting the same previous block
   ;; it seems that the template cannot access list elements.
   (tempo-save-named 'prev-block-var_headers
                      (car (ram-get-prev-org-code-block-value-for-Python-graph))
                      )
   (tempo-save-named 'prev-block-content
                      (cadr (ram-get-prev-org-code-block-value-for-Python-graph))
                     )
   "#+name: " (s block-name) "_img" (s count) n
   ~ (s prev-block-var_headers)
   "#+header: :var fname=\"/tmp/" (s block-name) "_img" (s count) ".png\"" n
   "#+header: :file /tmp/" (s block-name) "_img" (s count) ".png" n
   "#+header: :results file link" n
   "#+begin_src python :session my-Python-session" n
   ~ (s prev-block-content)
   "#+end_src"  n n
   "#+RESULTS: " (s block-name) "_img" (s count) n
   ^
   )
 "<pgc"
 "Python Org code block displaying graphics with a copy of previous R block."
 'ram-tempo-org-template-tags
 )

(tempo-define-template "Python-block-with-previous-non-graph-block-content"
                       ;; Python blocks which has no '((:results . graphics)) params
                       '(
                         (ram-org-prev-code-block-copy-for-lang-with-or-without-params
                          'python nil '((:results . graphics))))
                       "<pbc"
                       "Previous Org code block for Python excluding ((:results . graphics))."
                       'ram-tempo-org-template-tags)

;;*** templates/org-mode/code blocks: bash/shell


(tempo-define-template "BASH-block-with-previous-block-content"
                       '(
                         (ram-org-prev-code-block-copy-for-lang-with-or-without-params
                          'shell nil nil))
                       "<shbc"
                       "Previous Org code block for shell."
                       'ram-tempo-org-template-tags)

;;*** templates/org-mode/code blocks: emacs-lisp

(tempo-define-template "EMACS-LISP-block-with-previous-block-content"
                       '(
                         (ram-org-prev-code-block-copy-for-lang-with-or-without-params
                          'emacs-lisp nil nil))
                       "<elbc"
                       "Previous Org code block for emacs-lisp."
                       'ram-tempo-org-template-tags)

;;* templates: testing


(tempo-define-template "tempo-org-foo-test"
                       '("<tempo-org-foo></tempo-org-foo>")
                       "<orgfoo"
                       "Testing"
                       'ram-tempo-org-template-tags)


(tempo-define-template  "buzz"
                        '("<buzz>""</buzz>")
                        "<buzz"
                        "An repeated 'buzz' should messup exapnssion."
                        'ram-tempo-elisp-template-tags)

;;* custom user elements

;;** custom user elements: set a point to jump to

(defvar tempo-initial-pos nil
  "Initial position to hold cursor after expansion.")

(defun ram-tempo-insert-template-position (element)
  "Store an `tempo-initial-pos' to place the cursor after expansion."
  (when (eq '~ element)
    (setq tempo-initial-pos (point-marker))
    ""))

(add-to-list 'tempo-user-element-functions #'ram-tempo-insert-template-position)

;;** custom user elements: jump to a point

(defun ram-tempo-goto-template-position (element)
  "Store an `tempo-initial-pos' to place the cursor after expansion."
  (when (eq '^ element)
    (when (integer-or-marker-p tempo-initial-pos)
      (goto-char tempo-initial-pos)
      (setq tempo-initial-pos nil))
    ""))

(add-to-list 'tempo-user-element-functions #'ram-tempo-goto-template-position)


;;* tempel templates


;;* fundamental-mode ;; Available anywhere

;; (today (format-time-string "%Y-%m-%d"))

;;* prog-mode

;;(fixme (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "FIXME ")
;;(todo (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "TODO ")
;;(bug (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "BUG ")
;;(hack (if (derived-mode-p 'emacs-lisp-mode) ";; " comment-start) "HACK ")

;;* latex-mode

;;(abstract "\\begin{abstract}\n" r> n> "\\end{abstract}")
;;(align "\\begin{align}\n" r> n> "\\end{align}")
;;(alignn "\\begin{align*}\n" r> n> "\\end{align*}")
;;(gather "\\begin{gather}\n" r> n> "\\end{gather}")
;;(gatherr "\\begin{gather*}\n" r> n> "\\end{gather*}")
;;(appendix "\\begin{appendix}\n" r> n> "\\end{appendix}")
;;(begin "\\begin{" (s env) "}" r> n> "\\end{" (s env) "}")
;;(center "\\begin{center}\n" r> n> "\\end{center}")
;;(displaymath "\\begin{displaymath}\n" r> n> "\\end{displaymath}")
;;(document "\\begin{document}\n" r> n> "\\end{document}")
;;(enumerate "\\begin{enumerate}\n\\item " r> n> "\\end{enumerate}")
;;(equation "\\begin{equation}" r> n> "\\end{equation}")
;;(flushleft "\\begin{flushleft}" r> n> "\\end{flushleft}")
;;(flushright "\\begin{flushright}" r> n> "\\end{flushright}")
;;(frac "\\frac{" p "}{" q "}")
;;(fussypar "\\begin{fussypar}" r> n> "\\end{fussypar}")
;;(itemize "\\begin{itemize}\n\\item " r> n> "\\end{itemize}")
;;(letter "\\begin{letter}\n" r> n> "\\end{letter}")
;;(math "\\begin{math}\n" r> n> "\\end{math}")
;;(minipage "\\begin{minipage}[t]{0.5\linewidth}\n" r> n> "\\end{minipage}")
;;(quotation "\\begin{quotation}\n" r> n> "\\end{quotation}")
;;(quote "\\begin{quote}\n" r> n> "\\end{quote}")
;;(sloppypar "\\begin{sloppypar}\n" r> n> "\\end{sloppypar}")
;;(theindex "\\begin{theindex}\n" r> n> "\\end{theindex}")
;;(trivlist "\\begin{trivlist}\n" r> n> "\\end{trivlist}")
;;(verbatim "\\begin{verbatim}\n" r> n> "\\end{verbatim}")
;;(verbatimm "\\begin{verbatim*}\n" r> n> "\\end{verbatim*}")

;;* texinfo-mode

;;(defmac "@defmac " p n> r> "@end defmac")
;;(defun "@defun " p n> r> "@end defun")
;;(defvar "@defvar " p n> r> "@end defvar")
;;(example "@example " p n> r> "@end example")
;;(lisp "@lisp " p n> r> "@end lisp")
;;(bullet "@itemize @bullet{}" n> r> "@end itemize")
;;(code "@code{" p "}")
;;(var "@var{" p "}")

;;* lisp-mode emacs-lisp-mode ;; Specify multiple modes

;; (lambda "(lambda (" p ")" n> r> ")")

;;* emacs-lisp-mode

;; (autoload ";;;###autoload")
;; (pt "(point)")
;; (var "(defvar " p "\n  \"" p "\")")
;; (local "(defvar-local " p "\n  \"" p "\")")
;; (const "(defconst " p "\n  \"" p "\")")
;; (custom "(defcustom " p "\n  \"" p "\"" n> ":type '" p ")")
;; (face "(defface " p " '((t :inherit " p "))\n  \"" p "\")")
;; (group "(defgroup " p " nil\n  \"" p "\"" n> ":group '" p n> ":prefix \"" p "-\")")
;; (macro "(defmacro " p " (" p ")\n  \"" p "\"" n> r> ")")
;; (alias "(defalias '" p " '" p ")")
;; (fun "(defun " p " (" p ")\n  \"" p "\"" n> r> ")")
;; (iflet "(if-let (" p ")" n> r> ")")
;; (whenlet "(when-let (" p ")" n> r> ")")
;; (whilelet "(while-let (" p ")" n> r> ")")
;; (andlet "(and-let* (" p ")" n> r> ")")
;; (cond "(cond" n "(" q "))" >)
;; (pcase "(pcase " (p "scrutinee") n "(" q "))" >)
;; (let "(let (" p ")" n> r> ")")
;; (lett "(let* (" p ")" n> r> ")")
;; (pcaselet "(pcase-let (" p ")" n> r> ")")
;; (pcaselett "(pcase-let* (" p ")" n> r> ")")
;; (rec "(letrec (" p ")" n> r> ")")
;; (dotimes "(dotimes (" p ")" n> r> ")")
;; (dolist "(dolist (" p ")" n> r> ")")
;; (loop "(cl-loop for " p " in " p " do" n> r> ")")
;; (command "(defun " p " (" p ")\n  \"" p "\"" n> "(interactive" p ")" n> r> ")")
;; (advice "(defun " (p "adv" name) " (&rest app)" n> p n> "(apply app))" n>
;;         "(advice-add #'" (p "fun") " " (p ":around") " #'" (s name) ")")
;; (header ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
;;         " -- " p " -*- lexical-binding: t -*-" n
;;         ";;; Commentary:" n ";;; Code:" n n)
;; (provide "(provide '" (file-name-base (or (buffer-file-name) (buffer-name))) ")" n
;;          ";;; " (file-name-nondirectory (or (buffer-file-name) (buffer-name)))
;;          " ends here" n)

;;* eshell-mode

;;(for "for " (p "i") " in " p " { " q " }")
;;(while "while { " p " } { " q " }")
;;(until "until { " p " } { " q " }")
;;(if "if { " p " } { " q " }")
;;(ife "if { " p " } { " p " } { " q " }")
;;(unl "unless { " p " } { " q " }")
;;(unle "unless { " p " } { " p " } { " q " }")

;;* text-mode

;; (box "┌─" (make-string (length str) ?─) "─┐" n
;;      "│ " (s str)                       " │" n
;;      "└─" (make-string (length str) ?─) "─┘" n)
;; (abox "+-" (make-string (length str) ?-) "-+" n
;;       "| " (s str)                       " |" n
;;       "+-" (make-string (length str) ?-) "-+" n)
;; (cut "--8<---------------cut here---------------start------------->8---" n r n
;;      "--8<---------------cut here---------------end--------------->8---" n)
;; (rot13 (p "plain text" text) n "----" n (rot13 text))
;; (calc (p "taylor(sin(x),x=0,3)" formula) n "----" n (format "%s" (calc-eval formula)))

;;* rst-mode

;;(title (make-string (length title) ?=) n (p "Title: " title) n (make-string (length title) ?=) n)

;;* java-mode

;;(class "public class " (p (file-name-base (or (buffer-file-name) (buffer-name)))) " {" n> r> n "}")

;; c-mode :when (re-search-backward "^\\S-*$" (line-beginning-position) 'noerror)

;;(inc "#include <" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) ">")
;;(incc "#include \"" (p (concat (file-name-base (or (buffer-file-name) (buffer-name))) ".h")) "\"")

;;* org-mode

;;(caption "#+caption: ")
;;(drawer ":" p ":" n r ":end:")
;; (begin "#+begin_" (s name) n> r> n "#+end_" name)
;; (quote "#+begin_quote" n> r> n "#+end_quote")
;; (sidenote "#+begin_sidenote" n> r> n "#+end_sidenote")
;; (marginnote "#+begin_marginnote" n> r> n "#+end_marginnote")
;; (example "#+begin_example" n> r> n "#+end_example")
;; (center "#+begin_center" n> r> n "#+end_center")
;; (ascii "#+begin_export ascii" n> r> n "#+end_export")
;; (html "#+begin_export html" n> r> n "#+end_export")
;; (latex "#+begin_export latex" n> r> n "#+end_export")
;; (comment "#+begin_comment" n> r> n "#+end_comment")
;; (verse "#+begin_verse" n> r> n "#+end_verse")
;; (src "#+begin_src " q n r n "#+end_src")
;; (gnuplot "#+begin_src gnuplot :var data=" (p "table") " :file " (p "plot.png") n r n "#+end_src" :post (org-edit-src-code))
;; (elisp "#+begin_src emacs-lisp" n r n "#+end_src" :post (org-edit-src-code))
;; (inlsrc "src_" p "{" q "}")
;;(title "#+title: " p n "#+author: Daniel Mendler" n "#+language: en")
