;; -*- coding: utf-8; lexical-binding: t; -*-

;;* abbrev
;;** abbrev: settings

(setq abbrev-file-name "~/.emacs.d/lisp/abbrev-defs")

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

;; (setq save-abbrevs nil)

;;** abbrev: auto-correct

;;*** abbrev/auto-correct: auto-correct-abbrev-table

;; define auto-correct-abbrev-table if it is not defined
(when (not (boundp 'auto-correct-abbrev-table))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file abbrev-file-name))
  (when (not (boundp 'auto-correct-abbrev-table))
    (define-abbrev-table 'auto-correct-abbrev-table
      '(("fo" "of")))))

;;*** abbrev/auto-correct: functions

;; by Artur Malabara
;; https://endlessparentheses.com/ispell-and-abbrev-the-perfect-auto-correct.html

(defun endless/simple-get-word ()
  (car-safe (save-excursion (ispell-get-word nil))))

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word', then create an abbrev for it.
With prefix P, create local abbrev. Otherwise it will
be global.
If there's nothing wrong with the word at point, keep
looking for a typo until the beginning of buffer. You can
skip typos you don't want to fix with `SPC', and you can
abort completely with `C-g'."
  (interactive "P")
  (let (bef aft)
    (save-excursion
      (while (if (setq bef (endless/simple-get-word))
                 ;; Word was corrected or used quit.
                 (if (ispell-word nil 'quiet)
                     nil ; End the loop.
                   ;; Also end if we reach `bob'.
                   (not (bobp)))
               ;; If there's no word at point, keep looking
               ;; until `bob'.
               (not (bobp)))
        (backward-word)
        (backward-char))
      (setq aft (endless/simple-get-word)))
    (if (and aft bef (not (equal aft bef)))
        (let ((aft (downcase aft))
              (bef (downcase bef)))
          (define-abbrev
            (if p local-abbrev-table auto-correct-abbrev-table ;; global-abbrev-table
              )
            bef aft)
          (message "\"%s\" now expands to \"%s\" %sally"
                   bef aft (if p "loc" "glob")))
      (user-error "No typo at or before point"))))

;;*** abbrev/auto-correct: bindings

(define-key org-mode-map (kbd "C-x i") #'endless/ispell-word-then-abbrev)
(define-key text-mode-map (kbd "C-x i") #'endless/ispell-word-then-abbrev)
(with-eval-after-load 'fundamental-mode
  (define-key fundamental-mode-map (kbd "C-x i") #'endless/ispell-word-then-abbrev))

;;** abbrev: global-abbrev-table

;; credit to http://ergoemacs.org/emacs/emacs_abbrev_mode.html

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; net addrev
    ("1t" "first")
    ("2d" "second")
    ("3d" "third")
    ("ty" "thank you")
    ("ws" "□")))

;;** abbrev: clojure-mode-abbrev-table

(setq clojure-abbrevs
        '(
          ("df" "defn")
          ("dm" "defmacro")
          ("dt" "deftest")
          ("me" "macroexpand-1")
          ("pl" "println")
          ("re" "require")))

(with-eval-after-load "clojure"
  (when (boundp 'clojure-mode-abbrev-table)
    (clear-abbrev-table clojure-mode-abbrev-table))

  (when (boundp 'cider-repl-mode-abbrev-table)
    (clear-abbrev-table cider-repl-mode-abbrev-table))

  (define-abbrev-table 'clojure-mode-abbrev-table
    clojure-abbrevs)

  (define-abbrev-table 'cider-repl-mode-abbrev-table
    clojure-abbrevs))

;;** abbrev: emacs-lisp-mode-abbrev-table

(when (boundp 'emacs-lisp-mode-abbrev-table)
  (clear-abbrev-table emacs-lisp-mode-abbrev-table))

(setq emacs-lisp-abbrevs
      '(
        ("df" "defun")
        ("la" "lambda")
        ("lam" "lambda")))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  emacs-lisp-abbrevs)

;;** abbrev: racket-repl-mode-abbrev-table

(setq racket-abbrevs
      '(
        ;; ("be" "begin")
        ("usquare" "⃞")
        ("ca" "call-with-current-continuation")
        ("df" "define")
        ("di" "display")
        ("la" "lambda")
        ("lam" "lambda")
        ;; ("pr" "procedure?")
        ))

(with-eval-after-load 'racket-mode
  (when (boundp 'racket-repl-mode-abbrev-table)
    (clear-abbrev-table racket-repl-mode-abbrev-table))

  (when (boundp 'racket-mode-abbrev-table)
    (clear-abbrev-table racket-mode-abbrev-table))

  (define-abbrev-table 'racket-mode-abbrev-table
    racket-abbrevs)

  (define-abbrev-table 'racket-repl-mode-abbrev-table
    racket-abbrevs))

;;** abbrev: org-mode-abbrev-table

(with-eval-after-load "org"
  (when (boundp 'org-mode-abbrev-table)
    (clear-abbrev-table org-mode-abbrev-table))

  (setq org-abbrevs
        '(
          ("~c" "~cons~")
          ("aor" "area of responsibility")
          ("ab" "abstraction")
          ("ag" "argument")
          ("alg" "algorithm")
          ("algs" "algorithms")
          ("ags" "arguments")
          ("app" "application")
          ("appd" "applied")
          ("bc" "because")
          ("bf" "before")
          ("bh" "behavior")
          ("bn" "between")

          ("chr" "character")
          ("char" "character")
          ("chrs" "characters")
          ("chars" "characters")
          ("charz" "characterize")
          ("charzd" "characterized")
          ("charc" "characteristic")
          ("con" "continuation")
          ("cm" "computer")
          ("comn" "computation")
          ("comp" "compute")

          ("dec" "declare")
          ("decn" "declaration")
          ("def" "definition")
          ("df" "different")
          ("dfc" "difference")

          ("el" "element")
          ("els" "elements")
          ("env" "environment")
          ("ev" "evaluate")
          ("eva" "evaluate")
          ("evd" "evaluated")
          ("evg" "evaluating")
          ("evn" "evaluation")
          ("evt" "evaluation")
          ("exp" "expression")
          ("expd" "expressed")
          ("exps" "expressions")
          ("exl" "example")
          ("ext" "execute")
          ("exts" "executes")
          ("exn" "execution")

          ("fn" "function")
          ("fns" "functions")
          ("fnl" "functional")

          ("hier" "hierarchy")
          ("hiers" "hierarchies")

          ("imp" "implementation")
          ("impd" "implemented")
          ("impt" "implement")

          ("inf" "information")
          ("inz" "initialize")
          ("inzd" "initialized")
          ("inzg" "initializing")
          ("inv" "invoke")
          ("invd" "invoked")
          ("invg" "invoking")
          ("invn" "invocation")

          ("lc" "logical consequence")
          ("lg" "language")


          ("mt" "manipulate")
          ("mtg" "manipulating")
          ("mec" "mechanism")

          ("nb" "number")
          ("nbs" "numbers")
          ("nm" "number")
          ("nms" "numbers")

          ("ob" "object")
          ("occ" "occurrence")
          ("occs" "occurrences")

          ("par" "parentheses")
          ("pg" "program")
          ("pgn" "programming")
        
          ("pr" "procedure")
          ("prs" "procedures")
          ("prop" "property")
          ("props" "properties")
          ("prd" "predicate")
          ("pri" "principle")
          ("prm" "parameter")
          ("prms" "parameters")
          ("prec" "preceding")
          ("prev" "previous")

          ("qr" "query")
          ("qrs" "queries")

          ("rc" "recursive")
          ("rcl" "recursively")
          ("rcn" "recursion")
          ("recv" "receiver")

          ("rep" "represent")
          ("reps" "represents")
          ("repd" "represented")
          ("repn" "representation")
          ("repg" "representing")
          ("resp" "responsibility")
          ("resps" "responsibilities")

          ("rl" "relation")
          ("rlp" "relationship")
          ("rn" "return")
          ("rnd" "returned")
          ("rng" "returning")
          ("rns" "returns")

          ("sm" "symbol")
          ("sms" "symbols")
          ("st" "structure")
          ("subs" "substitution")
          ("subss" "substitutions")
          ("subsg" "substituting")
          ("tq" "technique")

          ("todo" "TODO")
          ("und" "understand")

          ("val" "value")
          ("vals" "values")
          ("vl" "value")
          ("vls" "values")
          ("var" "variable")
          ("vars" "variables")

          ;; Greek

          ("theta" "Θ")))

  ;; gives error "circular list ..."
  ;; (define-abbrev-table 'org-mode-abbrev-table
  ;;   (nconc racket-abbrevs org-abbrevs))
  (define-abbrev-table 'org-mode-abbrev-table
    org-abbrevs)

  (abbrev-table-put org-mode-abbrev-table
                    :parents (list auto-correct-abbrev-table)))
