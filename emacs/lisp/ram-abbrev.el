;; -*- coding: utf-8; lexical-binding: t; -*-

;; credit to http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;;* global
(set-default 'abbrev-mode t)

(setq save-abbrevs nil)

(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; net addrev
    ("1t" "first")
    ("2t" "second")
    ("3t" "third")
    ("ty" "thank you")
    ("ws" "□")))

;;* clojure
(when (boundp 'clojure-mode-abbrev-table)
  (clear-abbrev-table clojure-mode-abbrev-table))

(when (boundp 'cider-repl-mode-abbrev-table)
  (clear-abbrev-table cider-repl-mode-abbrev-table))

(setq clojure-abbrevs
      '(
        ("df" "defn")
        ("dm" "defmacro")
        ("dt" "deftest")
        ("me" "macroexpand-1")
        ("pl" "println")
        ("re" "require")))

(define-abbrev-table 'clojure-mode-abbrev-table
  clojure-abbrevs)

(define-abbrev-table 'cider-repl-mode-abbrev-table
  clojure-abbrevs)

;;* emacs-lisp
(when (boundp 'emacs-lisp-mode-abbrev-table)
  (clear-abbrev-table emacs-lisp-mode-abbrev-table))

(setq emacs-lisp-abbrevs
      '(
        ("df" "defun")
        ("la" "lambda")
        ("lam" "lambda")))

(define-abbrev-table 'emacs-lisp-mode-abbrev-table
  emacs-lisp-abbrevs)

;;* racket
(when (boundp 'racket-repl-mode-abbrev-table)
  (clear-abbrev-table racket-repl-mode-abbrev-table))

(when (boundp 'racket-mode-abbrev-table)
  (clear-abbrev-table racket-mode-abbrev-table))

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

(define-abbrev-table 'racket-mode-abbrev-table
  racket-abbrevs)

(define-abbrev-table 'racket-repl-mode-abbrev-table
  racket-abbrevs)

;;* org

(when (boundp 'org-mode-abbrev-table)
  (clear-abbrev-table org-mode-abbrev-table))

(setq org-abbrevs
      '(
        ("~c" "~cons~")
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
        ("exps" "expressions")
        ("exl" "example")
        ("ext" "execute")
        ("exts" "executes")
        ("exn" "execution")

        ("fn" "function")
        ("fns" "functions")
        ("fnl" "functional")

        ("lg" "language")

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
