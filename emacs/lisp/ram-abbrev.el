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
    ("ws" "⃞")))

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
        ("ags" "arguments")
        ("app" "application")
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

        ("lg" "language")

        ("imp" "implementation")
        ("impt" "implement")

        ("inz" "initialize")
        ("inzd" "initialized")
        ("inzg" "initializing")
        ("inv" "invoke")
        ("invd" "invoked")
        ("invg" "invoking")
        ("invn" "invocation")

        ("mt" "manipulate")
        ("mtg" "manipulating")
        ("mec" "mechanism")

        ("nb" "number")
        ("nbs" "numbers")
        ("nm" "number")
        ("nms" "numbers")

        ("ob" "object")

        ("par" "parentheses")
        ("pg" "program")
        ("pgn" "programming")
        ("pr" "procedure")
        ("prs" "procedures")
        ("prd" "predicate")
        ("pri" "principle")
        ("prm" "parameter")
        ("prms" "parameters")
        ("prev" "previous")

        ("rc" "recursive")
        ("rcl" "recursively")
        ("rcn" "recursion")
        ("recv" "receiver")
        ("rep" "represent")
        ("repn" "representation")
        ("repg" "representing")
        ("rn" "return")
        ("rnd" "returned")
        ("rns" "returns")

        ("sm" "symbol")
        ("sms" "symbols")
        ("st" "structure")
        ("tq" "technique")
        ("und" "understand")))

(define-abbrev-table 'org-mode-abbrev-table
  (nconc racket-abbrevs org-abbrevs))
