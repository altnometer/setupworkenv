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
    ("br" "breadcrumbs")))

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
        ("pr" "procedure?")))

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
        ("app" "application")
        ("bc" "because")
        ("bf" "before")
        ("bh" "behavior")
        ("bn" "between")
        ("con" "continuation")
        ("cm" "computer")
        ("com" "computation")
        ("df" "different")
        ("ev" "evaluate")
        ("eva" "evaluate")
        ("evn" "evaluation")
        ("evt" "evaluation")
        ("exp" "expression")
        ("exl" "example")
        ("lg" "language")
        ("mec" "mechanism")
        ("nm" "number")
        ("par" "parentheses")
        ("pg" "program")
        ("pgg" "programming")
        ("pr" "procedure")
        ("pri" "principle")
        ("prm" "problem")
        ("rn" "return")
        ("sm" "symbol")
        ("st" "structure")
        ("tq" "technique")
        ("und" "understand")))

(define-abbrev-table 'org-mode-abbrev-table
  (nconc racket-abbrevs org-abbrevs))
