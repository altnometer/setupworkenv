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
        ("be" "begin")
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
        ("bc" "because")
        ("con" "continuation")
        ("com" "computation")
        ("eva" "evaluate")
        ("evn" "evaluation")
        ("evt" "evaluation")
        ("exp" "expression")
        ("pr" "procedure")
        ("und" "understand")))

(define-abbrev-table 'org-mode-abbrev-table
  (nconc racket-abbrevs org-abbrevs))
