;; packages.el --- Package configuration -*- lexical-binding: t -*-

(set-language-environment "UTF-8")
;; (set-language-environment "Latin-1")

(defconst large-sreen-width 3840
  "`large-sreen-width' value indicates when to use settings for large screens")

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

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "C-x i") #'endless/ispell-word-then-abbrev))
(define-key text-mode-map (kbd "C-x i") #'endless/ispell-word-then-abbrev)
(with-eval-after-load 'fundamental-mode
  (define-key fundamental-mode-map (kbd "C-x i") #'endless/ispell-word-then-abbrev))

;;** abbrev: global-abbrev-table

;; credit to http://ergoemacs.org/emacs/emacs_abbrev_mode.html

;; (clear-abbrev-table global-abbrev-table)

(when (boundp 'global-table)
  (clear-abbrev-table global-table))

(define-abbrev-table 'global-abbrev-table
  '(
    ;; net addrev
    ("1t" "first")
    ("2d" "second")
    ("3d" "third")
    ("ty" "thank you")
    ("ws" "□")
    ;; Pāli
    ("AA" "Ā")
    ("aa" "ā")
    ("II" "Ī")
    ("ii" "ī")
    ("UU" "Ū")
    ("uu" "ū")
    ;; the rest do not work as the symbols used are not word parts
    ;; ("\"N" "Ṅ")
    ;; ("\"n" "ṅ")
    ;; (".M" "Ṃ")
    ;; (".m" "ṃ")
    ;; ("~N" "Ñ")
    ;; ("~n" "ñ")
    ;; (".T" "Ṭ")
    ;; (".t" "ṭ")
    ;; (".D" "Ḍ")
    ;; (".d" "ḍ")
    ;; (".N" "Ṇ")
    ;; (".n" "ṇ")
    ;; (".L" "Ḷ")
    ;; (".l" "ḷ")
    ))

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

          ("bl" "buffer-local")

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
          ("cm" "computer")
          ("cat" "category")
          ("cats" "categories")

          ("comn" "computation")
          ("comp" "compute")
          ("comd" "command")
          ("compl" "completion")
          ("complg" "completing")

          ("comds" "commands")

          ("con" "continuation")
          ("cont" "contain")
          ("contg" "containing")
          ("conts" "contains")

          ("conf" "configure")
          ("confd" "configured")
          ("confn" "configuration")

          ("dec" "declare")
          ("decn" "declaration")
          ("def" "definition")
          ("df" "different")
          ("dfc" "difference")
          ("dfl" "default")

          ("el" "element")
          ("els" "elements")
          ("env" "environment")
          ("envs" "environments")
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
          ("hl" "headline")
          ("hls" "headlines")

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

          ("kw" "keyword")
          ("kws" "keywords")

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
          ("obs" "objects")
          ("occ" "occurrence")
          ("occs" "occurrences")

          ("oa" "org-agenda")

          ("par" "parentheses")
          ("pg" "program")
          ("pgn" "programming")

          ("par" "paragraph")
          ("pars" "paragraphs")
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

          ("spc" "specific")
          ("spd" "specified")
          ("spl" "special")
          ("sps" "specifies")
          ("spy" "specify")
          ("spec" "specification")
          ("specs" "specifications")

          ("sm" "symbol")
          ("sms" "symbols")
          ("st" "structure")
          ("subs" "substitution")
          ("subss" "substitutions")
          ("subsg" "substituting")
          ("syn" "syntax")
          ("sync" "syntactical")

          ("tq" "technique")
          ("todo" "TODO")

          ("und" "understand")

          ("val" "value")
          ("vals" "values")
          ("vl" "value")
          ("vls" "values")
          ("var" "variable")
          ("vars" "variables")

          ("wo" "without")

          ;; Greek

          ("theta" "Θ")))

  ;; gives error "circular list ..."
  ;; (define-abbrev-table 'org-mode-abbrev-table
  ;;   (nconc racket-abbrevs org-abbrevs))
  (define-abbrev-table 'org-mode-abbrev-table
    org-abbrevs)

  (abbrev-table-put org-mode-abbrev-table
                    :parents (list auto-correct-abbrev-table)))

;;** abbrev: text-mode-abbrev-table

(abbrev-table-put text-mode-abbrev-table
                  :parents (list auto-correct-abbrev-table))

;;* Leader Key
;;** Definition
;; leader tap
(define-key global-map (kbd "<SunProps>") nil)
;; leader hold
(define-key global-map (kbd "<cancel>") nil)
;; shift leader tap
(define-key global-map (kbd "<help>") nil)
;; shift leader hold
(define-key global-map (kbd "<find>") nil)

(defvar ram-leader-map-tap-global (make-sparse-keymap)
  "Keymap for \"leader tap key\" shortcuts in `global-map'.")

(defvar ram-leader-map-tap-prog (make-sparse-keymap)
  "Keymap for \"leader tap key\" shortcuts in `prog-mode-map'.")

(defvar ram-leader-map-hold (make-sparse-keymap)
  "Keymap for \"leader hold key\" shortcuts.")

(define-key global-map (kbd "<SunProps>") ram-leader-map-tap-global)

(define-key global-map (kbd "<cancel>") ram-leader-map-hold)
(define-key prog-mode-map (kbd "<SunProps>") ram-leader-map-tap-prog)

(with-eval-after-load "org"
  (defvar ram-leader-map-tap-org (make-sparse-keymap)
    "Keymap for \"leader tap key\" shortcuts in `org-mode-map'.")
  (define-key org-mode-map (kbd "<SunProps>") ram-leader-map-tap-org))

;;* bindings

;;** bindings: buffer

(defun ram-switch-to-buffer (n)
  "Invoke builtin `switch-to-buffer' or `switch-to-buffer-other-window' depending on N.
Disable `icomplete-vertical-mode' for this command."
  (interactive "p")
  (let ((default (default-value 'icomplete-vertical-mode)))
    (icomplete-vertical-mode 1)
    (condition-case err
        (if (= n 1)
            (call-interactively #'switch-to-buffer)
          (call-interactively #'switch-to-buffer-other-window))
      (error
       (icomplete-vertical-mode default)
       (signal (car err) (cdr err)))
      (quit
       (icomplete-vertical-mode default)
       (signal 'quit nil)))
    (icomplete-vertical-mode default)))

(define-key global-map (kbd "s-b") #'ram-switch-to-buffer)
(define-key global-map (kbd "C-s-b") 'display-buffer)
(define-key global-map (kbd "<M-f3>") #'ibuffer)

;;** bindings: file

(define-key global-map (kbd "C-s-f") #'find-file)
(define-key global-map (kbd "C-S-s-f") #'find-file-other-window)

(define-key ram-leader-map-tap-global (kbd "n") 'comint-dynamic-complete-filename)

(define-key ram-leader-map-tap-global (kbd "i") 'completion-at-point)
(define-key ram-leader-map-tap-global (kbd "w") #'widen)

(define-key global-map (kbd "C-x D") 'dired-other-window)

(define-key global-map (kbd "C-h C-k") #'describe-keymap)

;;** bindings: general

;; default "M-a" #'backward-sentence, only with #'push-mark
(defun ram-back-sentence (&optional arg)
  "Call `backward-sentence' after `push-mark'."
  (interactive "^p")
  (backward-sentence arg))

(define-key global-map (kbd "M-a") #'ram-back-sentence)
;; "M-(" was originally bound to #'insert-parentheses
(global-unset-key (kbd "M-("))
(define-key global-map (kbd "C-%") #'repeat)
(define-key global-map (kbd "C-h a") 'apropos)

;;* company

(straight-use-package
 '(company :type git :flavor melpa :host github :repo "company-mode/company-mode"))

;; (require 'company)

;; (add-hook 'after-init-hook 'global-company-mode)
(setq company-minimum-prefix-length 0)
;; upper case, lower case ignore when searching.
(setq completion-ignore-case nil)
(setq company-dabbrev-ignore-case nil)
;; do not downcase (lower case) the candidates (if upper case exist)
(setq company-dabbrev-downcase nil)
(setq company-transformers '(company-sort-by-backend-importance))

(setq company-idle-delay nil)

(setq company-search-regexp-function
      (lambda (input-str)
        "search words separated by white-space in any order."
        (let ((search-words (split-string input-str)))
          (cl-labels ((combinations (&rest lists)
                        (if (endp lists)
                            (list nil)
                          (mapcan (lambda (inner-val)
                                    (seq-remove #'null (mapcar (lambda (outer-val)
                                                                 (if (not (member outer-val inner-val))
                                                                     (cons (regexp-quote outer-val) inner-val)))
                                                               (car lists))))
                                  (apply #'combinations (cdr lists)))))
                      (clone-lists (l len)
                        (if (<= len 1)
                            (list l)
                          (cons l (clone-lists l (1- len))))))
            (string-join (mapcar (lambda (m)
                                   (concat "\\(" (string-join (mapcar (lambda (i) (concat "\\(" i "\\)")) m) ".*?") "\\)"))
                                 (apply #'combinations (clone-lists search-words (length search-words) )))
                         "\\|")))))

;; (setq company-search-regexp-function #'regexp-quote)

;; (define-key global-map (kbd "s-c") 'company-complete)
;; (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
;; originally bound to #'dabbrev-completion
(global-set-key (kbd "C-M-/") #'company-complete)

(setq company-backends
      '(
        (company-capf company-dabbrev)
        (
         company-files
         company-dabbrev
         company-gtags
         company-etags
         company-keywords
         company-capf
         company-yasnippet)
        (company-abbrev company-dabbrev)))

(add-hook 'emacs-lisp-mode-hook 'company-mode)
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-capf company-elisp company-dabbrev-code company-files)))))

(add-hook 'lisp-interaction-mode-hook 'company-mode)
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-elisp company-dabbrev-code company-files)))))

(add-hook 'clojure-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (list '(company-capf)))))
(add-hook 'clojure-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (list '(company-capf)))))

;; (add-hook 'emacs-lisp-mode-hook
;;         (lambda ()
;;           (set (make-local-variable 'company-backends)
;;                (list
;;                 (cons '(company-capf :with company-dabbrev)
;;                       (car company-backends))))))

;; ;; if you want to append to the end of the list.
;; (append (car company-backends)
;;         (list 'company-elisp))

;; (add-hook 'cider-repl-mode-hook 'company-mode)
;; (add-hook 'cider-mode-hook 'company-mode)


;; (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
;; (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)



;;* emacs-lisp elisp

;; (add-hook 'emacs-lisp-mode-hook #'(lambda () (outline-hide-sublevels 1)))
(add-hook 'emacs-lisp-mode-hook #'ram-remap-hl-line-face-in-find-file-hook)
(define-key emacs-lisp-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)

;; display eval result inline, use cider for that
;; credit to https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html
(autoload 'cider--make-result-overlay "cider-overlays")

(defun endless/eval-overlay (value point)
  (cider--make-result-overlay (format "%S" value)
    :where point
    :duration 'command)
  ;; Preserve the return value.
  value)

(advice-add 'eval-region :around
            (lambda (f beg end &rest r)
              (endless/eval-overlay
               (apply f beg end r)
               end)))

(advice-add 'eval-last-sexp :filter-return
            (lambda (r)
              (endless/eval-overlay r (point))))

;; !!! for some reason, if advice is set without delay (see the line after),
;; it has no effect when emacs starts up.
(defun set-displaying-eval-defun-result-inline ()
  "Display `eval-defun' results inline."
  (advice-add 'eval-defun :filter-return
              (lambda (r)
                (endless/eval-overlay
                 r
                 (save-excursion
                   (end-of-defun)
                   (point))))))

(run-with-idle-timer 1 nil #'set-displaying-eval-defun-result-inline)

(define-key emacs-lisp-mode-map (kbd "<S-return>") 'newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)

;;* shell

;; shell settings are based on
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

;;* eshell

;;** eshell: completion

;; (add-hook 'shell-mode-hook #'company-mode)
;; (add-hook 'eshell-mode-hook
;;           (lambda () (define-key eshell-mode-map (kbd "<C-tab>") #'company-manual-begin)))

(add-hook 'eshell-mode-hook
          (lambda () (define-key eshell-mode-map (kbd "<tab>") #'completion-at-point)))

;;** eshell: bindings

(defun ram-open-eshell (arg)
  "Switch to eshell buffer or open a new one."
  (interactive "p")
  (let ((eshell-buffer
         (nth (max (1- arg) 0)
              (sort
               (seq-filter (lambda (b) (string-prefix-p "*eshell*" (buffer-name b))) (buffer-list))
               (lambda (s1 s2) (string-lessp (buffer-name s1) (buffer-name s2)))))))
    (if eshell-buffer
        (switch-to-buffer eshell-buffer)
      ;; (pop-to-buffer-same-window eshell-buffer)
      (eshell arg))))

(define-key global-map (kbd "s-e") #'ram-open-eshell)

;;** eshell: history

(add-hook 'eshell-pre-command-hook #'eshell-save-some-history)
(setq eshell-history-size 10000)
(setq eshell-hist-ignoredups t)
(setq eshell-input-filter
      (lambda (input)
        (not (or (string-prefix-p "[[:space:]]+" input)
                 (< (seq-length input) 2)))))

;;*** eshell/history: ram-eshell-completion-mode

(autoload 'ram-eshell-completion-mode "ram-eshell-completion")

(with-eval-after-load "esh-mode"
  (add-hook 'eshell-mode-hook 'ram-eshell-completion-mode))

(autoload 'rec-toggle-mode "ram-eshell-completion")

(with-eval-after-load "esh-mode"
  (define-key eshell-mode-map (kbd "S-SPC") #'rec-toggle-mode)
  (define-key eshell-mode-map (kbd "<return>") (lambda () (interactive)
                                                 (eshell-send-input)
                                                 (rec-toggle-mode))))



;;** eshell: settings

;; (add-to-list eshell-visual-commands "top")
(setq eshell-visual-subcommands '("git" "log" "clone" "diff" "show"))
(setq eshell-visual-options '("git" "--help" "--paginate"))

(setq eshell-visual-commands '())

(setq eshell-scroll-to-bottom-on-input nil)
(setq eshell-scroll-to-bottom-on-output nil)
;; the previous settings do not seem to work, hence the following two lines:
(remove-hook 'eshell-output-filter-functions 'eshell-postoutput-scroll-to-bottom)
(remove-hook 'eshell-output-filter-functions 'eshell-preinput-scroll-to-bottom)

(add-to-list 'eshell-output-filter-functions #'eshell-truncate-buffer)
(setq eshell-buffer-maximum-lines 1000)

;;* vterm

(straight-use-package
 '(vterm :type git :flavor melpa :files
         ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
         :host github :repo "akermu/emacs-libvterm"))

(require 'vterm)

;;** vterm: bindings

(defun ram-open-vterm (arg)
  "Switch to vterm buffer or open a new one."
  (interactive "p")
  (let ((vterm-buffer
         (nth (max (1- arg) 0)
              (sort
               (seq-filter (lambda (b) (string-prefix-p "*vterm*" (buffer-name b))) (buffer-list))
               (lambda (s1 s2) (string-lessp (buffer-name s1) (buffer-name s2)))))))
    (if vterm-buffer
        (switch-to-buffer vterm-buffer)
      ;; (pop-to-buffer-same-window vterm-buffer)
      (vterm arg))))

(define-key global-map (kbd "s-v") #'ram-open-vterm)

(define-key vterm-mode-map (kbd "M-<f9>") #'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "M-<f9>") #'vterm-copy-mode-done)

;;* exwm

(straight-use-package
 '(exwm :type git :host github :repo "emacs-straight/exwm"))

(require 'exwm)
(require 'exwm-randr)

;;** exwm: exwm-randr

(setq exwm-randr-workspace-output-plist '(0 "HDMI-1-2"
                                          1 "HDMI-1-1"
                                          2 "HDMI-1-1"
                                          3 "HDMI-1-1"
                                          4 "HDMI-1-1"
                                          5 "HDMI-1-1"
                                          6 "HDMI-1-2"
                                          7 "HDMI-1-2"
                                          8 "HDMI-1-2"
                                          9 "HDMI-1-2"))

;; (start-process-shell-command
;;              "xrandr" nil
;;              "xrandr --output HDMI-1 --mode 3840x2160 --pos 0x0 --rotate normal")

;; (add-hook 'exwm-randr-screen-change-hook
;;           (lambda ()
;;             (start-process-shell-command
;;              "xrandr" nil
;;              "xrandr --output HDMI-1-1 --primary --mode 1920x2160 --pos 0x0 --rotate normal && xrandr --output HDMI-1-2 --mode 1920x2160 --pos 1920x0 --rotate normal")))

;;** exwm: settings

;; echo area stopped appearing with this option
;; (setq exwm-workspace-minibuffer-position 'bottom)
;; (setq exwm-workspace-display-echo-area-timeout 5)

(setq exwm-workspace-number 10)

;; Make class name the buffer name
(add-hook 'exwm-update-class-hook
          (lambda ()
            (exwm-workspace-rename-buffer exwm-class-name)))

;; floating X windows frames differ from emacs frames
(setq exwm-manage-force-tiling t)

;;** exwm: bindings

;; Global keybindings.
(setq exwm-input-global-keys
      `(
        (,(kbd "s-a") . org-agenda)

        (,(kbd "s-c n") . org-roam-dailies-capture-today)
        (,(kbd "s-c d") . org-roam-dailies-goto-today)
        (,(kbd "s-c f") . org-roam-dailies-goto-next-note)
        (,(kbd "s-c b") . org-roam-dailies-goto-previous-note)
        (,(kbd "s-c c") . org-roam-dailies-goto-date)
        (,(kbd "s-c v") . org-roam-dailies-capture-date)
        (,(kbd "s-c t") . org-roam-dailies-goto-tomorrow)
        (,(kbd "s-c y") . org-roam-dailies-goto-yesterday)

        (,(kbd "C-g") . keyboard-quit)
        ;; (,(kbd "C-h") . help-command)
        ([?\s-q] . kill-buffer-and-window)
        ;; ([?\s-Q] . save-buffers-kill-emacs)

        ([?\s-n] . switch-to-next-buffer)
        ([?\s-p] . switch-to-prev-buffer)
        ([?\s-f] . ram-choose-from-recentf)
        ([?\s-b] . ram-switch-to-buffer)
        ([?\s-B] . switch-to-buffer-other-window)
        ([?\s-e] . ram-open-eshell)
        ([?\s-o] . other-window)
        (,(kbd "C-s-f") . find-file)
        (,(kbd "C-S-s-f") . find-file-other-window)

        ([?\s-N] . ram-next-workspace)
        ([?\s-P] . ram-previous-workspace)
        ([?\s-O] . ram-other-workspace)
        (,(kbd "<M-f15>") . ram-other-workspace)
        (,(kbd "<f15>") . ram-other-workspace)

        ([?\s-t] . org-roam-node-find)

        ;; ([?\s-z] . exwm-layout-toggle-fullscreen)
        ;; ([?\s-/] . exwm-layout-toggle-mode-line)
        ;; ([?\s-y] . exwm-workspace-toggle-buffer)
        ;; ([?\s-h] . exwm-window-*focus left*)
        ;; ([?\s-j] . exwm-window-*focus down*)
        ;; ([?\s-H] . exwm-window-*move left*)
        ;; ([?\s--] . exwm-window-*resize*)

        ;; 's-.': Reset (to line-mode).
        ([?\s-.] . exwm-reset)
        ;; 's-w': Switch workspace.
        ;; ([?\s-w] . exwm-workspace-switch)
        ;; 's-&': Launch application.
        ([?\s-'] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; (,(kbd "<f2>") . (lambda () (interactive) (exwm-workspace-switch-create 2)))
        ;; (,(kbd "<f10>") . (lambda () (interactive) (exwm-workspace-switch-create 0)))
        ;; (,(kbd "<f6>") . (lambda () (interactive) (exwm-workspace-switch-create 6)))
        ;; (,(kbd "<f7>") . (lambda () (interactive) (exwm-workspace-switch-create 7)))
        ;; 's-N': Switch to certain workspace.
        (\,@ (mapcar (lambda (i)
                        `(,(kbd (format "s-%d" i)) .
                          (lambda ()
                            (interactive)
                            (exwm-workspace-switch-create ,i))))
                      (number-sequence 0 9)))))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(add-hook 'exwm-manage-finish-hook
          (lambda ()
            (cond
             ((and exwm-class-name
                   (string= exwm-class-name "qutebrowser"))
              (exwm-workspace-switch 1)
              (exwm-input-set-local-simulation-keys nil)
              (set-window-fringes (selected-window) 0 0)
              ;; git-gutter adds right fringe
              (git-gutter-mode -1)
              (exwm-layout-toggle-fullscreen)))))
              ;; (setq mode-line-format nil)


(setq exwm-manage-configurations
      '(((member exwm-instance-name '("qutebrowser"))
         workspace 1)))

;; Line-editing shortcuts

;; (setq exwm-input-simulation-keys
;;       '(([?\C-b] . [left])
;;         ([?\C-f] . [right])
;;         ([?\C-p] . [up])
;;         ([?\C-n] . [down])
;;         ([?\C-a] . [home])
;;         ([?\C-e] . [end])
;;         ([?\M-v] . [prior])
;;         ([?\C-v] . [next])
;;         ([?\C-d] . [delete])
;;         ([?\C-k] . [S-end delete])))

;; must be the last in 'exwm settings
(exwm-randr-enable)
(exwm-enable)

;;*** exwm: navigate workspaces

(defvar ram-next-workspaces nil
  "Hold workspace indexes added after `ram-previous-workspace' command.")

(defun ram-next-workspace ()
  "Switch to next workspace."
  (interactive)
  (if ram-next-workspaces
      (let((next-idx (car ram-next-workspaces)))
        (setq ram-next-workspaces (cdr ram-next-workspaces))
        (exwm-workspace-switch next-idx))
    (user-error "No next workspace")))

(defvar ram-prev-workspaces nil
  "Hold visited workspace added after `exwm-workspace-switch' is invoked.")

(defun ram--add-workspace-index (frame-or-index &optional force)
  "Add current workspace index to `ram-prev-workspaces'."
  (let ((idx (exwm-workspace--position exwm-workspace--current)))
    (when idx
      (setq ram-prev-workspaces (cons idx (delq idx ram-prev-workspaces))))))

(defun ram-previous-workspace ()
  "Switch to previous workspace."
  (interactive)
  (if ram-prev-workspaces
      (let ((current-idx (exwm-workspace--position exwm-workspace--current))
            (prev-idx (car ram-prev-workspaces)))
        (setq ram-next-workspaces (cons current-idx (delq current-idx ram-next-workspaces)))
        (setq ram-prev-workspaces (cdr ram-prev-workspaces))
        (exwm-workspace-switch prev-idx)
        ;; advice on #'exwm-workspace-switch adds index to ram-prev-workspaces,
        ;; remove it
        (setq ram-prev-workspaces (cdr ram-prev-workspaces)))
    (user-error "No previous workspace")))

(advice-add #'exwm-workspace-switch :before #'ram--add-workspace-index)

(defun ram-other-workspace (count)
  "Focus next visible workspace."
  (interactive "p")
  (let ((count (1- count))
        (visible-workspace
         (seq-filter (lambda (f) (and (not (equal f (selected-frame)))
                                      (frame-parameter f 'exwm-active)))
                     (frame-list))))
    (exwm-workspace-switch (nth count visible-workspace))))

;; all <M-f*> keys in layer activated with <tab> hold (right top thumb key)
(define-key global-map (kbd "<M-f15>") #'ram-other-workspace)
;; <f1> is bound to #'help-for-help by default
(define-key global-map (kbd "<f1>") nil)
;; <f2> is a prefix to some '2C-mode commands
(define-key global-map (kbd "<f2>") nil)
;; (define-key global-map (kbd "<f10>") (lambda () (interactive (exwm-workspace-switch-create 0))))
;; (define-key global-map (kbd "<f6>") (lambda () (interactive (exwm-workspace-switch-create 6))))
;; (define-key global-map (kbd "<f7>") (lambda () (interactive (exwm-workspace-switch-create 7))))
(define-key global-map (kbd "<M-f15>") #'ram-other-workspace)
(define-key global-map (kbd "s-O") #'ram-other-workspace)
(define-key global-map (kbd "s-N") #'ram-next-workspace)
(define-key global-map (kbd "s-P") #'ram-previous-workspace)
(define-key global-map (kbd "<f15>") #'ram-other-workspace)

;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))

;;* magit

(straight-use-package
 '(magit-section :type git
                 :files ("lisp/magit-section.el"
                         "Documentation/magit-section.texi")
                 :host github
                 :repo "magit/magit"))

(straight-use-package
 '(magit :type git :flavor melpa :files ("lisp/magit"
                                         "lisp/magit*.el"
                                         "lisp/git-rebase.el"
                                         "Documentation/magit.texi"
                                         "Documentation/AUTHORS.md"
                                         "LICENSE"
                                         (:exclude "lisp/magit-libgit.el")
                                         "magit-pkg.el")
         :host github :repo "magit/magit"))

(setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
(setq magit-save-repository-buffers 'dontask)
(setq magit-diff-refine-hunk 'all)
(setq magit-copy-revision-abbreviated t)

(define-key global-map (kbd "s-m") 'magit-status)
(define-key global-map (kbd "s-M") 'magit-file-dispatch)
(define-key global-map (kbd "M-s-m") 'magit-dispatch)

;;* macros

;; credit to https://stackoverflow.com/a/14946682/9913235
(defmacro ram-eval-exp-with-modified-hooks (exp &rest hooks)
  "Set hooks with HOOK and run EXP.

HOOK is of the form: '((before-save-hook (my-fn1, my-fn2)) (after-save-hook '()))."
  `(let ((b (current-buffer)))          ; memorize the buffer
     (with-temp-buffer ; new temp buffer to bind the global value of hooks
       (let ,@hooks
         (with-current-buffer b ; go back to the current buffer, hooks are now buffer-local
           (let ,@hooks
             ,exp))))))

;; credit to https://stackoverflow.com/a/14946682/9913235
(defmacro ram-with-hooks-reset-in-buffer (hooks buffer &rest body)
  "Reset global and local hooks, eval BODY in BUFFER.

HOOK is of the form: '((before-save-hook (remove my-fn1 before-save-hook)) (after-save-hook '()) ...)."
  `(with-temp-buffer ; new temp buffer to bind the global value of hooks
     (let ,hooks
       (with-current-buffer ,buffer ; go back to the current buffer, hooks are now buffer-local
         (let ,hooks
           ,@body)))))

;;* minibuffer

;;** minibuffer: settings

(setq completion-category-defaults nil)
(setq completion-cycle-threshold 3)
(setq completion-flex-nospace nil)
(setq completion-pcm-complete-word-inserts-delimiters t)
(setq completion-pcm-word-delimiters "-_./:| ")
(setq completion-show-help nil)
(setq completion-ignore-case t)
(setq minibuffer-eldef-shorten-default 1)
(setq minibuffer-default-prompt-format " [%s]")
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)
(setq completions-format 'vertical)
(setq enable-recursive-minibuffers t)
(setq read-answer-short t)
(setq resize-mini-windows 'grow-only)

(file-name-shadow-mode 1)
(minibuffer-depth-indicate-mode 1)
;; (minibuffer-electric-default-mode 1)

;; (with-eval-after-load "minibuf-eldef"
;;   (setq-default minibuffer-default-in-prompt-regexps
;;                 '(("\\( (default .*)\\): *" 1)
;;                   ("\\( \\[.*\\]\\):? *\\'" 1))))

(with-eval-after-load "minibuf-eldef"
  (setq-default minibuffer-default-in-prompt-regexps
                '(("\\( \\[.*]\\): *" 1)
                  ("\\( (default\\(?: is\\)? \\(.*\\))\\):? \\'" 1 " [\\2]")
                  ("([^(]+?\\(, default\\(?: is\\)? \\(.*\\)\\)):? \\'" 1)
                  ("\\( \\[.*\\]\\):? *\\'" 1))))

;;** minibuffer/settings: pre-minibuffer-buffer

;; if you need a last active buffer before you entered the minibuffer see
;; https://emacs.stackexchange.com/a/48877/31822

;; for my needs, #'with-minibuffer-selected-window seem to work for now.


;;** minibuffer: actions

(defun ram-kill-ring-save-minibuffer-candidate ()
  "Save completion candidate into the `kill-ring'."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (kill-new candidate)
      (message "Copied %s to kill-ring" (propertize candidate 'face 'success)))))

(defun ram-kill-minibuffer-candidate ()
  "Save completion candidate into the `kill-ring' and exit minibuffer."
  (interactive)
  (let ((candidate (car completion-all-sorted-completions)))
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (kill-new candidate)
      (top-level))))

(defun ram-insert-minibuffer-candidate (arg)
  "Insert completion candidate."
  (interactive "p")
  (let ((candidate (car completion-all-sorted-completions)))
    (when (and (minibufferp)
               (bound-and-true-p icomplete-mode))
      (with-minibuffer-selected-window
        (insert candidate)
        ;; exit minibuffer when no universal or digital arg (other than default 1)
        (when (= 1 arg)
          (top-level))))))

(defun ram-describe-function-from-minibuffer ()
  "Call `ram-describe-function' and use previous minibuffer input."
  (interactive)
   (let ((user-input  (buffer-substring (point-at-bol) (point-at-eol))))
    (minibuffer-with-setup-hook
        (lambda () (insert user-input))
      (condition-case err
          (call-interactively #'ram-describe-function)
        (quit (abort-recursive-edit))
        (:success (abort-recursive-edit))))))

(defun ram-describe-variable-from-minibuffer ()
  "Call `ram-describe-variable' and use previous minibuffer input."
  (interactive)
  (let ((user-input  (buffer-substring (point-at-bol) (point-at-eol))))
    (minibuffer-with-setup-hook
        (lambda () (insert user-input))
      (condition-case err
          (call-interactively #'ram-describe-variable)
        (quit (abort-recursive-edit))
        (:success (abort-recursive-edit))))))

(defun ram-describe-symbol-from-minibuffer ()
  "Call `describe-symbol' and use previous minibuffer input."
  (interactive)
  (let ((user-input  (buffer-substring (point-at-bol) (point-at-eol))))
    (minibuffer-with-setup-hook
        (lambda () (insert user-input))
      (condition-case err
          (call-interactively #'describe-symbol)
        (quit (abort-recursive-edit))
        (:success (abort-recursive-edit))))))

(defun ram-jump-to-outline-from-minibuffer ()
  "Call `ram-jump-to-outline' and use previous minibuffer input."
  (interactive)
  (let ((user-input  (buffer-substring (point-at-bol) (point-at-eol))))
    (minibuffer-with-setup-hook
        (lambda () (insert user-input))
      (condition-case err
          (call-interactively #'ram-jump-to-outline)
        (quit (abort-recursive-edit))
        (:success (abort-recursive-edit))))))

(defun ram-jump-to-def-from-minibuffer ()
  "Call `ram-jump-to-def' and use previous minibuffer input."
  (interactive)
  (let ((user-input  (buffer-substring (point-at-bol) (point-at-eol))))
    (minibuffer-with-setup-hook
        (lambda () (insert user-input))
      (condition-case err
          (call-interactively #'ram-jump-to-def)
        (quit (abort-recursive-edit))
        (:success (abort-recursive-edit))))))

;;** minibuffer: bindings

;;*** minibuffer/bindings: global-map

(define-key global-map (kbd "C-_") #'prot/focus-minibuffer)
(define-key global-map (kbd "C-~") #'prot/focus-minibuffer-or-completions)

;; quit minibuffer from anywhere
(define-key global-map (kbd "C-^") #'top-level)

;;*** minibuffer/bindings: minibuffer-local-completion-map

(define-key minibuffer-local-completion-map (kbd "C-h f") #'ram-describe-function-from-minibuffer)
(define-key minibuffer-local-completion-map (kbd "C-h v") #'ram-describe-variable-from-minibuffer)
(define-key minibuffer-local-completion-map (kbd "C-h o") #'ram-describe-symbol-from-minibuffer)

(define-key minibuffer-local-completion-map (kbd "M-<f5>") #'ram-jump-to-outline-from-minibuffer)
(define-key minibuffer-local-completion-map (kbd "M-S-<f5>") #'ram-jump-to-def-from-minibuffer)
(define-key minibuffer-local-completion-map (kbd "M-w") #'ram-kill-ring-save-minibuffer-candidate)
(define-key minibuffer-local-completion-map (kbd "C-w") #'ram-kill-minibuffer-candidate)
(define-key minibuffer-local-completion-map (kbd "C-y") #'ram-insert-minibuffer-candidate)

(define-key minibuffer-local-completion-map (kbd "M-n")
  (lambda (arg) (interactive "p")
    (next-history-element arg)
    (move-end-of-line 1)))

(define-key minibuffer-local-completion-map (kbd "M-p")
  (lambda (arg) (interactive "p")
    (previous-history-element arg)
    (move-end-of-line 1)))

(define-key minibuffer-local-completion-map (kbd "?")
  (lambda () (interactive)
    "Call `minibuffer-completion-help' and select *Completions* window. "
    (minibuffer-completion-help)
    (let ((completions (get-buffer-window "*Completions*")))
      (if (and completions
               (not (eq (selected-window)
                        completions)))
          (select-window completions nil)))))

;; force input unconditionally
(define-key minibuffer-local-completion-map (kbd "C-j") #'exit-minibuffer)
(define-key minibuffer-local-completion-map (kbd "<return>") #'minibuffer-force-complete-and-exit)

;; Space should never complete
(define-key minibuffer-local-completion-map (kbd "SPC") nil)

;;** minibuffer: functions

;;*** minibuffer/functions: supporting functions

(defmacro ram-add-to-history-cmd (name history)
  `(defun ,name ()
     ,(format "Add search string entered in minibuffer to `%s'." (eval history))
     (interactive)
     (let ((search-str (buffer-substring
                        (line-beginning-position) (line-end-position 1))))
       (if (< 3 (length search-str))
           (progn
             (add-to-history ,history search-str))))
     (minibuffer-force-complete-and-exit)))

;;*** minibuffer/functions: ram-describe-variable

(defvar ram-describe-variable-history nil
  "`ram-describe-variable' history list.")
(put 'ram-describe-variable-history 'history-length 100)

(defun ram-describe-variable (variable &optional swap-history-p)
  "Describe variable and store the search string and input to history."
  (interactive
   (let ((v (variable-at-point))
         (enable-recursive-minibuffers t)
         (orig-buffer (current-buffer))
         ;; (default-history-add-val history-add-new-input)
         (old-binding (cdr (assoc 'return minibuffer-local-completion-map)))
         (hist-item (car ram-describe-variable-history))
         val)
     ;; (setq history-add-new-input nil)

     (define-key minibuffer-local-completion-map (kbd "<return>")
       (ram-add-to-history-cmd ram-add-to-describe-variable-history 'ram-describe-variable-history))

     (setq val (completing-read
                (if (symbolp v)
                    (format-prompt
                     "Describe variable" v)
                  (if (car ram-describe-variable-history)
                      (format-prompt "Describe variable" (car ram-describe-variable-history))
                    (format-prompt "Describe variable" nil)))
                #'help--symbol-completion-table
                (lambda (vv)
                  ;; In case the variable only exists in the buffer
                  ;; the command we switch back to that buffer before
                  ;; we examine the variable.
                  (with-current-buffer orig-buffer
                    (or (get vv 'variable-documentation)
                        (and (boundp vv) (not (keywordp vv))))))
                t nil 'ram-describe-variable-history
                (if (symbolp v)
                    (symbol-name v)
                  ram-describe-variable-history)))

     (define-key minibuffer-local-completion-map (kbd "<return>") old-binding)

     ;; (setq history-add-new-input default-history-add-val)
     (list (if (equal val "")
               v (intern val))
           ;; if two items are inserted, swap them so that the search str is first
           (let ((third-element (caddr ram-describe-variable-history))
                 (second-element (cadr ram-describe-variable-history)))
             (and second-element (equal hist-item third-element))))))

  (when swap-history-p
    (setq ram-describe-variable-history
          (cons (cadr ram-describe-variable-history)
                (cons (car ram-describe-variable-history)
                      (cddr ram-describe-variable-history)))))

  (let ((default history-add-new-input))
    (setq history-add-new-input nil)
    (describe-variable variable)
    (setq history-add-new-input default)))

(define-key global-map (kbd "C-h v") #'ram-describe-variable)

(defun prot/focus-minibuffer ()
    "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
    (interactive)
    (let ((mini (active-minibuffer-window)))
      (when mini
        (select-window mini))))

(defun prot/focus-minibuffer-or-completions ()
  "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`prot/focus-minibuffer' and `switch-to-completions' in
succession."
  (interactive)
  (let* ((mini (active-minibuffer-window))
         (completions (get-buffer-window "*Completions*")))
    (cond ((and mini
                (not (minibufferp)))
           (select-window mini nil))
          ((and completions
                (not (eq (selected-window)
                         completions)))
           (select-window completions nil)))))

;;*** minibuffer/functions: ram-describe-function

(defvar ram-describe-function-history nil
  "`ram-describe-function' history list.")
(put 'ram-describe-function-history 'history-length 100)

(defun ram-describe-function (fn-name &optional swap-history-p)
  "Describe function and store the search string and input to history."
  (interactive
   (let ((fn (save-excursion
               (cond
                ((looking-at ram-open-delimiters-re) (forward-char))
                ((looking-back ram-close-delimiters-re) (forward-sexp -1) (forward-char)))
               (function-called-at-point)))
         (enable-recursive-minibuffers t)
         (old-binding (cdr (assoc 'return minibuffer-local-completion-map)))
         (hist-item (car ram-describe-function-history))
         val)

     (define-key minibuffer-local-completion-map (kbd "<return>")
       (ram-add-to-history-cmd ram-add-to-describe-function-history 'ram-describe-function-history))

     (setq val (completing-read
                (if (and fn (symbol-name fn))
                    (format-prompt "Describe function" fn)
                  (if (car ram-describe-function-history)
                      (format-prompt "Describe function" (car ram-describe-function-history))
                    (format-prompt "Describe function" nil)))
                #'help--symbol-completion-table
                (lambda (f) (or (fboundp f) (get f 'function-documentation)))
                t nil 'ram-describe-function-history
                (if fn
                    (symbol-name fn)
                  ram-describe-function-history)))

     (define-key minibuffer-local-completion-map (kbd "<return>") old-binding)

     ;; (setq history-add-new-input default-history-add-val)
     (unless (equal val "")
       (setq fn (intern val)))
     (unless (and fn (symbolp fn))
       (user-error "You didn't specify a function symbol"))
     (unless (or (fboundp fn) (get fn 'function-documentation))
       (user-error "Symbol's function definition is void: %s" fn))
     (list fn
           ;; if two items are inserted, swap them so that the search str is first
           (let ((third-element (caddr ram-describe-function-history))
                 (second-element (cadr ram-describe-function-history)))
             (and second-element (equal hist-item third-element))))))

  ;; reorder history so that the search string is fist and the input is second.
  (when swap-history-p
    (setq ram-describe-function-history
          (cons (cadr ram-describe-function-history)
                (cons (car ram-describe-function-history)
                      (cddr ram-describe-function-history)))))

  (let ((default history-add-new-input))
    (setq history-add-new-input nil)
    (describe-function fn-name)
    (setq history-add-new-input default)))

;; (define-key ram-leader-map-tap-global "v" #'ram-describe-function)
(define-key global-map (kbd "C-h f") #'ram-describe-function)

;;*** minibuffer/functions: ram-jump-to-outline

(defvar ram-jump-to-outline-history nil "History for outlines to jump to.")
(put 'ram-jump-to-outline-history 'history-length 20)

(defun ram-make-duplicate-keys-unique (alist)
  "Keep `concat' \"*\" to duplicate keys in `alist' until all keys are unique."
  (cl-labels ((make-dups-unique (alist pair)
                                (if (assoc (car pair) alist)
                                    (cons (car alist) (make-dups-unique (cdr alist)
                                                                        (cons (concat (car pair) "*") (cdr pair))))
                                  (append (list pair) alist))))
    (cl-reduce #'make-dups-unique alist :initial-value '())))

(defun ram-jump-to-outline (outline &optional swap-history-p)
  "Jump to outline."
  (interactive
   (let ((headlines '())
         (buffer (if (minibufferp)
                     (with-minibuffer-selected-window
                       (current-buffer))
                   (current-buffer)))
         (headline-regex
          ;; TODO: use 'outline-regexp and copy the line
          (cond
           ((eq major-mode 'org-mode)
            "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$")
           ((eq major-mode 'markdown-mode)
            "^\\( *\\)\\(#+\\)\\(?: +\\(.*?\\)\\)?[[:blank:]]*$")
           ((eq major-mode 'python-mode)
            (rx line-start
                (* space)
                                        ; 0 or more spaces
                ;; (group (one-or-more (syntax comment-start)))
                (group (+ (syntax comment-start)))
                ;; Heading level
                (+ space)
                (group (repeat 1 8 "\*"))
                (+ space)
                ;; heading text
                ;; Must be accessible with (match-string 3)
                (group (+? not-newline))
                (* space)
                line-end))
           (t
            "^;;\\(?:\\(;+\\)\\|\\(\\*+\\)\\)\\(?: +\\(.*?\\)\\)?[ ]*$")))
         (old-binding-to-return (cdr (assoc 'return minibuffer-local-completion-map)))
         (hist-item (car ram-jump-to-outline-history)))
     (setf (alist-get 'return minibuffer-local-completion-map)
           (ram-add-to-history-cmd ram-add-to-jump-to-outline-history 'ram-jump-to-outline-history))
     (condition-case err
         (progn (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-max))
                    (while (re-search-forward headline-regex nil t -1)
                      (setq headlines (cons (cons (match-string-no-properties 3) (point)) headlines)))))
                (setq headlines (ram-make-duplicate-keys-unique headlines))
                (setq val (cdr (assoc (completing-read
                                       (format-prompt
                                        "Find heading" (car ram-jump-to-outline-history))
                                       headlines
                                       nil t nil
                                       'ram-jump-to-outline-history
                                       (car ram-jump-to-outline-history))
                                      headlines)))
                (define-key minibuffer-local-completion-map (kbd "<return>") old-binding-to-return)
                (list val
                      ;; if two items are inserted, swap them so that the search str is first
                      (let ((third-element (caddr ram-jump-to-outline-history))
                            (second-element (cadr ram-jump-to-outline-history)))
                        (and second-element (equal hist-item third-element)))))
       (quit
        (setf (alist-get 'return minibuffer-local-completion-map) old-binding-to-return)
        (signal 'quit nil))))

   ;; reorder history so that the search string is fist and the input is second.
   ;; Use it for different order when pressing <M-p> for previous history item.
   (when swap-history-p
     (setq ram-jump-to-outline-history
           (cons (cadr ram-jump-to-outline-history)
                 (cons (car ram-jump-to-outline-history)
                       (cddr ram-jump-to-outline-history))))))
  (when val
    (when (minibufferp)
      (let ((pre-minibuffer-buffer (with-minibuffer-selected-window
                                     (current-buffer))))
        (switch-to-buffer pre-minibuffer-buffer)))
    (push-mark)
    (goto-char val)
    (beginning-of-line)
    (recenter)
    ;; (let ((default (if (boundp pulse-flag)
    ;;                    pulse-flag
    ;;                  nil)))
    ;;   ;; pulse-iteration pulse-delay
    ;;   (setq pulse-flag nil)
    ;;   (pulse-momentary-highlight-one-line (point) 'isearch)
    ;;   (setq pulse-flag default))
    ))

;; (eval-after-load "org"
;;   '(define-key org-mode-map (kbd "<M-f5>") 'ram-jump-to-outline))
;; (define-key emacs-lisp-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)
(define-key global-map (kbd "<M-f5>") 'ram-jump-to-outline)

;;*** minibuffer/functions: ram-jump-to-def

(defvar ram-jump-to-def-history nil "History for definitions to jump to.")
(put 'ram-jump-to-def-history 'history-length 10)

(defun ram-jump-to-def-get-regexs (major-mode name-regex)
  "Return regex to capture definitions for MAJOR-MODE."
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (rx line-start
        (* space)
        (seq "("
             (group (or
                     "defalias"
                     "defconst"
                     "defcustom"
                     "define-abbrev-table"
                     "define-error"
                     "defun"
                     "defun-motion"
                     "defvar"
                     "define-minor-mode"
                     "defmacro"
                     "defsubst"
                     "cl-defun"
                     "cl-defstruct"
                     "cl-defmethod"
                     "set"
                     "setq"
                     "set-default")
                    (+ space)           ; 1 or more whitespaces
                    (regexp name-regex)))))
   ((eq major-mode 'racket-mode)
    (format "^(\\(def\\(?:ine\\) +%s\\)" name-regex))
   (t (error (format "%s is not supported, add regex to `ram-jump-to-def'" major-mode)))))

(defun ram-jump-to-def (def-str &optional swap-history-p)
  "Jump to def."
  (interactive
   (let* ((defs '())
          (buffer (if (minibufferp)
                      (with-minibuffer-selected-window
                        (current-buffer))
                    (current-buffer)))
          (def-regex (with-current-buffer buffer
                       (ram-jump-to-def-get-regexs major-mode "\\([^[:blank:]\t\r\n\v\f)]+\\)")))
          (old-binding-to-return (cdr (assoc 'return minibuffer-local-completion-map)))
          (hist-item (car ram-jump-to-def-history))
          (str-at-point (thing-at-point 'symbol))
          (old-binding-to-control-y (cdr (assoc ?\C-y minibuffer-local-completion-map))))
     ;; (define-key minibuffer-local-completion-map (kbd "<return>")
     ;;   (ram-add-to-history-cmd ram-add-to-jump-to-def-history 'ram-jump-to-def-history))
     (setf (alist-get ?\C-y minibuffer-local-completion-map)
           (lambda (arg) (interactive "p")
             (let ((candidate
                    (replace-regexp-in-string "\\*+$" ""
                                              (car (last (split-string (substring-no-properties
                                                                        (car completion-all-sorted-completions))))))))
               (when (minibufferp)
                 (with-minibuffer-selected-window
                   (insert candidate)
                   ;; exit minibuffer when no universal or digital arg (other than default 1)
                   (when (= 1 arg)
                     (top-level)))))))
     (setf (alist-get 'return minibuffer-local-completion-map)
           (ram-add-to-history-cmd ram-add-to-jump-to-def-history 'ram-jump-to-def-history))

     (condition-case err
         (progn (with-current-buffer buffer
                  (save-excursion
                    (goto-char (point-max))
                    (while (re-search-forward def-regex nil t -1)
                      ;; (cl-pushnew (match-string 1) defs)
                      ;; completing-read does not display duplicates,
                      ;; modify duplicate string to make it unique
                      (setq defs (cons (cons (match-string-no-properties 1) (point)) defs)))))
                ;; make duplicates unique adding "*" , otherwise, completing-read would not show them.
                (setq defs (ram-make-duplicate-keys-unique defs))
                (setq val (cdr (assoc (completing-read
                                       (if str-at-point
                                           (format-prompt "Jump to def" str-at-point)
                                         (format-prompt "Jump to def" nil))
                                       defs
                                       nil t nil
                                       'ram-jump-to-def-history
                                       (if str-at-point
                                           str-at-point
                                         ram-jump-to-def-history)
                                       nil)
                                      ;; (car ram-jump-to-def-history)
                                      defs)))
                (setf (alist-get ?\C-y minibuffer-local-completion-map) old-binding-to-control-y)
                (setf (alist-get 'return minibuffer-local-completion-map) old-binding-to-return)
                (list val
                      ;; if two items are inserted, swap them so that the search str is first
                      (let ((third-element (caddr ram-jump-to-def-history))
                            (second-element (cadr ram-jump-to-def-history)))
                        (and second-element (equal hist-item third-element)))))
       (quit
        (setf (alist-get ?\C-y minibuffer-local-completion-map) old-binding-to-control-y)
        (setf (alist-get 'return minibuffer-local-completion-map) old-binding-to-return)
        (signal 'quit nil)))))

  ;; reorder history so that the search string is fist and the input is second.
  ;; Use it for different order when pressing <M-p> for previous history item.
  (when swap-history-p
    (setq ram-jump-to-def-history
          (cons (cadr ram-jump-to-def-history)
                (cons (car ram-jump-to-def-history)
                      (cddr ram-jump-to-def-history)))))
  (when val
    (when (minibufferp)
      (let ((pre-minibuffer-buffer (with-minibuffer-selected-window
                                     (current-buffer))))
        (switch-to-buffer pre-minibuffer-buffer)))
    (push-mark)
    (goto-char val)
    (beginning-of-line)
    (recenter)))

;;** minibuffer: completion

;;*** minibuffer/completion: icomplete

;;**** minibuffer/completion/icomplete: settings

;; credit to:
;; https://gitlab.com/protesilaos/dotfiles/-/blob/master/emacs/.emacs.d/emacs-init.org

(fido-mode -1)                        ; Emacs 27.1
(icomplete-mode 1)

(setq icomplete-delay-completions-threshold 100)
(setq icomplete-max-delay-chars 2)
(setq icomplete-compute-delay 0.2)
(setq icomplete-show-matches-on-no-input t)
(setq icomplete-hide-common-prefix nil)
(setq icomplete-prospects-height 1)
;; (setq icomplete-separator (propertize " · " 'face 'shadow))
(setq icomplete-separator (propertize " · " 'face '((:foreground "green3"))))
(setq icomplete-with-completion-tables t)
(setq icomplete-tidy-shadowed-file-names t)

;;**** minibuffer/completion/icomplete: icomplete-vertical

(icomplete-vertical-mode 1)

;; (straight-use-package
;;  '(icomplete-vertical :type git :flavor melpa :host github :repo "oantolin/icomplete-vertical"))
;; (setq icomplete-vertical-prospects-height (/ (frame-height) 6))
;; (icomplete-vertical-mode -1)

;; credit to https://gitlab.com/protesilaos/dotfiles/-/blob/e8d6268866fb77c0aec6a6b68c9e7183daa65347/emacs/.emacs.d/emacs-init.org

(defun prot/kill-ring-yank-complete ()
  "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.

Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
  (interactive)
  (let ((kills                          ; do not sort items
         (lambda (string pred action)
           (if (eq action 'metadata)
               '(metadata (display-sort-function . identity)
                          (cycle-sort-function . identity))
             (complete-with-action
              action kill-ring string pred)))))
    (when (use-region-p)
      (delete-region (region-beginning) (region-end)))
    (insert
     (completing-read "Yank from kill ring: " kills nil t))))

(define-key global-map (kbd "C-s-y") #'prot/kill-ring-yank-complete)
(define-key icomplete-minibuffer-map (kbd "C-v") #'icomplete-vertical-toggle)

;;**** minibuffer/completion/icomplete: bindings

(define-key icomplete-minibuffer-map (kbd "<tab>") #'icomplete-force-complete)
;; exit with completion
;; (define-key icomplete-minibuffer-map (kbd "<return>") #'icomplete-force-complete-and-exit)

;; force input unconditionally
(define-key icomplete-minibuffer-map (kbd "C-j") #'exit-minibuffer)
(define-key icomplete-minibuffer-map (kbd "C-n") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<right>") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "<down>") #'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-p") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<left>") #'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "<up>") #'icomplete-backward-completions)
;; this command is from Emacs 27.1
(define-key icomplete-minibuffer-map (kbd "<C-backspace>") #'icomplete-fido-backward-updir)

;;**** minibuffer/completion/icomplete: functions

(defun prot/icomplete-minibuffer-truncate ()
  "Truncate minibuffer lines in `icomplete-mode'.
This should only affect the horizontal layout and is meant to
enforce `icomplete-prospects-height' being set to 1, which is
what I always want.

Hook it to `icomplete-minibuffer-setup-hook'."
  (when (and (minibufferp)
             (bound-and-true-p icomplete-mode))
    (setq truncate-lines t)))

(add-hook 'icomplete-minibuffer-setup-hook #'prot/icomplete-minibuffer-truncate)

;;*** minibuffer/completion: buffer actions

(defun prot/completions-kill-save-symbol ()
  "Add symbol-at-point to the kill ring.

Intended for use in the \\*Completions\\* buffer.  Bind this to a
key in `completion-list-mode-map'."
  (interactive)
  (kill-new (thing-at-point 'symbol)))

(defmacro prot/completions-buffer-act (name doc &rest body)
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((completions-window (get-buffer-window "*Completions*"))
           (completions-buffer (get-buffer "*Completions*"))
           (symbol (thing-at-point 'symbol)))
       (if (window-live-p completions-window)
           (with-current-buffer completions-buffer
             ,@body)
         (user-error "No live window with Completions")))))

(prot/completions-buffer-act
 prot/completions-kill-symbol-at-point
 "Add \"Completions\" buffer symbol-at-point to the kill ring."
 (kill-new `,symbol)
 (message "Copied %s to kill-ring"
          (propertize `,symbol 'face 'success)))

(prot/completions-buffer-act
 prot/completions-insert-symbol-at-point
 "Add \"Completions\" buffer symbol-at-point to active window."
 (let ((window (window-buffer (get-mru-window))))
   (with-current-buffer window
     (insert `,symbol)
     (message "Inserted %s"
              (propertize `,symbol 'face 'success)))))

(prot/completions-buffer-act
 prot/completions-insert-symbol-at-point-exit
 "Like `prot/completions-insert-symbol-at-point' plus exit."
 (prot/completions-insert-symbol-at-point)
 (top-level))

  ;; Technically, this is not specific to the minibuffer, but I define
  ;; it here so that you can see how it is also used from inside the
  ;; "Completions" buffer
(defun prot/describe-symbol-at-point (&optional arg)
  "Get help (documentation) for the symbol at point.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
  (interactive "P")
  (let ((symbol (symbol-at-point)))
    (when symbol
      (describe-symbol symbol)))
  (when arg
    (let ((help (get-buffer-window "*Help*")))
      (when help
        (if (not (eq (selected-window) help))
            (select-window help)
          (select-window (get-mru-window)))))))

(define-key completion-list-mode-map (kbd "h") #'prot/describe-symbol-at-point)
(define-key completion-list-mode-map (kbd "w") #'prot/completions-kill-symbol-at-point)
(define-key completion-list-mode-map (kbd "i") #'prot/completions-insert-symbol-at-point)
(define-key completion-list-mode-map (kbd "j") #'prot/completions-insert-symbol-at-point-exit)
(define-key completion-list-mode-map (kbd "n") #'next-line)
(define-key completion-list-mode-map (kbd "p") #'previous-line)
(define-key completion-list-mode-map (kbd "b") #'previous-completion)
(define-key completion-list-mode-map (kbd "M-v") #'prot/focus-minibuffer)

;;*** minibuffer/completion: styles

(setq completion-styles
      '(orderless))
;; (setq completion-styles
;;       '(basic partial-completion substring))

;;**** minibuffer/completion/styles: orderless

(straight-use-package
 '(orderless :type git :flavor melpa :host github :repo "oantolin/orderless"))

(with-eval-after-load 'orderless
  (setq orderless-regexp-separator "[/\s_-]+")
  ;; (setq orderless-matching-styles
  ;;       '(orderless-strict-leading-initialism
  ;;         orderless-regexp
  ;;         orderless-prefixes
  ;;         orderless-literal))
  (setq orderless-matching-styles
        '(orderless-regexp))
  ;; orderless-initialism


  (defun prot/orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot/orderless-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

  (setq orderless-style-dispatchers
        '(prot/orderless-literal-dispatcher
          prot/orderless-initialism-dispatcher)))

;;* frame, window, buffer, sentence

;;** frame

;; transparency
;; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
;; (add-to-list 'default-frame-alist '(alpha . (90 . 90)))
;; (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
;; (add-to-list 'default-frame-alist '(fullscreen . maximized))


;;** windows

;;*** windows: delete

(defun ram-quit-other-windows ()
  "Quit `ram-info-buffer-p' buffers first.
Then, quit other window."
  (interactive)
  (let* ((win-list (window-list-1 nil 'nomini 'A))
         (win-info-buf-list (seq-filter (lambda (w) (ram-info-buffer-p (window-buffer w) nil)) win-list))
         (win-interactive-buf-list (seq-filter (lambda (w) (ram-interactive-buffer-p (window-buffer w) nil)) win-list)))
    (cond
     ((car win-interactive-buf-list) (quit-window nil (car win-interactive-buf-list)))
     ((car win-info-buf-list) (quit-window nil (car win-info-buf-list)))
     ((cadr win-list) (quit-window nil (cadr win-list))))))

(define-key global-map (kbd "s-w") 'ram-quit-other-windows)
;; <XF86Copy> key is in layer where F1-F12 defined, in place of "w" key
(define-key global-map (kbd "<M-XF86Copy>") 'delete-other-windows)
(define-key global-map (kbd "<M-S-XF86Copy>") 'delete-window)
(define-key global-map (kbd "s-W") 'delete-window)

;;*** windows: navigate

(defun ram-other-window (count)
  "Call `other-window' with added ALL-FRAMES."
  (interactive "p")
  (other-window count 'visible))

(define-key global-map (kbd "s-o") 'other-window)
(define-key global-map (kbd "<M-S-f15>") (lambda () (interactive) (other-window -1)))

;;*** windows: general settings
;; used for splitting windows sensibly, default width 160, height 80
;; nil values would forbid splitting
(setq split-width-threshold 150)
(setq split-height-threshold 180)

(setq switch-to-buffer-preserve-window-point t)
(setq switch-to-buffer-obey-display-actions t)

;;*** windows: swap, move left and right

;; credit to https://www.emacswiki.org/emacs/buffer-move.el
(require 'windmove)

(defun buf-move-left ()
 "Swap the current buffer and the buffer on the left of the split.
If there is no split, ie now window on the left of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'left))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No left split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(defun buf-move-right ()
 "Swap the current buffer and the buffer on the right of the split.
If there is no split, ie now window on the right of the current
one, an error is signaled."
  (interactive)
  (let* ((other-win (windmove-find-other-window 'right))
         (buf-this-buf (window-buffer (selected-window))))
    (if (null other-win)
        (error "No right split")
      ;; swap top with this one
      (set-window-buffer (selected-window) (window-buffer other-win))
      ;; move this one to top
      (set-window-buffer other-win buf-this-buf)
      (select-window other-win))))

(define-key global-map (kbd "C-s-c") #'buf-move-left)
(define-key global-map (kbd "C-s-t") #'buf-move-right)

;;*** windows: winner-mode

;; undo and redo changes in window configuration
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (define-key global-map (kbd "C-s-p") 'winner-undo)
  (define-key global-map (kbd "C-s-n") 'winner-redo))

;;** buffers

;; zero disable the mode
(global-auto-revert-mode 0)

;;*** buffers: no prompt kill

(defun ram-kill-curr-buffer (arg)
  (interactive "p")
  ;; do not save buffers that strat with "*"
  (if (or (string-match "^\\*.*$" (buffer-name (current-buffer)))
          (ram-info-buffer-p (current-buffer) nil)
          (ram-interactive-buffer-p (current-buffer) nil))
      (kill-buffer (current-buffer))
    (when (not arg) (save-buffer (current-buffer)))
    (kill-buffer (current-buffer))))
(global-set-key (kbd "C-x k") #'ram-kill-curr-buffer)

;;*** buffers: switch

(defun ram-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key global-map (kbd "s-p") #'switch-to-prev-buffer)
(define-key global-map (kbd "s-n") #'switch-to-next-buffer)

;;*** buffers: display

;;**** buffers/display: interactive

(defvar ram-interactive-modes
  '(cider-repl-mode
    dired-mode
    racket-repl-mode))

(defvar ram-interactive-buffer-names '("*eshell*"))

(defun ram-interactive-buffer-p (buf act)
  "Check if BUF belongs to interactive buffers."
  (or
   (with-current-buffer buf
     (cond ((memq major-mode ram-interactive-modes))
           ((derived-mode-p ram-interactive-modes))))
   (let* ((buffer-name (if (stringp buf) buf (buffer-name buf)))
          (names ram-interactive-buffer-names))
     ;; credit to https://emacs.stackexchange.com/a/28689
     (while (and names (not (string-match (car names) buffer-name)))
       (setq names (cdr names)))
     names)))

;;**** buffers/display: information
(defvar ram-info-modes '(apropos-mode
                         debugger-mode
                         diff-mode
                         helpful-mode
                         ivy-occur-mode
                         ivy-occur-grep-mode
                         lisp-interaction-mode
                         ;; magit-diff-mode
                         messages-buffer-mode
                         Man-mode
                         occur-mode
                         pydoc-mode
                         pylookup-mode
                         rg-mode
                         woman-mode))

(defvar ram-info-buffers '("*ob-ipython-inspect*"
                           "*Ivy Help*"
                           ;; "*scratch*"
                           "*Backtrace*"))

(defun ram-info-buffer-p (buf act)
  "Check if BUF belongs to a custom info group."
  (or ;; (member (buffer-local-value 'major-mode  (get-buffer buf)) ram-info-modes)
   (with-current-buffer buf
     (cond ((memq major-mode ram-info-modes))
           ((derived-mode-p ram-info-modes))))
   (let* ((buffer-name (if (stringp buf) buf (buffer-name buf)))
          (names ram-info-buffers))
     ;; credit to https://emacs.stackexchange.com/a/28689
     (while (and names (not (string-match (car names) buffer-name)))
       (setq names (cdr names)))
     names)))

;;**** buffers/display: action functions

(defun ram-display-buffer-split-right (buffer alist)
  "Display buffer in a split window to the right."
  (let ((window (split-window-no-error (window-main-window) nil t)))
    (when window
      (setq window (window--display-buffer buffer window 'window alist))
      (balance-windows-area)
      window)))

(defun ram-display-buffer-reuse-right-window (buffer alist)
  "Display buffer in a right window if it exists."
  (let ((inhibit-same-window (cdr (assq 'inhibit-same-window alist)))
        right-window)
    (walk-window-tree
     (lambda (window)
       (cond
        ((and (not right-window)
              (not (and inhibit-same-window (not (eq (window-main-window) window))))
              (window-in-direction 'right window))
         (setq right-window (window-in-direction 'right window)))))
     nil nil 'nomini)
    (when right-window
      (window--display-buffer buffer right-window 'reuse alist))))

(defun ram-display-buffer-in-info-window (buf alist)
  "Display buffer in window with `ram-info-buffer-p' buffer."
  (let (info-windows)
    (dolist (window (window-list-1 nil 'nomini 'A))
      (when (ram-info-buffer-p (window-buffer window) nil)
        (push window info-windows)))
    ;; (set-window-buffer (car info-windows) buf)
    (when info-windows
      (window--display-buffer buf (car info-windows) 'reuse alist))))

(defun ram-display-buffer-in-interactive-window (buf alist)
  "Display buffer in window with `ram-interactive-buffer-p' buffer."
  (let (interactive-windows)
    (dolist (window (window-list-1 nil 'nomini 'A))
      (when (ram-interactive-buffer-p (window-buffer window) nil)
        (push window interactive-windows)))
    ;; (set-window-buffer (car interactive-windows) buf)
    (when interactive-windows
      (window--display-buffer buf (car interactive-windows) 'reuse alist))))

(defun ram-display-buffer-common-window (buf alist)
  "Display buffer in a common window"
  (when (not (or (cdr (assq 'inhibit-same-window alist))
                 (window-minibuffer-p)
                 (window-dedicated-p)))
    (let ((buf-name (if (stringp buf) buf (buffer-name buf)))
          (buf-mode (with-current-buffer buf major-mode))
          same-name
          same-mode)
      (dolist (window (window-list-1 nil 'nomini 'A))
        (let ((name? (with-current-buffer (window-buffer window)
                       (cond ((equal buf-name (buffer-name)) 'same))))
              (mode? (with-current-buffer (window-buffer window)
                       (cond ((eq major-mode buf-mode) 'same)))))
          (push window (cond
                        ((eq name? 'same) same-name)
                        ((eq mode? 'same) same-mode)
                        ((eq mode? 'derived) derived-mode)))))
      (let* ((windows (nconc ;; (reverse same-name)
                             (reverse same-mode)))
             (window (car windows)))
        (when (window-live-p window)
          (window--display-buffer buf window 'reuse alist))))))

(defun ram-display-buffer-in-other-window (buf alist)
  "Display buffer in any other window if it exists."
  (let (windows)
    (dolist (window (window-list-1 nil 'nomini 'A))
      (when (not (eq window (selected-window)))
        (push window windows)))
    (when windows
      (window--display-buffer buf (car windows) 'reuse alist))))

(defun ram-display-buffer-in-same-window (buf alist)
  "Display buffer in the same window."
  (let ((buffer-window (get-buffer-window buf 'A)))
    (when buffer-window
      (window--display-buffer buf buffer-window 'reuse alist))))

;;**** buffers/display: alist

;;***** buffers/display/alist: functions

(defun ram-create-display-buffer-in-specific-monitor-alist-element (test-buffer-p workspace-idx)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the condition part of the element. The action
part returns a window for displaying the buffer in WORKSPACE-IDX
`exwm-mode' workspace."
  (list test-buffer-p
        `((lambda (buffer alist)
            ,(format "Display BUFFER in exwm desktop %d" workspace-idx)
            (let* ((frame (exwm-workspace--workspace-from-frame-or-index ,workspace-idx))
                   (window (car (window-list-1 nil 'nomini frame)))
                   (selected-frm (selected-frame)))
              (when window
                (exwm-workspace-switch ,workspace-idx)
                (exwm-workspace-switch selected-frm)
                (window--display-buffer buffer window 'reuse alist)))))))

(defun ram-create-display-buffer-in-primary-workspace-alist-element (test-buffer-p primary secondary)
  "Return an element to be added to `display-buffer-alist'.

 TEST-BUFFER-P as the condition part of the element. The action
part returns a window for displaying the buffer in
`exwm-mode' workspaces PRIMARY or SECONDARY.

The workspace is chosen by a set of conditions in `cond'
expression."

  (list test-buffer-p
        `((lambda (buffer alist)
            ,(format "Display BUFFER in exwm workspace %d or %d" primary secondary)
            (let* ((primary-frame (exwm-workspace--workspace-from-frame-or-index ,primary))
                   (primary ,primary)
                   (secondary ,secondary)
                   (buffer-sameness-p (lambda (frm)
                                        (,test-buffer-p (window-buffer (frame-selected-window frm)))))
                   (selected-frm (selected-frame))
                   ;; decide between primary and secondary workspaces
                   (workspc (cond
                             ;; primary-frame is not active, select it
                             ((not (frame-parameter primary-frame 'exwm-active))
                              ;; (message "???????? case 1")
                              primary)
                             ;; primary is active but buffer-sameness-p is false, select it
                             ((and (frame-parameter primary-frame 'exwm-active)
                                   (not (funcall buffer-sameness-p primary-frame)))
                              ;; (message "???????? case 2")
                              primary)
                             ;; selected primary is displaying sameness buffer, select secondary
                             ((and (eq selected-frm primary-frame)
                                   (funcall buffer-sameness-p primary-frame))
                              ;; (message "???????? case 3")
                              secondary)
                             ;; not selected primary is displaying sameness buffer
                             ((and (frame-parameter primary-frame 'exwm-active)
                                   (not (eq selected-frm primary-frame))
                                   (funcall buffer-sameness-p primary-frame))
                              ;; selected displaying sameness buffer too, select primary
                              ;; (message "???????? case 4")
                              (if (funcall buffer-sameness-p selected-frm)
                                  primary
                                secondary))
                             (t
                              ;; (message "???????? case default")
                              primary)))
                   ;; ALIST indicates it wants other window or frame
                   (other-frame-p (or (cdr (assq 'inhibit-same-window alist))
                                      (cdr (assq 'reusable-frames alist))))
                   ;; swap workspc when other-frame-p is true
                   (workspc (if other-frame-p
                                (if (= workspc primary) secondary primary)
                              workspc))
                   (workspc-frm (exwm-workspace--workspace-from-frame-or-index workspc))
                   (window-to-display-in (car (window-list-1 nil 'nomini workspc-frm))))
              (when window-to-display-in
                (exwm-workspace-switch workspc-frm)
                ;; when new and selected frame share same monitor, keep new one active
                (when (not (string= (frame-parameter workspc-frm 'exwm-randr-monitor)
                                    (frame-parameter selected-frm 'exwm-randr-monitor)))
                  (exwm-workspace-switch selected-frm))
                (window--display-buffer buffer window-to-display-in 'reuse alist)))))))

(defun ram-create-display-buffer-in-other-monitor-alist-element (test-buffer-p primary secondary)
  "Return an element to be added to `display-buffer-alist'.

This element is of the form (CONDITION . ACTION) where
test-buffer-p is CONDITION. The ACTION function return a window
on either PRIMARY or SECONDARY `exwm-randr-monitor'."

  (list test-buffer-p
        `((lambda (buffer alist)
            ,(format "Display BUFFER in exwm workspace %d or %d" primary secondary)
            (let* ((primary-frame (exwm-workspace--workspace-from-frame-or-index ,primary))
                   (primary ,primary)
                   (secondary ,secondary)
                   (buffer-sameness-p (lambda (frm)
                                        (,test-buffer-p (window-buffer (frame-selected-window frm)))))
                   (selected-frm (selected-frame))
                   ;; decide between primary and secondary workspaces
                   (workspc (cond
                             ;; selected is displaying sameness buffer, choose it
                             ((funcall buffer-sameness-p (selected-frame))
                              (if (eq (selected-frame) primary-frame)
                                  primary
                                secondary))
                             (t
                              ;; choose workspace in the other monitor
                              (if (string= (frame-parameter (selected-frame) 'exwm-randr-monitor)
                                           (frame-parameter primary-frame 'exwm-randr-monitor))
                                  secondary
                                primary))))
                   (window-to-display-in
                    (car (window-list-1 nil 'nomini
                                        (exwm-workspace--workspace-from-frame-or-index workspc)))))
              (when window-to-display-in
                (exwm-workspace-switch workspc)
                (exwm-workspace-switch selected-frm)
                (window--display-buffer buffer window-to-display-in 'reuse alist)))))))

(defun ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element (test-buffer-p workspace-idx)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the CONDITION part of (CONDITION . ACTION). The
ACTION part returns a `exwm-mode' WORKSPACE-IDX window to display
the buffer.

It splits the window horizontally if TEST-BUFFER-P is not
visible."
  (list test-buffer-p
        `(lambda (buffer alist)
           ,(format "Display BUFFER in workspace %d in a horizontal split." workspace-idx)
           (let* ((target-frame (exwm-workspace--workspace-from-frame-or-index ,workspace-idx))
                  (target-window (frame-selected-window target-frame))
                  (next-to-target-window (next-window target-window 'nomini target-frame)))
             (cond
              ;; reuse target-window displaying same buffer
              ((string= (buffer-name (window-buffer target-window))
                        (if (stringp buffer) buffer (buffer-name buffer)))
               (window--display-buffer buffer target-window 'reuse alist))
              ;; reuse next-to-target-window
              ((and (window-live-p next-to-target-window) (not (eq target-window next-to-target-window)))
               (window--display-buffer buffer next-to-target-window 'reuse alist))
              ;; split unconditionally
              (t (let ((new-window (split-window-no-error target-window nil 'below)))
                   (when new-window
                     (setq new-window (window--display-buffer buffer new-window 'window alist))
                     (balance-windows-area)
                     new-window))))))))

(defun ram-create-display-buffer-in-same-monitor-horiz-split-alist-element (test-buffer-p)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the CONDITION part of (CONDITION . ACTION). The
ACTION part returns a window to display the buffer in the
`#'selected-frame'.

It splits the window horizontally if TEST-BUFFER-P is not
visible."
  (list test-buffer-p
        `(lambda (buffer alist)
           ,(format "Display BUFFER in a horizontal split.")
           (let* (
                  (target-window (frame-selected-window (selected-frame)))
                  (next-to-target-window (next-window target-window 'nomini (selected-frame))))
             (cond
              ;; reuse target-window displaying same buffer
              ((string= (buffer-name (window-buffer target-window))
                        (if (stringp buffer) buffer (buffer-name buffer)))
               (window--display-buffer buffer target-window 'reuse alist))
              ;; reuse next-to-target-window
              ((and (window-live-p next-to-target-window) (not (eq target-window next-to-target-window)))
               (window--display-buffer buffer next-to-target-window 'reuse alist))
              ;; split unconditionally
              (t (let ((new-window (split-window-no-error target-window nil 'below)))
                   (when new-window
                     (setq new-window (window--display-buffer buffer new-window 'window alist))
                     (balance-windows-area)
                     new-window))))))))

(defun ram-create-display-buffer-in-other-monitor-horiz-split-alist-element (test-buffer-p primary secondary)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the CONDITION part of (CONDITION . ACTION). The
ACTION part returns a window to display the buffer in either
PRIMARY or SECONDARY `exwm-mode' workspaces.

It splits the selected window horizontally if it is not
displaying TEST-BUFFER-P buffer."
  (list test-buffer-p
        `(lambda (buffer alist)
           ,(format "Display BUFFER in exwm desktop %d or %d with horizontal split." primary secondary)
           (let* ((primary-frame (exwm-workspace--workspace-from-frame-or-index ,primary))
                  (primary ,primary)
                  (secondary ,secondary)
                  (buffer-sameness-p (lambda (frm)
                                        (,test-buffer-p (window-buffer (frame-selected-window frm)))))
                  (workspc (cond
                            ;; selected is displaying sameness buffer, choose it
                            ((funcall buffer-sameness-p (selected-frame))
                             (if (eq (selected-frame) primary-frame)
                                 primary
                               secondary))
                            (t
                             ;; if selected workspace is in the same monitor as the primary,
                             ;; choose secondary.
                             (if (string= (frame-parameter (selected-frame) 'exwm-randr-monitor)
                                          (frame-parameter primary-frame 'exwm-randr-monitor))
                                 secondary
                               primary))))
                  (target-frame (exwm-workspace--workspace-from-frame-or-index workspc))
                  (target-window (frame-selected-window target-frame))
                  (next-to-target-window (next-window target-window 'nomini target-frame)))
             (exwm-workspace-switch target-frame)
             (cond
              ;; reuse target-window displaying same buffer
              ((string= (buffer-name (window-buffer target-window))
                        (if (stringp buffer) buffer (buffer-name buffer)))
               (window--display-buffer buffer target-window 'reuse alist))
              ;; reuse next-to-target-window
              ((and (window-live-p next-to-target-window) (not (eq target-window next-to-target-window)))
               (window--display-buffer buffer next-to-target-window 'reuse alist))
              ;; if 'target-window buffer is of the same mode, split it horizontally
              ;; ((funcall buffer-sameness-p target-frame)
              ;;  (let ((new-window (split-window-no-error target-window nil 'below)))
              ;;    (when new-window
              ;;      (setq new-window (window--display-buffer buffer new-window 'window alist))
              ;;      (balance-windows-area)
              ;;      new-window)))
              ;; split unconditionally
              (t (let ((new-window (split-window-no-error target-window nil 'below)))
                   (when new-window
                     (setq new-window (window--display-buffer buffer new-window 'window alist))
                     (balance-windows-area)
                     new-window)))
              ;; reuse 'target-window
              (t (window--display-buffer buffer target-window 'reuse alist)))))))

;;***** buffers/display/alist: (add-to-list 'display-buffer-alist ...)

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (string-match-p "\\*Help\\*" buf-name)
                      (string-match-p "^\\*info\\*\\(<[0-9]+>\\)?$" buf-name)
                      (string-match-p "\\*Messages\\*" buf-name)
                      (string-match-p "^magit.*$" buf-name))))
              6 4))


;;****** buffers/display/alist: emacs-lisp

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-primary-workspace-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'emacs-lisp-mode mode)))
              2 8))


;;****** buffers/display/alist: deft

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-monitor-alist-element
              (lambda (buffer &optional alist)
                (string-match-p (regexp-quote "*Deft*")
                                (if (stringp buffer) buffer (buffer-name buffer)))) 9))

;;****** buffers/display/alist: clojure

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-primary-workspace-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'clojure-mode mode)))
              2 8))

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (eq 'cider-repl-mode mode)
                      (eq 'cider-stacktrace-mode mode))))
              8 2))

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-primary-workspace-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (string-match-p "^\\*Org Src .+?\\[ clojure \\]\\*$" buf-name)))
              2 8))


;;****** buffers/display/alist: racket

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-primary-workspace-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'racket-mode mode)))
              2 8))

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-same-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (eq 'racket-repl-mode mode)))))

;;****** buffers/display/alist: org

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-primary-workspace-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'org-mode mode)))
              8 2))


;;****** buffers/display/alist: org weekly

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (string-match-p "^CAPTURE\\(-[0-9]\\)\\{,1\\}-[0-9]\\{4\\}-[0-9]\\{2\\}-w[0-9]\\{1,2\\}\\.org$" buf-name)
                      (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-w[0-9]\\{1,2\\}\\.org$" buf-name))))
              7))

;;****** buffers/display/alist: org-roam dailies

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (string-match-p "^CAPTURE\\(-[0-9]\\)\\{,1\\}-[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$" buf-name)
                      (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$" buf-name))))
              7))

;;****** buffers/display/alist: org-roam notes

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (string-match-p "^CAPTURE-[0-9]\\{14\\}-.+\\.org$" buf-name)
                      (string-match-p "^[0-9]\\{14\\}-.+\\.org$" buf-name))))
              7))

;;****** buffers/display/alist: eshell, dired

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (eq 'dired-mode mode)
                      (string-match-p "^\\*eshell\\*<[0-9]+>$" buf-name))))
              7 3))

;;****** buffers/display/alist: *scratch*

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-monitor-alist-element
              (lambda (buffer &optional alist)
                (string-match-p (regexp-quote "*scratch*")
                                (if (stringp buffer) buffer (buffer-name buffer)))) 0))

;; (add-to-list 'display-buffer-alist
;;              `("*"
;;                ((lambda (buf alist) (progn (print
;;                                             (format "################ any buffer type : buf name: %s, mode: %s" buf
;;                                                     (with-current-buffer buf major-mode))) nil)))))

;;*** buffers: breadcrumbs

;; (require 'breadcrumbs)

;; (autoload 'global-breadcrumbs-mode "breadcrumbs")
(autoload 'breadcrumbs-blink "breadcrumbs")

;; (global-breadcrumbs-mode 1)

;;** sentences
(setq sentence-end-double-space nil)

;;* org-mode

;; (straight-use-package
;;  '(org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org"))

;; this recipe takes very long time to clone
(straight-use-package
 '(org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org"
       :depth full          ; default value is full
       :pre-build (straight-recipes-org-elpa--build)
       ;; :branch "release_9.5"
       :build (:not autoloads) :files (:defaults "lisp/*.el" ("etc/styles/" "etc/styles/*"))))

;;** org-mode: functions

;; credit to https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(defun ram-org-buffer-contains-todos-p (&optional file-path)
  "Return non-nil if FILE buffer contains any to-dos.

If no arg provided, default to the current buffer.
"
  (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))
  (let (buffer kill-buffer-p contains-todos-p)
    (setq buffer (let ((buffer (find-buffer-visiting file-path)))
                   (if buffer
                       ;; the file visited, do not closed it
                       (progn (setq kill-buffer-p nil)
                              buffer)
                     ;; the file was not visited, close it
                     (setq kill-buffer-p t)
                     (setq buffer (ram-eval-exp-with-modified-hooks
                                   (find-file-noselect file-path)
                                   ((find-file-hook '())
                                    (org-mode-hook '()))))
                     buffer)))

    (setq contains-todos-p
          (with-current-buffer buffer
            (org-element-map
                (org-element-parse-buffer 'headline)
                'headline
              (lambda (h)
                (eq (org-element-property :todo-type h)
                    'todo))
              nil 'first-match)))
    (when kill-buffer-p
      (ram-eval-exp-with-modified-hooks
       (kill-buffer buffer)
       ((before-save-hook nil)
        (after-save-hook nil))))
    contains-todos-p))

;; credit to https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el
(defun zp/org-find-time-file-property (property &optional anywhere)
  "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
  (save-excursion
    (goto-char (point-min))
    (let ((first-heading
           (save-excursion
             (re-search-forward org-outline-regexp-bol nil t))))
      (when (re-search-forward (format "^#\\+%s:" property)
                               (if anywhere nil first-heading)
                               t)
        (point)))))

(defun zp/org-has-time-file-property-p (property &optional anywhere)
  "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
  (when-let ((pos (zp/org-find-time-file-property property anywhere)))
    (save-excursion
      (goto-char pos)
      (if (and (looking-at-p " ")
               (progn (forward-char)
                      (org-at-timestamp-p 'lax)))
          pos
        -1))))
(defun zp/org-set-time-file-property (property &optional anywhere pos)
  "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
  (when-let ((pos (or pos
                      (zp/org-find-time-file-property property))))
    (save-excursion
      (goto-char pos)
      (if (looking-at-p " ")
          (forward-char)
        (insert " "))
      (delete-region (point) (line-end-position))
      (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
        (insert now)))))

(defun zp/org-set-last-modified ()
  "Update the LAST_MODIFIED file property in the preamble."
  (when (derived-mode-p 'org-mode)
    (zp/org-set-time-file-property "LAST_MODIFIED")))

(defun ram-hide-block-toggle ()
  "Toggle visibility from anywhere in the block."
  (interactive)
  (cond
   ((org-at-block-p) (org-hide-block-toggle))
   ((org-in-src-block-p)
    (org-previous-block 1)
    (org-hide-block-toggle))
   (t (org-hide-block-toggle))))

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<C-tab>") #'ram-hide-block-toggle))

;;** org-mode: bindings

(defun ram-org-hide-block-toggle-all ()
  "interactive org-hide-block-toggle-all"
  (interactive)
  (org-hide-block-toggle-all))

(defun ram-push-mark-for-none-consecutive-cmd (&optional arg cmd)
  "Push mark only if `this-command' is different from `last-command'."
  (interactive "p")
  (unless (eq this-command last-command)
    (push-mark))
  (funcall cmd arg))

(defun ram-wrap-in-~ ()
  "Insert \"~\" in front of a sexp and after it."
  (interactive)
  (backward-sexp 1)
  (insert "~")
  (backward-sexp -1)
  (insert "~"))

(with-eval-after-load "org"
  ;; originally, C-' runs the command org-cycle-agenda-files
  (define-key org-mode-map (kbd "C-'") nil)

  (define-key org-mode-map (kbd "<M-f19>") (lambda (arg) (interactive "p")
                                            (ram-push-mark-for-none-consecutive-cmd arg #'org-next-block)))
  (define-key org-mode-map (kbd "C-c M-f") (lambda (arg) (interactive "p")
                                             (ram-push-mark-for-none-consecutive-cmd arg #'org-next-block)))
  (define-key org-mode-map (kbd "<M-f20>") (lambda (arg) (interactive "p")
                                            (ram-push-mark-for-none-consecutive-cmd arg #'org-previous-block)))
  (define-key org-mode-map (kbd "C-c M-b") (lambda (arg) (interactive "p")
                                             (ram-push-mark-for-none-consecutive-cmd arg #'org-previous-block)))

  (define-key org-mode-map (kbd "C-c C-n") #'org-next-link)
  (define-key org-mode-map (kbd "C-c C-p") #'org-previous-link)

  (define-key org-mode-map (kbd "C-c z") 'ram-org-hide-block-toggle-all)
  ;; originally bound to 'org-table-copy-down
  (define-key org-mode-map (kbd "<S-return>") 'newline-and-indent)
  (define-key org-mode-map (kbd "M-h") (lambda () (interactive) (push-mark) (org-mark-element)))
  (define-key org-mode-map (kbd "C-~") #'ram-wrap-in-~)

  (define-key org-mode-map (kbd "s-R") #'ram-avy-goto-org-heading)
  (define-key org-mode-map (kbd "s-l") #'ram-avy-goto-org-link)

  (define-key global-map (kbd "C-c C-S-L") #'org-store-link)
  (define-key org-mode-map (kbd "C-c C-l") #'org-insert-link))

;;** org-mode: settings

(setq org-blank-before-new-entry
      '((heading . always) (plain-list-item . nil)))

;; toggle it with "C-c C-x \" or "M-x org-toggle-pretty-entities"
(setq org-pretty-entities t)

(with-eval-after-load "org"
  (add-to-list 'org-tags-exclude-from-inheritance "project"))

;;*** org-mode/settings: tags

(setq org-tag-alist '((:startgroup . nil)
                      ;; ("@work" . ?w)
                      ;; ("@home" . ?h)
                      (:endgroup . nil)
                      (:newline . nil)
                      ("continue" . ?c)
                      ("emacs" . ?e)
                      ("learn" . ?l)
                      ("read" . ?r)
                      ("review" . ?v)
                      ("write" . ?w)))
(setq org-fast-tag-selection-single-key 'expert)

;; enable org-indent-mode for every file
;; per file sittings: #+STARTUP: indent, #+STARTUP: noindent
(setq org-startup-indented t)
(setq org-indent-mode-turns-on-hiding-stars nil)
(setq org-indent-mode-turns-off-org-adapt-indentation t)
(setq org-link-file-path-type 'absolute)

;;** org-mode: hooks, advice, timers

;; (add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-hide-block-all)
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))
;; credit to https://github.com/zaeph/.emacs.d/blob/4548c34d1965f4732d5df1f56134dc36b58f6577/init.el
(defun ram-org-set-last-modified-in-save-hook ()
  (when (derived-mode-p 'org-mode)
    (zp/org-set-last-modified)))
(add-hook 'before-save-hook #'ram-org-set-last-modified-in-save-hook)

;;** org-mode: syntax-table

(defun ram-org-mode-syntax-table-update ()
  "Update `org-mode-syntax-table'."
  ;; the default is word constituent, breaks expansion
  (modify-syntax-entry ?' "'" org-mode-syntax-table)
  ;; the default is symbol constituent, breaks expansion
  (modify-syntax-entry ?~ "'" org-mode-syntax-table))

(add-hook 'org-mode-hook #'ram-org-mode-syntax-table-update)

;;** org-mode: faces, fonts

;; (setq org-src-block-faces
;;       '(("clojure" (:family "Operator Mono Medium" :weight 'normal :background "#EEFFEE"))))

;; !!! Including :weight property would reset
;; font-lock-string-face and font-lock-doc-face that are set to light.
(with-eval-after-load "org"
  (set-face-attribute
   'org-block nil
   :family "Operator Mono Medium"))

;;** org-mode: emphasis

;; modify org-emphasis-regexp-components, 3rd entry, to include char to emphasis markup
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
(with-eval-after-load "org"
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\""))

;; (with-eval-after-load
;;     (add-to-list 'org-emphasis-alist '("/" (:background "green"))))
(setq org-emphasis-alist
  '(("*" (bold :foreground "grey60"))
    ("/" (:family "Operator Mono Light" :slant italic))
    ("_" underline)
    ("=" org-verbatim verbatim)
    ("~" org-code verbatim)
    ("+" (:strike-through t))))

(setq org-hide-emphasis-markers t)
;; setting this to nil "unhides" the emphasis markers
;; (setq org-descriptive-links nil)
(setq org-src-window-setup 'current-window)
(setq org-hide-leading-stars nil)
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-return-follows-link t)
;; do not indent code in code-blocks
(setq org-edit-src-content-indentation 0
      org-src-tab-acts-natively t
      org-src-preserve-indentation t)

(setq org-imenu-depth 7)

;; credit to https://stackoverflow.com/a/7165419/9913235
(font-lock-add-keywords 'org-mode '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)))

;; this will affect derived modes too.
;; (defun add-quotes-to-font-lock-keywords ()
;;   (font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face))))

;; (add-hook 'text-mode-hook 'add-quotes-to-font-lock-keywords)

;;** org-mode: structure-templates

;; https://orgmode.org/manual/Structure-Templates.html

;; (eval-after-load "org"
;;   '(progn (add-to-list 'org-structure-template-alist
;;                        '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
;;           (add-to-list 'org-structure-template-alist
;;                        '("cl" "#+BEGIN_SRC clojure\n?\n#+END_SRC"))
;;           (add-to-list 'org-structure-template-alist
;;                        '("cls" "#+BEGIN_SRC clojurescript\n?\n#+END_SRC"))
;;           (add-to-list 'org-structure-template-alist
;;                        '("qt" "#+BEGIN_QUOTE\n?\n#+END_QUOTE"))
;;           (add-to-list 'org-structure-template-alist
;;                        '("rac" "#+BEGIN_SRC racket :lang racket/base :results output \n?\n#+END_SRC"))
;;           (add-to-list 'org-structure-template-alist
;;                        '("n" "#+NAME: ?"))
;;           (add-to-list 'org-structure-template-alist
;;                        '("he" "#+HEADER: ?"))))

;; For example, type "<el" and press TAB key to expand
(with-eval-after-load "org"
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
               '("cl" . "src clojure"))
  (add-to-list 'org-structure-template-alist
               '("cls" . "src clojurescrsipt"))
  (add-to-list 'org-structure-template-alist
               '("rac" . "src racket :lang racket/base :results output"))
  (add-to-list 'org-structure-template-alist
               '("sh" . "src shell"))
  (add-to-list 'org-structure-template-alist
               '("n" . "name"))
  (add-to-list 'org-structure-template-alist
               '("ex" . "example"))
  (add-to-list 'org-structure-template-alist
               '("hd" . "header")))

;;** org-mode: org-babel

(straight-use-package
 '(org-babel-eval-in-repl :type git :flavor melpa :host github :repo "diadochos/org-babel-eval-in-repl"))

(with-eval-after-load "org"
  (autoload 'ob-racket "ob-racket")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (js . t)
     (emacs-lisp . t)
     (clojure . t)
     (python . t)
     (racket . t)
     (prolog . t)
     ;; (scribble . t)
     (css . t))))

;;*** org-mode/org-babel: org-babel-eval-in-repl

(with-eval-after-load "ob"
  ;; (require 'eval-in-repl-racket)
  (require 'org-babel-eval-in-repl)
  (add-to-list 'ober-org-babel-type-list '("prolog" . (ediprolog ediprolog-dwim)))
  (add-to-list 'ober-org-babel-type-list '("racket" . (eval-in-repl-racket eir-eval-in-racket)))
  (define-key ram-leader-map-tap-org (kbd "e") #'ober-eval-block-in-repl)
  (define-key ram-leader-map-tap-org (kbd "E") #'ober-eval-in-repl))


(with-eval-after-load "org-babel-eval-in-repl"
  (setq eir-jump-after-eval nil))

;; ;;* git-gutter
;; ;; https://github.com/syohex/emacs-git-gutter
;; ;; https://github.com/nschum/fringe-helper.el
;; ;; https://github.com/syohex/emacs-git-gutter-fringe
;; (use-package git-gutter-fringe
;;   :defer 0.1
;;   :load-path "site-lisp/emacs-git-gutter-fringe"
;;   :diminish git-gutter
;;   :config
;;   (global-git-gutter-mode t)
;;   ;; (remove-hook 'git-gutter:update-hooks 'magit-refresh-buffer-hook)
;;   ;; (advice-add #'select-window :after (lambda () (git-gutter t)))
;;   ;;(git-gutter:linum-setup)
;;   (custom-set-variables
;;    '(git-gutter:update-interval 1)
;;    ;; don't ask y/n before staging/reverting
;;    '(git-gutter:ask-p nil)
;;    ;; don't log/message
;;    '(git-gutter:verbosity 0)
;;    ;; count unstaged hunks in the current buffer
;;    '(git-gutter:buffer-hunks 1)
;;    '(git-gutter:statistic 1)))


;;** org-mode:orgit

;; Link to Magit buffers from Org documents
;; https://github.com/magit/orgit
(straight-use-package
 '(orgit :type git :flavor melpa :host github :repo "magit/orgit"))
;;* org-agenda

;;** org-agenda: bindings

(define-key global-map (kbd "s-a") #'org-agenda)

;;** org-agenda: functions

;; credit to https://d12frosted.io/posts/2020-06-24-task-management-with-roam-vol2.html
(defun ram-make-org-doc-category-identifier (&optional len)
  "Create a string 'category' identifier for an org buffer.

Make the identifier base on either:

- TITLE property
- CATEGORY property
- filename

This function is created to identify TODO items in agenda buffers.
"
  (let* ((file-name (when buffer-file-name
                      (file-name-sans-extension
                       (file-name-nondirectory buffer-file-name))))
         (title (vulpea-buffer-prop-get "title"))
         (category (org-get-category))
         (result (or title category file-name "")))
    (if (numberp len)
        (s-truncate len (s-pad-right len " " result))
      result)))

(defun vulpea-buffer-prop-get (name)
  "Get a buffer property called NAME as a string."
  (org-with-point-at 1
    (when (re-search-forward (concat "^#\\+" name ": \\(.*\\)")
                             (point-max) t)
      (buffer-substring-no-properties
       (match-beginning 1)
       (match-end 1)))))

(defun ram-update-agenda-files (&rest _)
  "Update the value of `org-agenda-files'."
  (message "Updating org-agenda-files ...")
  (let ((inhibit-message nil)
        (message-log-max 1000))
    (setq org-agenda-files (seq-uniq (append (vulpea-project-files) (ram-daily-tagged-poject-files))))
    nil)
  (message "... org-agenda-files updated.")
  nil)

(defun ram-agenda-files-add (file-path)
  "Add FILE-PATH to `org-agenda-files'."
  (cl-pushnew file-path org-agenda-files :test #'string=))

(defun ram-agenda-files-remove (file-path)
  "Remove FILE-PATH from `org-agenda-files'."
  (setq org-agenda-files (remove file-path org-agenda-files)))

;;** org-agenda: settings

(setq org-agenda-sticky t)
(setq org-agenda-prefix-format
      '((agenda . " %i %(ram-make-org-doc-category-identifier 16)%?-12t% s")
        (todo . " %i %(ram-make-org-doc-category-identifier 16) ")
        (tags . " %i %(ram-make-org-doc-category-identifier 16) ")
        (search . " %i %(ram-make-org-doc-category-identifier 16) ")))

(setq org-agenda-restore-windows-after-quit t)

;;** org-agenda: hooks, advice, timers

;; (setq org-agenda-files "~/backup/org/org-roam/notes/")
;; (setq org-agenda-files '("~/backup/org/org-roam/notes/" "~/backup/org/org-roam/daily/"))

(advice-add 'org-agenda :before #'ram-update-agenda-files)
;; (advice-remove 'org-agenda #'ram-update-agenda-files)

;; (setq org-agenda-files '("~/backup/org/org-roam/notes/20211002191735-org_roam_category.org"))

;;* org-roam

;;** org-roam: bindings

(define-key global-map (kbd "C-c n f") #'org-roam-node-find)
(define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
(define-key global-map (kbd "C-c n l") #'org-roam-buffer-toggle)
(define-key global-map (kbd "C-c n g") #'org-roam-graph)
(define-key global-map (kbd "s-t") #'org-roam-node-find)
(define-key global-map (kbd "s-T") #'org-roam-node-insert)
(define-key ram-leader-map-tap-global (kbd "/") #'org-roam-node-find)
(define-key ram-leader-map-tap-global (kbd "'") #'org-roam-node-insert)

;;** org-roam: functions

(defun ram-buffer-is-from-roam-directory-p ()
  "Return non-nil if the currently visited buffer is from `org-roam-directory'."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-directory))
        (file-name-directory buffer-file-name))))

;; credit to https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html
(defun vulpea-project-files ()
  "Return a list of note files containing 'project' tag." ;
  (require 'org-roam-db)
  (seq-uniq
   (seq-map
    #'car
    (org-roam-db-query
     [:select [nodes:file]
      :from tags
      :left-join nodes
      :on (= tags:node-id nodes:id)
      :where (like tag (quote "%\"project\"%"))]))))

(defun ram-daily-tagged-poject-files ()
  "Return daily notes marked with 'project' tag.

Consider both org-roam notes and dailies."
  (seq-filter #'ram-org-buffer-contains-todos-p
              (directory-files
               (expand-file-name org-roam-dailies-directory org-roam-directory)
               'absolute-path
               "^[^\\(.#\\)].*org$")))

(defun ram-update-org-roam-tag-if-contains-todos (&optional file-path)
  "Add or remove PROJECT tag if buffer contains todos."
  (require 'org-roam)
  (when (and (not (active-minibuffer-window))
             (org-roam-file-p)
             (save-excursion
               (goto-char (point-min))
               (org-roam-db-node-p)))

    (setq file-path (or file-path (buffer-file-name (buffer-base-buffer))))

    ;; check if file was modified (e.g., to-do is added)
    ;; by comparing file hashes.
    ;; nice idea, but not familiar with org-roam-db-autosync-mode internals yet, so
    ;; leave it out for now.
    ;; (let ((content-hash (org-roam-db--file-hash file-path))
    ;;       (db-hash (caar (org-roam-db-query [:select hash :from files
    ;;                                                  :where (= file $s1)] file-path))))
    ;;   (unless (string= content-hash db-hash)))

    (save-excursion
      (goto-char (point-min))
      (if (ram-org-buffer-contains-todos-p)
          (progn
            (org-roam-tag-add '("project"))
            (ram-agenda-files-add file-path))
        (condition-case err
            (org-roam-tag-remove '("project"))
          (user-error (when (not (string= (error-message-string err)
                                          "No tag to remove"))
                        (signal (car err) (cdr err))))
          (error (signal (car err) (cdr err)))
          (:success
           (ram-agenda-files-remove file-path)))))

    (when (buffer-modified-p)
      (ram-eval-exp-with-modified-hooks (save-buffer)
                                        ((before-save-hook nil)
                                         (after-save-hook nil))))

    ;; (save-excursion
    ;;   (goto-char (point-min))
    ;;   (let* ((tags (let ((value (vulpea-buffer-prop-get "FILETAGS")))
    ;;                  (when (and value
    ;;                             (not (string-empty-p value)))
    ;;                    (split-string-and-unquote value))))
    ;;          (original-tags tags))
    ;;     (if (ram-org-buffer-contains-todos-p)
    ;;         (setq tags (cons "project" tags))
    ;;       (setq tags (remove "project"
    ;;                          ;; cleanup duplicates
    ;;                          (setq tags (seq-uniq tags)))))
    ;;     ;; update tags if changed
    ;;     (when (or (seq-difference tags original-tags)
    ;;               (seq-difference original-tags tags))
    ;;       (vulpea-buffer-prop-set "FILETAGS" (string-join tags " ")))))
    ))

(defun vulpea-buffer-prop-set (name value)
  "Set a file property called NAME to VALUE in buffer file.
If the property is already set, replace its value."
  (setq name (downcase name))
  (org-with-point-at 1
    (let ((case-fold-search t))
      (if (re-search-forward (concat "^#\\+" name ":\\(.*\\)")
                             (point-max) t)
          (replace-match (concat "#+" name ": " value) 'fixedcase)
        (while (and (not (eobp))
                    (looking-at "^[#:]"))
          (if (save-excursion (end-of-line) (eobp))
              (progn
                (end-of-line)
                (insert "\n"))
            (forward-line)
            (beginning-of-line)))
        (insert "#+" name ": " value "\n")))))

;;
;; run only once after starting Emacs. it will update the note tag to
;; reflect if it contains TODO items.

;; credit to https://d12frosted.io/posts/2021-01-16-task-management-with-roam-vol5.html

(defun ram-update-all-org-roam-files-for-todo-items ()
  "Run `ram-update-org-roam-tag-if-contains-todos' for all files."
  (interactive)
  (require 'org-roam)
  (message "Updating all org-roam notes and dailies for todos ...")
  (let ((time (current-time)))
    (let ((inhibit-message nil)
          (message-log-max 1000))
      (dolist (file (seq-uniq (append
                               ;; notes
                               (org-roam-list-files)
                               ;; (directory-files
                               ;;  (expand-file-name org-roam-directory)
                               ;;  'absolute-path
                               ;;  "^[^\\(.#\\)].*org$")
                               ;; (.#) excludes lock files (info "emacs#Interlocking")
                               ;; dailies
                               (directory-files
                                (expand-file-name org-roam-dailies-directory org-roam-directory)
                                'absolute-path
                                "^[^\\(.#\\)].*org$"))))
        (let ((buffer (find-buffer-visiting file)))
          (if buffer
              (with-current-buffer buffer
                (ram-update-org-roam-tag-if-contains-todos))
            (let ((buffer
                   (ram-eval-exp-with-modified-hooks
                    (find-file-noselect file)
                    ((find-file-hook nil)
                     (org-mode-hook nil)))))
              (with-current-buffer buffer
                (ram-update-org-roam-tag-if-contains-todos))
              (ram-eval-exp-with-modified-hooks
               (kill-buffer buffer)
               ((before-save-hook nil)
                (after-save-hook nil)))))))
      nil)
    (message "finished updating all org-roam notes and dailies in %.01f sec" (float-time (time-since time)))))

;;** org-roam: hooks, advice, timers

;; !!! 'org-capture-before-finalize-hook is set to nil in org-capture-kill
;; (add-hook 'org-capture-before-finalize-hook #'ram-remove-face-remapping)
(add-hook 'org-capture-prepare-finalize-hook #'ram-remove-face-remapping)

;; !!! why not use hooks specific to org or org-roam buffers
(add-hook 'find-file-hook #'ram-update-org-roam-tag-if-contains-todos)
(add-hook 'before-save-hook #'ram-update-org-roam-tag-if-contains-todos)

;; it fires even when you open a daily note (or yesterday)
;; (add-hook 'org-roam-capture-new-node-hook #'ram-set-face-remapping-alist-capture-new-node)

(add-hook 'org-roam-find-file-hook #'ram-remap-hl-line-face-in-find-file-hook)

(add-hook 'org-capture-mode-hook #'ram-add-remap-face-to-hl-line-in-capture-hook)

;;** org-roam: capture-templates

(with-eval-after-load "org-roam"
  (setq org-roam-capture-templates
        '(("d" "default"
           entry "* ${title}\n\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n")
           :unnarrowed t))))


;;** org-roam: init

(setq org-roam-v2-ack t)
(setq org-roam-directory (file-truename "~/backup/org/org-roam/notes/"))

;;** org-roam: installation

(straight-use-package
 '(org-roam :type git :flavor melpa :host github :repo "org-roam/org-roam"))
;(require 'org-roam)

;;** org-roam: deft, search notes with regexp

;; https://github.com/jrblevin/deft
;; deft is for quickly browsing, filtering, and editing directories of notes.

(straight-use-package
 '(deft :type git :flavor melpa :host github :repo "jrblevin/deft"))

;;*** org-roam/deft: settings

(setq deft-extensions '("org"))
(with-eval-after-load "deft"
  (setq deft-directory org-roam-directory))
;; (setq deft-ignore-file-regexp "your regex here")
;; (setq deft-recursive t)
(setq deft-recursive nil)
;; (setq deft-recursive-ignore-dir-regexp "daily")
;; (setq deft-strip-title-regexp "your regex here")
;; 0 disables auto save feature
;; when saving, ram-update-org-roam-tag-if-contains-todos is called by hook,
;; it calls org-roam-tag-remove which raises (wrong-type-argument integer-or-marker-p nil)
(setq deft-auto-save-interval 0)
;; (setq deft-parse-title-function 'your-title-parsing-function)
(setq deft-use-filename-as-title t)
;; (setq deft-strip-summary-regexp "your regex here")
(setq deft-current-sort-method 'title)
;; nil would make regexp search
;; (setq deft-incremental-search t)

;;*** org-roam/deft: functions

;; (when (not (boundp 'org-list-full-item-re))
;; (require 'org))

(defvar org-list-full-item-re)

(defun ram-get-deft-strip-summary-regexp-for-notes ()
  "Return a regexp to exclude from note content."
  (concat "\\(?:"
          "[[:space:]]+"                ; blank
          "\\|"
          "^:PROPERTIES:.*$"
          "\\|"
          "^:ID:.*$"
          "\\|"
          "^:END:.*$"
          "\\|"
          "^#\\+[^[:blank:]]+:.*$"      ; org-mode metadata
          "\\|"
          "^- tags $"                   ; tags
          "\\|"
          "^-source .*$"                ; source
          "\\|"
          "\\[\\[\\(?:\\(id\\)\\|\\(info\\)\\|\\(file\\)\\)[^]]+\\]" ; links
          "\\|"
          "\\][^]]"                     ; ']' from link
          "\\|"
          "\\(?:^\\*\\*+[[:blank:]]+.*$\\)" ; any headline that is not top level
          "\\|"
          "\\(?:^[^*]\\{1,\\}.*$\\)"   ; anything that is not headline
          "\\|"
          (regexp-quote org-list-full-item-re) ; org list items
          "\\)"))

(setq deft-file-limit 65)

(defun ram-deft-search-org-roam-notes ()
  "Invoke `deft' with settings for `org-roam' notes."
  (interactive)
  ;; avoid (error "Defining as dynamic an already lexical var") with #'require
  (require 'deft)
  ;; Lisp error: (error "No buffer named *Deft*")
  (condition-case err
      (kill-buffer "*Deft*")
    (error (if (string= (error-message-string err)
                        "No buffer named *Deft*")
               (message "No *Deft* buffer to kill")
             (signal (car err) (cdr err))))
    (:success
     (message "Killed org-roam dailies *Deft* buffer")))
  (let ((deft-directory (expand-file-name org-roam-directory))
        ;; (deft-strip-summary-regexp (ram-get-deft-strip-summary-regexp-for-notes))
        (deft-current-sort-method 'title)
        ;; (deft-recursive t)
        ;; (deft-recursive-ignore-dir-regexp "\\(?:\\.\\|\\.\\.\\)$\\|\\(?:/daily\\)")
        (title-match "second_brain"))
    (cl-letf* (;; (list title-match)
               ((symbol-function 'deft-parse-title)
                (lambda (file contents)
                  (let (
                        ;; first headline
                        ;; (begin (string-match "^\\*[[:blank:]]+\\(.*\\)$" contents))
                        ;; title
                        (begin (string-match "^#\\+TITLE: \\(.*\\)$" contents)))
                    (if begin
                        (setq title-match (substring contents (match-beginning 1) (match-end 1)))
                      (setq title-match (replace-in-string (deft-base-filename file) "^[0-9]\\{14\\}-" ""))))))

               ((symbol-function 'deft-parse-summary)
                (lambda (contets title)
                  "Remove any line that contains `org-roam' note \"title\"."
                  (let ((summary (let* ((case-fold-search nil)
                                        ;; (begin (string-match "^#\\+TITLE: \\(.*\\)$" contents))
                                        (title-regex (when (string-match "^#\\+TITLE: \\(.*\\)$" contents)
                                                       (format "^.*%s.*$"
                                                               (regexp-quote
                                                                (string-replace "_" " "
                                                                                (substring contents
                                                                                           (match-beginning 1)
                                                                                           (match-end 1)))))))
                                        (strip-summary-regex (if title-regex
                                                                 (concat (ram-get-deft-strip-summary-regexp-for-notes)
                                                                         "\\|\\(?:" title-regex "\\)")
                                                               (ram-get-deft-strip-summary-regexp-for-notes))))
                                   (replace-regexp-in-string strip-summary-regex " " contents))))
                    (deft-chomp summary)))))
      (deft))))

(defun ram-deft-search-daily-notes ()
  "Invoke `deft' after setting `deft-directory' to `org-roam-dailies-directory'"
  (interactive)
  ;; avoid (error "Defining as dynamic an already lexical var") with #'require
  (require 'deft)
  ;; Lisp error: (error "No buffer named *Deft*")
  (condition-case err
      (kill-buffer "*Deft*")
    (error (if (string= (error-message-string err)
                        "No buffer named *Deft*")
               (message "No *Deft* buffer to kill")
             (signal (car err) (cdr err))))
    (:success
     (message "Killed org-roam dailies *Deft* buffer")))
  (cl-letf ((deft-directory (expand-file-name org-roam-dailies-directory org-roam-directory))
            (deft-strip-summary-regexp (ram-get-deft-strip-summary-regexp-for-notes))
            ((symbol-function 'deft-sort-files)
             (lambda (files)
               (sort files (lambda (f1 f2)
                             (let ((t1 (deft-file-title f1))
                                   (t2 (deft-file-title f2)))
                               (string-greaterp
                                (and t1 (downcase t1))
                                (and t2 (downcase t2)))))))))
    (deft)))

;;*** org-roam/deft: bindings

(define-key global-map (kbd "s-c S") #'ram-deft-search-org-roam-notes)
(define-key global-map (kbd "s-c s") #'ram-deft-search-daily-notes)

;;** org-roam: settings

(with-eval-after-load "org-roam"
  (org-roam-db-autosync-mode))

;;** org-roam: dailies

;;*** org-roam/dailies: functions

(defun ram-buffer-is-from-roam-dailies-directory-p ()
  "Return non-nil if the currently visited buffer is from `org-roam-dailies-directory'."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory org-roam-dailies-directory))
        (file-name-directory buffer-file-name))))

;;*** org-roam/dailies: settings

;; relative to org-roam-directory
(setq org-roam-dailies-directory "./daily/")

;;*** org-roam/dailies: capture templates

(defun ram-org-get-heading ()
  "Return the current heading and links in its section element."
  (let* ((current-headline (save-excursion
                             (condition-case nil
                                 (outline-back-to-heading)
                               (error
                                (user-error "Before first headline at position %d in buffer %s"
		                            (point) (current-buffer)))
                               (:success (org-element-at-point)))))
         ;; the heading is inserted as level one anyway
         ;; (current-headline (org-element-put-property (org-element-copy (org-element-at-point)) :level 1))
         (headline-element (car (org-element-map
                                    (org-element-parse-buffer)
                                    'headline
                                  (lambda (headline)
                                    (when (string= (org-element-property :raw-value headline)
                                                   (org-element-property :raw-value current-headline))
                                      headline)))))
         (section-element (org-element-map
                              headline-element
                              'section #'identity 'first-match 'no-recursive-edit))
         (all-links (reverse (org-element-map
                                 section-element
                                 'link
                               ;; make lisp smaller by removing :parent value
                               (lambda (link) (org-element-put-property link :parent nil)))))
         all-links-str
         ;; used only to split links to 'fill-column size
         all-description-str)
    (while all-links
      (let* ((link (pop all-links))
             (link-str (org-element-interpret-data link))
             (link-description (or (car (org-element-contents link))
                                   (org-element-property :raw-link link)))
             (link-separator (let ((separator (if all-links-str " " "")))
                               (if (and all-description-str
                                        (> (+ (length (car (last (split-string all-description-str "\n" 'OMIT-NULLS))))
                                              (length link-description))
                                           fill-column))
                                   "\n"
                                 separator)))
             )
        (setq all-links-str (concat link-str link-separator all-links-str))
        (setq all-description-str (concat link-description link-separator all-description-str))))
    (concat (org-element-interpret-data current-headline)
            all-links-str)))


(defun ram-org-capture-heading-to-dailies (&optional arg)
  "Capture the current headline into a `org-roam' daily note.
Insert into daily note for ARG days from now. Or use calendar if
ARG value is 4."
  (interactive "P")
  (let* ((templates
          `(("d" "continue task under heading"
             entry ,(ram-org-get-heading)
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %U")
             :empty-lines-before 1
             :empty-lines-after 1
             :unnarrowed t
             :kill-buffer t
             :immediate-finish nil
             :no-save nil)))
         (time (if (equal arg '(4))
                   (let ((org-read-date-prefer-future t))
                     (org-read-date nil 'TO-TIME nil "Capture to daily-note: " ))
                 (time-add (* (or arg 0) 86400) (current-time)))))
    (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
      (org-roam-capture- :goto nil
                         :node (org-roam-node-create)
                         :props (list :override-default-time time)
                         :templates templates))))

(defun ram-org-element-get-title (&optional parsed-buffer)
  "Return the document title."
  (let ((buf (or parsed-buffer
                 (org-element-parse-buffer))))
    (org-element-map buf 'keyword
      (lambda (kw)
        (when (string= (org-element-property :key kw) "TITLE")
          (org-element-property :value kw)))
      :first-match t)))

(defun ram-org-capture-title-to-dailies (&optional arg)
  "Capture the document title into a `org-roam' daily note.
Insert into daily note for ARG days from now. Or use calendar if
ARG value is 4."
  (interactive "P")
  (let* ((parsed-buffer (org-element-parse-buffer))
         (doc-title (ram-org-element-get-title parsed-buffer))
         (backlink (org-element-map
                       parsed-buffer 'node-property
                     (lambda (prop)
                       (when (string= (org-element-property :key prop) "ID")
                         (concat "[[id:" (org-element-property :value prop) "][" doc-title "]]")))
                     'first-match t))
         (templates
          `(("t" "capture document title"
             entry ,(concat "* " doc-title " %(org-set-tags \":read:\")  " "\n" backlink "\n\n%?")
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %U")
             :empty-lines-before 1
             :empty-lines-after 1
             :unnarrowed t
             :kill-buffer t
             :immediate-finish nil
             :no-save nil)))
         (time (if (equal arg '(4))
                   (let ((org-read-date-prefer-future t))
                     (org-read-date nil 'TO-TIME nil "Capture to daily-note: " ))
                 (time-add (* (or arg 0) 86400) (current-time)))))
    (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
      (org-roam-capture- :goto nil
                         :node (org-roam-node-create)
                         :props (list :override-default-time time)
                         :templates templates))
    (org-align-tags)))

(defun ram-org-capture-defun-to-dailies (&optional arg)
  "Capture the defun into a `org-roam' daily note.
Insert into daily note for ARG days from now. Or use calendar if
ARG value is 4."
  (interactive "P")
  (let* ((defun-name (save-excursion
                       (when (not (= (char-before) ?\n))
                         (beginning-of-defun))
                       (down-list)
                       (forward-symbol 2)
                       (let ((bounds (bounds-of-thing-at-point 'sexp)))
                         (buffer-substring-no-properties (car bounds) (cdr bounds)))))
         (backlink (format "[[file:%s::%s][%s]]" (buffer-file-name) defun-name defun-name))
         (templates
          `(("t" "capture document title"
             entry ,(concat "* " defun-name  " %(org-set-tags \":"
                            (car (split-string (symbol-name major-mode) "-"))
                            ":\")\n" backlink "\n\n%?")
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %U")
             :empty-lines-before 1
             :empty-lines-after 1
             :unnarrowed t
             :kill-buffer t
             :immediate-finish nil
             :no-save nil)))
         (time (if (equal arg '(4))
                   (let ((org-read-date-prefer-future t))
                     (org-read-date nil 'TO-TIME nil "Capture to daily-note: " ))
                 (time-add (* (or arg 0) 86400) (current-time)))))
    (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
      (org-roam-capture- :goto nil
                         :node (org-roam-node-create)
                         :props (list :override-default-time time)
                         :templates templates))
    (org-align-tags)))

(defun ram-org-capture-magit-commit-to-dailies (&optional arg)
  "Capture magit commit into a `org-roam' daily note.
Insert into daily note for ARG days from now. Or use calendar if
ARG value is 4."
  (interactive "P")
  (let* ((repo (abbreviate-file-name default-directory))
         (rev (if (eq major-mode 'magit-status-mode)
                  (magit-copy-section-value nil)
                (magit-git-string "rev-parse" "HEAD")))
         (backlink (format "[[orgit-rev:%s::%s][%s]]" repo rev (substring rev 0 7)))
         (summary (substring-no-properties (magit-format-rev-summary rev)))
         (templates
          `(("t" "capture document title"
             entry, (format "* %s %%(org-set-tags \":git:\")\n%s\n\n%%?"
                            summary backlink)
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %U")
             :empty-lines-before 1
             :empty-lines-after 1
             :unnarrowed t
             :kill-buffer t
             :immediate-finish nil
             :no-save nil)))
         (time (if (equal arg '(4))
                   (let ((org-read-date-prefer-future t))
                     (org-read-date nil 'TO-TIME nil "Capture to daily-note: " ))
                 (time-add (* (or arg 0) 86400) (current-time)))))
    (let ((org-roam-directory (expand-file-name org-roam-dailies-directory org-roam-directory)))
      (org-roam-capture- :goto nil
                         :node (org-roam-node-create)
                         :props (list :override-default-time time)
                         :templates templates))
    (org-align-tags)))

(with-eval-after-load "org-roam-dailies"
  ;; (setq time-stamp-format "[%Y-%02m-%02d %3a %02H:%02M]")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" plain ""
           :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n")
           :empty-lines-before 1
           :empty-lines-after 1
           :unnarrowed t
           :kill-buffer nil
           :no-save nil))))

;;*** org-roam/dailies: bindings

;; these command are ###autoload and 'org-roam-dailies-map is not
;; (with-eval-after-load "org-roam-dailies"
;;   (define-key global-map (kbd "s-c") org-roam-dailies-map))
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "s-c a") #'ram-org-capture-heading-to-dailies)
  (define-key org-mode-map (kbd "s-c A") #'ram-org-capture-title-to-dailies))

(define-key emacs-lisp-mode-map (kbd "s-c a") #'ram-org-capture-defun-to-dailies)

(define-key global-map (kbd "s-c g") #'ram-org-capture-magit-commit-to-dailies)

(define-key global-map (kbd "s-c n") #'org-roam-dailies-capture-today)
(define-key global-map (kbd "s-c d") #'org-roam-dailies-goto-today)
(define-key global-map (kbd "s-c f") #'org-roam-dailies-goto-next-note)
(define-key global-map (kbd "s-c b") #'org-roam-dailies-goto-previous-note)
(define-key global-map (kbd "s-c c") #'org-roam-dailies-goto-date)
(define-key global-map (kbd "s-c C") #'org-roam-dailies-capture-date)
(define-key global-map (kbd "s-c t") #'org-roam-dailies-goto-tomorrow)
(define-key global-map (kbd "s-c T") #'org-roam-dailies-capture-tomorrow)
(define-key global-map (kbd "s-c y") #'org-roam-dailies-goto-yesterday)
(define-key global-map (kbd "s-c Y") #'org-roam-dailies-capture-yesterday)

;;** org-roam/dailies: hooks, advice, timers

;; (add-hook 'find-file-hook #'ram-set-face-remapping-alist)
;; (remove-hook 'find-file-hook #'ram-set-face-remapping-alist)

;; (add-hook 'org-roam-dailies-find-file-hook #'ram-set-face-remapping-alist)
;; (remove-hook 'org-roam-dailies-find-file-hook #'ram-set-face-remapping-alist)

;;** org-roam: weeklies

;;** org-roam/ram-weeklies: settings

(setq ram-org-roam-weeklies-directory "./weekly/")
(defun ram-org-roam-weeklies--capture (time &optional goto)
  "Capture an entry in a weekly-note for TIME, creating it if necessary.
When GOTO is non-nil, go to the note without creating an entry."
  (let ((org-roam-directory (expand-file-name ram-org-roam-weeklies-directory org-roam-directory)))
    (org-roam-capture- :goto (when goto '(4))
                       :node (org-roam-node-create)
                       :templates ram-org-roam-weeklies-capture-templates
                       :props (list :override-default-time time))
    ;; (when goto (run-hooks 'ram-org-roam-dailies-find-file-hook))
    ))

(setq ram-org-roam-weeklies-capture-templates
      '(("d" "default" entry "* %?"
         :target (file+head "%<%Y-%U>.org" "#+TITLE: %<%Y-%U>\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n\n"))))


;;* outline, headings, headlines

;;** outline: setup

(outline-minor-mode t)

(with-eval-after-load "outline"
  (require 'foldout))

;;** outline: bicycle
(straight-use-package
 '(bicycle :type git :flavor melpa :host github :repo "tarsius/bicycle"))

;; based on, credit to https://gitlab.com/protesilaos/dotfiles.git

;;** outline: functions

(defun ram-outline-hide-all ()
    "Hide all `outline-mode' subtrees."
    (interactive)
    (outline-map-region 'outline-hide-subtree (point-min) (point-max))
    (move-beginning-of-line 1))

(defun ram-toggle-narrow-outline-heading (arg)
  "Toggle narrow and widen from anywhere in subtree."
  (interactive "p")
  (if (buffer-narrowed-p)
      (progn
        (widen)
        (recenter 0))
    ;; (foldout-exit-fold arg)
    (when (not (outline-on-heading-p))
      (outline-next-visible-heading (- 1)))
    (foldout-zoom-subtree)))

(defun prot/bicycle-cycle-tab-dwim ()
    "Convenience wrapper for TAB key in `outline-mode'."
    (interactive)
    (if (outline-on-heading-p)
        (bicycle-cycle)
      (indent-for-tab-command)))

(defun prot/outline-down-heading ()
    "Move to the next `outline-mode' subtree."
    (interactive)
    ;; Hacky, but it kinda works.
    (outline-up-heading 1 t)
    (outline-forward-same-level 1))

(defun ram-outline-up-heading (arg &optional invisible-ok)
  "Call `outline-up-heading' and ignore the error.

Ignore \"Already at top level of the outline\" error, call
`outline-backward-same-level' instead."
(interactive "p")
(condition-case err
    (outline-up-heading 1 t)
  (error (if (string= (error-message-string err)
                      "Already at top level of the outline")
             (outline-backward-same-level arg)
           (signal (car err) (cdr err))))))

(defun ram-outline-forward-same-level (arg &optional invisible-ok)
  "Call `outline-forward-same-level' and ignore the error.

Ignore \"No following same-level heading\" error, call
`prot/outline-down-heading' instead."
  (interactive "p")
  (condition-case err
      (outline-forward-same-level arg)
    (error (if (string= (error-message-string err)
                        "No following same-level heading")
               (prot/outline-down-heading)
             (signal (car err) (cdr err))))))

;;** outline: bindings

(with-eval-after-load "outline"
  (define-key outline-minor-mode-map (kbd "<tab>") #'bicycle-cycle))

(define-key ram-leader-map-tap-global (kbd "n") #'outline-next-visible-heading)
(define-key ram-leader-map-tap-global (kbd "p") #'outline-previous-visible-heading)
(define-key ram-leader-map-tap-global (kbd "f") #'ram-outline-forward-same-level)
(define-key ram-leader-map-tap-global (kbd "b") #'outline-backward-same-level)
(define-key ram-leader-map-tap-global (kbd "o") #'outline-show-all)
(define-key ram-leader-map-tap-global (kbd "q") #'ram-outline-hide-all)
(define-key ram-leader-map-tap-global (kbd "u") #'ram-outline-up-heading)
(define-key ram-leader-map-tap-global (kbd "d") #'prot/outline-down-heading)
(define-key ram-leader-map-tap-global (kbd "z") #'ram-toggle-narrow-outline-heading)

;;** outline: hooks, advice, timers

(add-hook 'emacs-lisp-mode-hook (lambda ()
                                  (setq-local outline-regexp "^;;\\(?:;+[^#]\\|\\*+\\)")
                                  (outline-minor-mode 1)))
(add-hook 'clojure-mode-hook (lambda ()
                                  (setq-local outline-regexp "^;;\\(?:;+[^#]\\|\\*+\\)")
                                  (outline-minor-mode 1)))

;;* hideshow

(add-hook 'prog-mode-hook #'hs-minor-mode)

(with-eval-after-load "hideshow"
  (define-key hs-minor-mode-map (kbd "<C-tab>") #'hs-toggle-hiding))

;;* emacs-git-gutter-fringe

(straight-use-package
 '(git-gutter-fringe :type git :flavor melpa :host github :repo "syohex/emacs-git-gutter-fringe"))
(global-git-gutter-mode t)
(with-eval-after-load 'emacs-git-gutter-fringe
  (setq git-gutter:update-interval 1)
  ;; don't ask y/n before staging/reverting
  (setq git-gutter:ask-p nil)
  ;; don't log/message
  (setq git-gutter:verbosity 0)
  ;; count unstaged hunks in the current buffer
  (setq git-gutter:buffer-hunks 1)
  (setq git-gutter:statistic 1))

(add-to-list 'window-buffer-change-functions
             (lambda (frame)
               "Call `git-gutter' when buffer changes."
               (let ((buffer
                      (window-buffer (frame-selected-window frame))))
                 (with-current-buffer buffer
                   (when vc-mode
                     (git-gutter))))))


;;* hydra
(straight-use-package
 '(hydra :type git :flavor melpa :files (:defaults (:exclude "lv.el") "hydra-pkg.el") :host github :repo "abo-abo/hydra"))

;; https://github.com/abo-abo/hydra#foreign-keys

;;** hydra-window

(defhydra hydra-window (
                        :exit nil
                        :foreign-keys run)
  "window"
  ("h" windmove-left nil)
  ("l" windmove-right nil)
  ("k" windmove-up nil)
  ("j" windmove-down nil)
  ("v" (lambda ()
         (interactive)
         (split-window-right)
         (windmove-right))
   "vert")
  ("s" (lambda ()
         (interactive)
         (split-window-below)
         (windmove-down))
   "horiz")
  ;; ("t" transpose-frame "'" :exit nil)
  ("a" ace-window "ace")
  ("/" ace-swap-window "swap")
  ("d" ace-delete-window "del" :exit t)
  ;; ("b" helm-buffers-list "buf")
  ("b" ido-switch-buffer "buf" :exit t)
  ("f" counsel-find-file "file" :exit t)
  ;; headlong-bookmark-jump is from headlong package
  ;; ("m" headlong-bookmark-jump "bmk")
  ("m" bookmark-jump "bmk" :exit t)
  ("u" winner-undo "undo")
  ("c" delete-window "close")
  ("o" delete-other-windows "one")
  ("SPC" dired-jump "dired")
  ("<escape>" nil "quit" :exit t)
  ("q" nil "quit" :exit t))

;;** hydra-git-gutter

(defun ram-git-gutter-make-hunk-visible ()
  "Make whole hunk visible in window."
  (let ((point (point))
        (win-end (save-excursion (goto-char (window-end)) (previous-line 5) (point)))
        (hunk-end (save-excursion (git-gutter:end-of-hunk) (point))))
    (when (> hunk-end win-end)
      (recenter))))

(defhydra hydra-git-gutter (
                            :columns 4
                            :exit nil
                            :foreign-keys nil)
  "git-gutter"
  ("n" (lambda ()
         (interactive)
         (outline-show-all)
         (git-gutter:next-hunk 1)
         (ram-git-gutter-make-hunk-visible))
   "next")
  ("p" (lambda ()
         (interactive)
         (outline-show-all)
         (git-gutter:previous-hunk 1))
   "prev")
  ("f" (lambda ()
         (interactive)
         (goto-char (point-min))        ;move to first line
         (git-gutter:next-hunk 1)
         (ram-git-gutter-make-hunk-visible))
   "first")
  ("c" (lambda ()
         (interactive)
         (progn
           (save-buffer)
           (let ((diff-win (car (seq-filter
                                 (lambda (w) (equal "*git-gutter:diff*" (buffer-name (window-buffer w))))
                                 (window-list-1 nil 'nomini 'A)))))
             (when diff-win
               (quit-window nil diff-win)))
           (magit-status)))
   "commit" :exit t)
  ("N" git-gutter:end-of-hunk "end")
  ("m" git-gutter:mark-hunk "mark")
  ("d" git-gutter:popup-hunk "diff")
  ("r" git-gutter:revert-hunk "revert")
  ("s" git-gutter:stage-hunk "stage")
  ("SPC" nil "quit" :exit t)
  ("q" nil "quit" :exit t))

(define-key ram-leader-map-tap-global (kbd "h") 'hydra-git-gutter/body)
(define-key global-map (kbd "<f5>") 'hydra-git-gutter/body)

;;** hydra-multicursor

;; based on: https://github.com/abo-abo/hydra/wiki/multiple-cursors
(defhydra hydra-multiple-cursors
  (
   ;; https://stackoverflow.com/questions/53798055/hydra-disable-interpretation-of-prefix-argument
   ;; only C-u starts a universal prefix, 0..9, -, self-insert
   :base-map (make-sparse-keymap)
   :exit nil
   :foreign-keys nil
   :hint nil)
  ;; (mc/num-cursors) causes (void-function mc/num-cursors)
  ;; TODO: sort loading multiple-cursors to avoid the error
  ;; Next          Prev                 % 2(mc/num-cursors) selected
  "
   Next          Prev
------------------------------------------------------------------
   _n_:            _p_:
   _N_: skip       _P_: skip
 _M-n_: unmark   _M-p_: unmark
------------------------------------------------------------------
"
  ("n" mc/mark-next-like-this)
  ("N" mc/skip-to-next-like-this)
  ("M-n" mc/unmark-next-like-this)
  ("p" mc/mark-previous-like-this)
  ("P" mc/skip-to-previous-like-this)
  ("M-p" mc/unmark-previous-like-this)

  ("a" nil :exit t)
  ("q" nil :exit t))

(global-set-key (kbd "<f9>") 'hydra-multiple-cursors/body)

;;* cider
;; (setq package-check-signature nil)

(straight-use-package
 '(cider :type git :flavor melpa :files ("*.el" (:exclude ".dir-locals.el") "cider-pkg.el") :host github :repo "clojure-emacs/cider"))
(with-eval-after-load 'cider
  (add-hook 'clojure-mode-hook #'cider-mode)
  (cider-auto-test-mode 1) ;; run test on buffer load
  ;; (cider-fringe-good-face ((t (:foreground ,green-l))))
  (setq cider-save-file-on-load t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000)
  (setq cider-test-show-report-on-success nil)
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq cider-repl-display-help-banner nil)
  (face-spec-set
   'cider-fringe-good-face
   '((t :foreground "SkyBlue2"
        :width ultra-expanded
        :weight bold))
   'face-defface-spec))

;;** cider: repl

(with-eval-after-load "cider"
  (define-key cider-repl-mode-map (kbd "<f2>") #'cider-repl-return))

;;* racket

(straight-use-package
 '(racket-mode :type git :flavor melpa :files
               (:defaults "*.rkt"
                          ("racket" "racket/*")
                          (:exclude "racket/example/*" "racket/test/*")
                          "racket-mode-pkg.el")
               :host github :repo "greghendershott/racket-mode"))
(require 'racket-mode)

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)
  (define-key racket-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  (define-key racket-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun))

;; breaks lispy-mode key bindings
;; (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  (define-key racket-mode-map (kbd "<S-return>") 'newline-and-indent))

;;** racket: repl

(with-eval-after-load "racket-repl"
  (define-key racket-repl-mode-map (kbd "<f2>") #'racket-repl-submit))

;;* lispy
(straight-use-package 'lispy)

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

;; (add-hook 'lisp-interaction-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
;; (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
;; (add-hook 'racket-repl-mode-hook (lambda () (lispy-mode 1)))

;;** lispy settings

(with-eval-after-load "lispy"
  (setq lispy-avy-keys '(?s ?a ?r ?e ?t ?i ?u ?n ?o ?p ?l ?m ?f ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d))
  (setq lispy-eval-display-style 'overlay)
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
  ;; (setq lispy-avy-style-char 'at-full)
  (setq lispy-avy-style-char 'at-full)
  (setq lispy-avy-style-paren 'at-full)
  (setq lispy-avy-style-symbol 'at-full)

  (setq lispy-compat '(edebug cider))

  ;; do not recenter when navigating with #'lispy-ace-* commands
  (fset 'lispy--recenter-bounds (lambda (bnds) ())))

;;** lispy: fix outline hide/expand cycling

(defun my-org-show-children (&optional level)
  "Show all direct subheadings of this heading.
Prefix arg LEVEL is how many levels below the current level
should be shown.  Default is enough to cause the following
heading to appear."
  (interactive "p")
  ;; If `orgstruct-mode' is active, use the slower version.
  (if orgstruct-mode (call-interactively #'outline-show-children)
    (save-excursion
      (org-back-to-heading t)
      (let* ((current-level (funcall outline-level))
             (max-level (org-get-valid-level
                         current-level
                         (if level (prefix-numeric-value level) 1)))
             (end (save-excursion (org-end-of-subtree t t)))
                	     ;; (regexp-fmt "^\\*\\{%d,%s\\}\\(?: \\|$\\)")
                     ;; (regexp-fmt lispy-outline)
             (regexp-fmt "^;;\\(?:;[^#]\\|\\*+\\)\\|^\\*\\{%d,%s\\}\\(?: \\|$\\)")
             (past-first-child nil)
                	     ;; Make sure to skip inlinetasks.
             (re (format regexp-fmt
                  current-level
                  (cond
                         ((not (featurep 'org-inlinetask)) "")
                         (org-odd-levels-only (- (* 2 org-inlinetask-min-level)
                                               3))
                         (t (1- org-inlinetask-min-level))
                	;; Display parent heading.
                   	(outline-flag-region (line-end-position 0) (line-end-position) nil)
                   	(forward-line)
                	;; Display children.  First child may be deeper than expected
                	;; MAX-LEVEL.  Since we want to display it anyway, adjust
                	;; MAX-LEVEL accordingly.
                   	(while (re-search-forward re end t)
                       (unless past-first-child
                           (setq re (format regexp-fmt
                                     current-level
                                     (max (funcall outline-level) max-level)))
                           (setq past-first-child t))
                       (outline-flag-region
                          (line-end-position 0) (line-end-position) nil))))))))))

;; https://github.com/abo-abo/lispy/issues/57
;;** lispy: lispy-mode-map-special

;; my custom lispy-mode-map-special to modify for BEAKL layout
;; (eval-after-load 'lispy )

;; (require 'lispy)

(autoload 'lispy-mode "lispy")

(eval-after-load "lispy"
  '(setq lispy-mode-map-special
       (let ((map (make-sparse-keymap)))
         ;; navigation
         (lispy-define-key map "t" 'lispy-right)
         (lispy-define-key map "c" 'lispy-left)
         (lispy-define-key map "f" 'lispy-flow)
         (lispy-define-key map "s" 'lispy-down)
         (lispy-define-key map "r" 'lispy-up)
         (lispy-define-key map "d" 'lispy-different)
         (lispy-define-key map "o" 'lispy-other-mode)
         ;; (lispy-define-key map "p" 'lispy-eval-other-window)
         (lispy-define-key map "P" 'lispy-paste)
         (lispy-define-key map "y" 'lispy-occur)
         (lispy-define-key map "z" 'lh-knight/body)
         ;; outline
         (lispy-define-key map "S" 'lispy-outline-next)
         (lispy-define-key map "R" 'lispy-outline-prev)
         (lispy-define-key map "T" 'lispy-outline-goto-child)
         ;; Paredit transformations
         (lispy-define-key map ">" 'lispy-slurp)
         (lispy-define-key map "<" 'lispy-barf)
         (lispy-define-key map "/" 'lispy-splice)
         (lispy-define-key map "k" 'lispy-raise)
         (lispy-define-key map "K" 'lispy-raise-some)
         (lispy-define-key map "+" 'lispy-join)
         ;; more transformations
         (lispy-define-key map "C" 'lispy-convolute)
         (lispy-define-key map "X" 'lispy-convolute-left)
         (lispy-define-key map "w" 'lispy-move-up)
         (lispy-define-key map "j" 'lispy-move-down)
         (lispy-define-key map "O" 'lispy-oneline)
         (lispy-define-key map "M" 'lispy-alt-multiline)
         (lispy-define-key map "J" 'lispy-stringify)
         ;; marking
         (lispy-define-key map "a" 'lispy-ace-symbol
           :override '(cond ((looking-at lispy-outline)
                             (lispy-meta-return))))
         (lispy-define-key map "L" 'lispy-ace-symbol-replace)
         (lispy-define-key map "m" 'lispy-mark-list)
         ;; dialect-specific
         (lispy-define-key map "e" 'lispy-eval)
         (lispy-define-key map "E" 'lispy-eval-and-insert)
         (lispy-define-key map "G" 'lispy-goto-local)
         (lispy-define-key map "g" 'lispy-goto)
         (lispy-define-key map "F" 'lispy-follow t)
         (lispy-define-key map "D" 'pop-tag-mark)
         (lispy-define-key map "A" 'lispy-beginning-of-defun)
         (lispy-define-key map "_" 'lispy-underscore)
         ;; miscellanea
         (define-key map (kbd "SPC") 'lispy-space)
         (lispy-define-key map "i" 'lispy-tab)
         (lispy-define-key map "I" 'lispy-shifttab)
         (lispy-define-key map "N" 'lispy-narrow)
         (lispy-define-key map "W" 'lispy-widen)
         (lispy-define-key map "l" 'lispy-clone)
         (lispy-define-key map "u" 'lispy-undo)
         (lispy-define-key map "q" 'lispy-ace-paren
           :override '(cond ((bound-and-true-p view-mode)
                             (View-quit))))
         (lispy-define-key map "Q" 'lispy-ace-char)
         (lispy-define-key map "v" 'lispy-view)
         (lispy-define-key map "h" 'lispy-teleport
           :override '(cond ((looking-at lispy-outline)
                             (end-of-line))))
         (lispy-define-key map "n" 'lispy-new-copy)
         (lispy-define-key map "b" 'lispy-back)
         (lispy-define-key map "B" 'lispy-ediff-regions)
         (lispy-define-key map "x" 'lispy-x)
         (lispy-define-key map "Z" 'lispy-edebug-stop)
         (lispy-define-key map "V" 'lispy-visit)
         (lispy-define-key map "-" 'lispy-ace-subword)
         (lispy-define-key map "." 'lispy-repeat)
         (lispy-define-key map "~" 'lispy-tilde)
         ;; digit argument
         (mapc (lambda (x) (lispy-define-key map (format "%d" x) 'digit-argument))
               (number-sequence 0 9))
         map)))

(defun ram-lispy-ampersand ()
  "Insert &."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\|\\\\")
  (insert "& "))

(defun ram-lispy-persent ()
  "Insert %."
  (interactive)
  (lispy--space-unless "\\s-\\|\\s(\\|[:?]\\|\\\\")
  (insert "%"))

(eval-after-load "lispy"
  '(setq lispy-mode-map-lispy
       (let ((map (copy-keymap lispy-mode-map-base)))
         ;; navigation
         (define-key map (kbd "]") 'lispy-forward)
         (define-key map (kbd "[") 'lispy-backward)
         (define-key map (kbd ")") 'lispy-right-nostring)
         ;; kill-related
         (define-key map (kbd "C-y") 'lispy-yank)
         (define-key map (kbd "C-d") 'lispy-delete)
         (define-key map (kbd "DEL") 'lispy-delete-backward)
         (define-key map (kbd "M-k") 'lispy-kill-sentence)
         (define-key map (kbd "M-m") 'lispy-mark-symbol)
         (define-key map (kbd "C-,") 'lispy-kill-at-point)
         (define-key map (kbd "C-M-,") 'lispy-mark)
         ;; pairs
         (define-key map (kbd "{") 'lispy-braces)
         (define-key map (kbd "}") 'lispy-brackets)
         (define-key map (kbd "\"") 'lispy-quotes)
         ;; insert
         (define-key map (kbd ":") 'lispy-colon)
         (define-key map (kbd "^") 'lispy-hat)
         (define-key map (kbd "@") 'lispy-at)
         ;; (define-key map (kbd "'") 'lispy-tick)
         (define-key map (kbd "'") nil)
         (define-key map (kbd "`") 'lispy-backtick)
         (define-key map (kbd "#") 'lispy-hash)
         (define-key map (kbd "M-j") 'lispy-split)
         (define-key map (kbd "M-J") 'lispy-join)
         (define-key map (kbd "<C-return>") 'lispy-open-line)
         (define-key map (kbd "<M-return>") 'lispy-meta-return)
         (define-key map (kbd "<return>") 'lispy-alt-line)
         (define-key map (kbd "M-RET") 'lispy-meta-return)
         ;; misc
         (define-key map (kbd "M-o") 'lispy-string-oneline)
         (define-key map (kbd "M-i") 'lispy-iedit)
         (define-key map (kbd "<backtab>") 'lispy-shifttab)
         ;; outline
         (define-key map (kbd "M-<left>") 'lispy-outline-demote)
         (define-key map (kbd "M-<right>") 'lispy-outline-promote)
         (define-key map (kbd "&") 'ram-lispy-ampersand)
         (define-key map (kbd "%") 'ram-lispy-persent)
         map)))

(eval-after-load "lispy"
  '(lispy-set-key-theme '(special lispy c-digits)))

;;** lispy bindings
(define-key ram-leader-map-tap-prog (kbd "e") 'lispy-eval-and-comment)

(defun ram-expand-abbrev-call-lispy-parens ()
  "Call `expand-abbrev' and then call `lispy-parens'."
  (interactive)
  (expand-abbrev)
  (lispy-parens current-prefix-arg))

(with-eval-after-load "lispy"
  (define-key lispy-mode-map (kbd "(") #'ram-expand-abbrev-call-lispy-parens))

;;* avy

(straight-use-package
 '(avy :type git :flavor melpa :host github :repo "abo-abo/avy"))

;;** avy: actions

;; credit to https://karthinks.com/software/avy-can-do-anything/
(defun ram-avy-action-help (pt)
  (save-excursion
    (goto-char pt)
    (if (looking-at ram-open-delimiters-re)
        (forward-char))
    (let (;; (symbol (thing-at-point 'sexp))
          (symbol (symbol-at-point)))
      (cond
       ((derived-mode-p 'emacs-lisp-mode)
        (cond
         ((symbol-function symbol) (describe-function symbol))
         ((special-form-p symbol) (describe-function symbol))
         ((macrop symbol) (describe-function symbol))
         ((and (boundp symbol) (symbol-value symbol)) (describe-variable symbol))
         (t (message "The thing at point: \"%S\" is not bound to anything"  symbol))))
       (t (message "The %s is not supported" major-mode)))))
  (select-window
   (cdr (ring-ref avy-ring 0)))
  t)

;;   (setq avy-dispatch-alist
;;         '((?- . avy-action-kill-move)
;;           (?! . avy-action-kill-stay)
;;           (?\' . avy-action-teleport)
;;           (?` . avy-action-mark)
;;           (?/ . avy-action-copy)
;;           (?? . avy-action-yank)
;;           (?. . avy-action-ispell)
;;           (?# . avy-action-zap-to-char)))

(setq avy-dispatch-alist
      '((?K . avy-action-kill-move)
        (?_ . avy-action-kill-stay)     ; same place as 'K'
        (?T . avy-action-teleport)
        (?M . avy-action-mark)
        (?C . avy-action-copy)
        (?Y . avy-action-yank)
        ;; (?. . avy-action-ispell)
        ;; (?. . ace-link--org-action)
        (?. . avy-action-goto)
        (?# . avy-action-zap-to-char)
        (?L . ace-link--org-action)
        (?H . ram-avy-action-help)))

;;** avy: settings

(setq avy-all-windows nil)
(setq avy-all-windows-alt 'all-frames)
;; avy-lead-face-0
;; Dim all windows when displaying overlay for targets
(setq avy-background t)
(setq avy-timeout-seconds 0.4)
;; (setq highlight-first t)
;; https://github.com/abo-abo/avy/wiki/defcustom#avy-keys-alist#avy-keys
;; (setq avy-keys '(?a ?s ?r ?e ?t ?i ?u ?n ?o ?p ?l ?m ?f ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d))
;; (setq avy-keys '(?a ?s ?r ?e ?t ?i ?u ?n ?o ?p ?l ?m ?f ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d ?9 ?8 ?7 ?6 ?5 ?4 ?3 ?2 ?1))
(setq avy-keys '(?a ?s ?r ?e ?t ?i ?u ?n ?o ?p ?l ?m ?f ?- ?' ?/ ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d ?\( ?$ ?\) ?{ ?* ?} ?9 ?8 ?7 ?6 ?5 ?4 ?3 ?2 ?1))

;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
(setq avy-style 'at-full)
(setq avy-styles-alist '(
                         ;; (avy-goto-char-2 . post)
                         (avy-goto-word-1 . at-full)
                         ;; (avy-goto-char-2 . pre)
                         (avy-goto-char-2 . at-full)
                         (avy-goto-char-timer . at-full)))

;; (setq avy-keys (nconc
;;                     (number-sequence ?1 ?9)
;;                     '(?0)))

(setq avy-orders-alist
      '((avy-goto-char . avy-order-closest)
        (avy-goto-char-2 . avy-order-closest)
        (avy-goto-word-1 . avy-order-closest)
        ;; (lispy-ace-paren . ram-avy-order-furthest)
        ;; (lispy-ace-paren . ram-avy-order-from-beg-of-defun)
        (lispy-ace-paren . ram-avy-order-from-end-of-defun)
        (lispy-ace-char . avy-order-closest)
        (lispy-ace-symbol . avy-order-closest)
        (lispy-ace-subword . avy-order-closest)
        (ram-avy-goto-subword-2 . avy-order-closest)
        (ram-avy-goto-paragraph-start . ram-avy-order-furthest)))

;;** avy: custom commands

;; https://github.com/abo-abo/avy/wiki/custom-commands

(defvar org-link-any-re)
(defvar avy-background)
(declare-function org-open-at-point "org")
(declare-function outline-invisible-p "outline")
(declare-function ace-link--org-collect "ace-link")
(declare-function avy-process "avy")
(declare-function avy-jump "avy")
(declare-function avy-goto-subword-0 "avy")
;; this is the default value
(defface avy-background-face
  '((t (:foreground "gray40")))
  "Face for whole window background during selection.")

(defvar ram-avy--overlays-background nil
  "Hold dimmed background overlays enabled before avy-goto-* commands.")

(defun ram-avy-order-furthest (x)
  "The furthest from the point gets the lowest value. Works
only for the selected window."
  (- (max (abs (- (point) (window-start))) (abs (- (point) (window-end))))
     (abs (- (caar x) (point)))))

(defun ram-avy-order-from-beg-of-defun (x)
  "Order from beginning of `defun'."
  (abs (- (caar x) (save-excursion (beginning-of-defun) (point)))))

(defun ram-avy-order-from-end-of-defun (x)
  "Order from end of `defun'."
  (abs (- (caar x) (save-excursion (end-of-defun) (point)))))

(defun ram-avy--done ()
  "Clean up overlays."
  (mapc #'delete-overlay ram-avy--overlays-background)
  (setq ram-avy--overlays-background nil))
;; (avy--remove-leading-chars)


(defun ram-avy--keyboard-quit-advice (fn &rest args)
  (unwind-protect
      (apply fn args)
    (when ram-avy--overlays-background
      (ram-avy--done))))

(advice-add 'keyboard-quit :around #'ram-avy--keyboard-quit-advice)
;; (advice-add 'minibuffer-keyboard-quit :before #'ram-avy--done)

(defun ram-avy--make-backgrounds ()
  "Create dim a background overlay for selected window"
  (when avy-background
    (setq ram-avy--overlays-background
          (mapcar (lambda (w)
                    (let ((ol (make-overlay
                               (window-start w)
                               (window-end w)
                               (window-buffer w))))
                      (overlay-put ol 'face 'avy-background-face)
                      (overlay-put ol 'window w)
                      ol))
                  (list (selected-window))))))

(defun ram-avy-goto-subword-2 (char1 char2 &optional arg)
  "Jump to the currently visible CHAR at a subword start.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive (list (read-char "char 1: " t)
                     (read-char "char 2: " t)
                     current-prefix-arg))
  (let ((char1 (downcase char1))
        (char2 (downcase char2))
        (avy-style 'at-full)
        (avy-command 'ram-avy-goto-subword-2))
    (setq avy-action nil)
    (avy-goto-subword-0
     arg (lambda ()
           (and (char-after)
                (eq (downcase (char-after)) char1)
                (if (eq char2 ?*)
                    t
                  (save-excursion (forward-char) (eq (downcase (char-after)) char2))))))))

(defun ram-avy-goto-top-paren ()
  (interactive)
  (let ((avy-command 'ram-avy-goto-top-paren)
        (avy-style 'at)
        (avy--overlay-offset 0))
    (setq avy-action nil)
    (avy-jump "^(" :window-flip nil :beg nil :end nil)))

(defun ram-avy-goto-paragraph-start ()
  (interactive)
  (ram-avy--make-backgrounds)
  (let ((avy-command 'ram-avy-goto-paragraph-start)
        (avy-style 'post)
        (avy--overlay-offset -1))
    (setq avy-action nil)
    (avy-jump "\n\n[ \t]*[[:graph:]]" :window-flip nil :beg nil :end nil))
  (re-search-forward "[[:graph:]]" (window-end) t 1)
  (backward-char)
  (ram-avy--done))

(defun ram-avy-goto-org-heading ()
  (interactive)
  (let ((avy-style 'at-full)
        (avy-command 'ram-avy-goto-org-heading)
        (avy--overlay-offset 0))
    (setq avy-action nil)
    (avy-jump "^[[:blank:]]*\\*+ [^[:space:]]" :window-flip nil :beg nil :end nil))
  (re-search-forward "[^*[:space:]]" (window-end) t 1)
  (forward-char -1))

(defun ram-avy-goto-org-link ()
  (interactive)
  (let ((avy-command 'ram-avy-goto-org-link))
    ;; (setq avy-action nil)
    (setq avy-action 'ace-link--org-action)
    (avy-process (mapcar #'cdr (ace-link--org-collect)))))

;; copied from  https://github.com/abo-abo/ace-link
(defun ace-link--org-collect ()
  (let ((end (window-end))
        res)
    (save-excursion
      (goto-char (window-start))
      (while (re-search-forward org-link-any-re end t)
        ;; Check that the link is visible. Look at the last character
        ;; position in the link ("...X]]") to cover links with and
        ;; without a description.
        (when (not (outline-invisible-p (- (match-end 0) 3)))
          (push
           (cons
            (buffer-substring-no-properties
             (match-beginning 0)
             (match-end 0))
            (match-beginning 0))
           res)))
      (nreverse res))))

(defun ace-link--org-action (pt)
  (when (numberp pt)
    (goto-char pt)
    (org-open-at-point)))

(defun ram-avy-goto-ace-paren ()
  "Call `lispy-ace-paren'."
  (interactive)
  (require 'lispy)
  (let* ((avy-command 'ram-avy-goto-ace-paren)
         (avy-style 'at-full)
         (avy-orders-alist (list (cons avy-command 'ram-avy-order-from-beg-of-defun))))
    (call-interactively 'lispy-ace-paren)))

(defun ram-avy-goto-paragraph-start ()
  (interactive)
  (ram-avy--make-backgrounds)
  (let ((avy-command 'ram-avy-goto-paragraph-start)
        (avy-style 'post)
        (avy--overlay-offset -1))
    (setq avy-action nil)
    (avy-jump "\n\n[ \t]*[[:graph:]]" :window-flip nil :beg nil :end nil))
  (re-search-forward "[[:graph:]]" (window-end) t 1)
  (backward-char)
  (ram-avy--done))

(defun ram-avy-goto-symbol-in-defun ()
  "Call `lispy-ace-paren'."
  (interactive)
  (require 'lispy)
  (let (beg end)
    (save-excursion
      (setq beg (or (ram-up-list-backward 50) (point)))
      (setq end (forward-list)))
    (let ((avy-command 'ram-avy-goto-symbol-in-defun)
          ;; (avy-style 'at-full)
          (avy-style 'at-full)
          (avy--overlay-offset 0)
          (cands (avy--regex-candidates
                  ;; "\\_<\\(?:\\sw\\|\\s_\\[^\\s\.]\\)+\\_>"
                  "[([{ ]\\(?:\\sw\\|\\s_\\|[\"'`#~,@]\\)"
                  ;; "\\(?:\\s\".+?\\s\"\\)"
                  beg end
                  (lambda () (forward-char -1)
                    (let* ((sp (syntax-ppss))
                           (beg (nth 8 sp)))
                      (not (when (or (eq (char-after beg) ?\") ; no strings
                                     (nth 4 sp))  ; no comments
                             beg)))) 0)))
      (dolist (x cands)
        (when (> (- (cdar x) (caar x)) 1)
          (cl-incf (caar x))))
      (setq avy-action nil)
      (avy-process cands))))

(defun ram-avy-goto-word-in-defun ()
  "Call `lispy-ace-paren'."
  (interactive)
  (require 'lispy)
  (let (beg end)
    (save-excursion
      (setq beg (ram-up-list-backward 50))
      (setq end (forward-list)))
    (cl-letf ((avy-command 'ram-avy-goto-word-in-defun)
              (avy-style 'at-full)
              (avy--overlay-offset 0)
              (cands (avy--regex-candidates
                      ;; "[([{ -/]\\(?:\\sw\\|\\s_\\|\\s(\\|[\"'`#]\\)"
                      "\\<\\sw+\\>"
                      beg end
                      (lambda () (let ((sp (syntax-ppss)))
                                   (not (nth 4 sp)))) nil)) ; no comments
              ((symbol-function 'avy-action-mark)
               (lambda (pt)
                 (goto-char pt)
                 (forward-word)
                 (set-mark (point))
                 (goto-char pt))))
      (setq avy-action nil)
      (avy-process cands))))

;;** avy: bindings

(defun ram-avy-goto-subword-2-dim ()
  "Dim window before Invoking `ram-avy-goto-subword-2'."
  (interactive)
  (ram-avy--make-backgrounds)
  (condition-case err
      (call-interactively #'ram-avy-goto-subword-2)
    (error
     (ram-avy--done)
     (signal (car err) (cdr err)))
    (quit
     (ram-avy--done)
     (signal 'quit nil))
    (:success (ram-avy--done))))

(eval-after-load "avy"
  '(progn
     ;; (define-key global-map (kbd "s-s") (lambda () (interactive)
     ;;                                      (ram-avy--make-backgrounds)
     ;;                                      (avy-goto-word-or-subword-1)
     ;;                                      (ram-avy--done)))
     (define-key global-map (kbd "s-s") #'ram-avy-goto-subword-2-dim)
     ;; (define-key global-map (kbd "s-s") 'avy-goto-char-2)
     (define-key global-map (kbd "s-r") 'ram-avy-goto-paragraph-start)
     ;; (define-key global-map (kbd "s-R") 'ram-avy-goto-top-paren)
     ;; (define-key global-map (kbd "s-d") 'avy-goto-char-in-line)
     ;; (define-key global-map (kbd "s-N") 'avy-resume)

     (define-key prog-mode-map (kbd "s-R") #'ram-avy-goto-top-paren)

     (define-key prog-mode-map (kbd "s-s") #'ram-avy-goto-subword-2-dim)
     (define-key prog-mode-map (kbd "s-S") #'ram-avy-goto-symbol-in-defun)
     (define-key prog-mode-map (kbd "C-s-s") #'ram-avy-goto-word-in-defun)
     (define-key prog-mode-map (kbd "s-r") #'ram-avy-goto-ace-paren)
     (define-key prog-mode-map (kbd "C-s-r") #'avy-resume)
     ))

;;* projectile

(straight-use-package
 '(projectile :type git :flavor melpa :files ("projectile.el" "projectile-pkg.el") :host github :repo "bbatsov/projectile"))
(projectile-mode 1)
(with-eval-after-load 'projectile
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "<f12>") 'projectile-command-map)
  ;; <f7> because it is same place as 't' which the default binding for this command
  (define-key projectile-mode-map (kbd "<f12> <f7>") 'projectile-toggle-between-implementation-and-test))

;;* multiple-cursors

(straight-use-package
 '(multiple-cursors :type git :flavor melpa :host github :repo "magnars/multiple-cursors.el"))
;; will make <return> insert a newline; multiple-cursors-mode can still be disabled with C-g
(with-eval-after-load "multiple-cursors"
  (define-key mc/keymap (kbd "<return>") nil)

  (add-to-list 'mc/cmds-to-run-once 'mc/toggle-cursor-at-point)
  (add-to-list 'mc/cmds-to-run-once 'multiple-cursors-mode))


;;credit to https://stackoverflow.com/a/39885314/9913235
(defun mc/toggle-cursor-at-point ()
  "Add or remove a cursor at point."
  (interactive)
  (require 'multiple-cursors)
  (if multiple-cursors-mode
      (error "Cannot toggle cursor at point while `multiple-cursors-mode' is active.")
    (let ((existing (mc/fake-cursor-at-point)))
      (if existing
          (mc/remove-fake-cursor existing)
        (mc/create-fake-cursor-at-point)))))

(define-key global-map (kbd "M-<f9>") #'mc/toggle-cursor-at-point)
(define-key global-map (kbd "M-S-<f9>") 'multiple-cursors-mode)

;;* brackets, parentheses, parens, sexps

;;** brackets, parentheses, parens, sexps: multi-line, one-line, flatten

(defun ram-toggle-multiline-delimited-sexp ()
  "Toggle between single line and multi-line delimited sexp format."
  (interactive)
  (let* ((bounds (ram-delimited-sexp-bounds))
         (location (- (point) (car bounds)))
         newline-match-p)
    (save-excursion
      (goto-char (cdr bounds))
      (while (re-search-backward "\n[[:space:]]*" (car bounds) t)
        (setq newline-match-p t)
        ;; if at close paren do not insert whitespace
        (if (and
             (goto-char (match-end 0))
             (memq (char-after) ram-close-delimiters))
            (progn
              (replace-match "")
              (goto-char (match-beginning 0)))
          (replace-match " "))))
    (when (not newline-match-p)
      (save-excursion
        (goto-char (cdr bounds))
        (while (re-search-backward "[[:space:]]+" (car bounds) t)
          (let ((ppss (syntax-ppss)))
            (when (not (or
                        (nth 3 ppss)
                        (nth 4 ppss)
                        ;; inside child list
                        (not (= (car bounds) (car (last (nth 9 ppss)))))))
              (replace-match "\n")
              (beginning-of-line)))))
      (save-excursion (goto-char (car bounds)) (indent-sexp))
      ;; (indent-according-to-mode)
      )))

;;** brackets, parentheses, parens, sexps: navigation

(defun ram-forward-list (&optional arg)
  "Move to the next thing.
When at the list limit, move a level up and continue.
If ARG is `nil', do not `push-mark'."
  (interactive "p")
  (let* ((bounds (ram-sexp-bounds))
         (next-bounds (save-excursion (goto-char (cdr bounds))
                                      (ram-next-thing-bounds)))
         (at-end-p (= (point) (cdr bounds))))
    (deactivate-mark)
    (when (and arg
               (mark)
               (not (= (mark) (point))))
      (push-mark))
    (when next-bounds
      (if at-end-p
          (progn
            (goto-char (cdr next-bounds))
            next-bounds)
        (goto-char (car next-bounds))
        (if (> (car bounds) (car next-bounds))
            (ram-forward-list)
          next-bounds)))))

(defun ram-backward-list (&optional arg)
  "Move to the previous thing.
When at the list limit, move a level up and continue.
If ARG is `nil', do not `push-mark'."
  (interactive "p")
  (let* ((point (point))
         (bounds (ram-thing-bounds))
         (prev-bounds (save-excursion (goto-char (car bounds))
                                      (ram-prev-thing-bounds)))
         (at-end-p (ram-at-thing-end-p)))
    (deactivate-mark)
    (when (and arg
               (when (mark) (not (= (mark) (point)))))
      (push-mark))
    (if at-end-p
        (progn
          (goto-char (cdr prev-bounds))
          (if (< (cdr bounds) (cdr prev-bounds))
              (ram-backward-list)
            prev-bounds))
      (and prev-bounds
           (goto-char (car prev-bounds)))
      prev-bounds)))

;; This behaves like ram-jump-forward-to-close-delimiter
;; I'll keep to see if they differ for some edge cases.
(defun ram-down-list ()
  "Move down parenthesis groups. If cannot, move `forward-list'.
If cannot move forward, go `up-list' and try again from there."
  (interactive)
  (cl-labels ((up-list-then-forward ()
                "Move `up-list' then `forward-list', repeat on error."
                (condition-case err
                    (up-list)
                  (scan-error (message "Call ram-forward-sexp (preserve the current open or close paren)"))
                  (:success (condition-case err
                                (forward-list)
                              (scan-error (up-list-then-forward))
                              (:success (backward-sexp)))))))
    (condition-case err
        (down-list)
      (scan-error (scan-error (forward-list)))
      (:success (progn
                  (condition-case err
                      (forward-list)
                    (scan-error (up-list-then-forward))
                    (:success (backward-sexp))))))))

(defun ram-jump-backward-to-open-delimiter ()
  "Jump backward to the open delimiter that is not in a string."
  (interactive)
  (cl-labels ((back-to-delim ()
                "Jump backward to the open delimiter that is not in a string."
                (re-search-forward ram-open-delimiters-re nil t -1)
                ;; skip matches in strings and comments
                (let ((s (syntax-ppss)))
                  (if (or (nth 3 s)
                          (nth 4 s))
                      (back-to-delim)))))
    (back-to-delim)))

(defun ram-jump-forward-to-open-delimiter ()
  "Jump forward to the open delimiter that is not in a string."
  (interactive)
  (cl-labels ((forward-to-delim ()
                "Jump forward to the open delimiter that is not in a string."
                (re-search-forward ram-open-delimiters-re nil t 1)
                ;; skip matches in strings and comments
                (let ((s (syntax-ppss)))
                  (if (or (nth 3 s)
                          (nth 4 s))
                      (forward-to-delim)))))
    (forward-char 1)
    (forward-to-delim)
    (forward-char -1)))

(defun ram-jump-forward-to-close-delimiter ()
  "Jump forward to the close delimiter that is not in a string."
  (interactive)
  (cl-labels ((forward-to-delim ()
                "Jump forward to the close delimiter that is not in a string."
                (re-search-forward ram-close-delimiters-re nil t 1)
                ;; skip matches in strings and comments
                (let ((s (syntax-ppss)))
                  (if (or (nth 3 s)
                          (nth 4 s))
                      (forward-to-delim)))))
    (forward-to-delim)))

(defun ram-jump-backward-to-close-delimiter ()
  "Jump backward to the close delimiter that is not in a string."
  (interactive)
  (cl-labels ((back-to-delim ()
                "Jump backward to the open delimiter that is not in a string."
                (when (ram-at-delimited-end-p) (backward-char))
                (re-search-backward ram-close-delimiters-re nil t 1)
                (when (match-string 0) (forward-char))
                ;; skip matches in strings and comments
                (let ((s (syntax-ppss)))
                  (if (or (nth 3 s)
                          (nth 4 s))
                      (back-to-delim)))))
    (back-to-delim)))

;; credit to http://xahlee.info/emacs/emacs/emacs_navigating_keys_for_brackets.html
(defun xah-goto-matching-bracket ()
  "Move cursor to the matching bracket.
If cursor is not on a bracket, call `backward-up-list'."
  (interactive)
  (if (nth 3 (syntax-ppss))
      (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)
    (cond
     ((eq (char-after) ?\") (forward-sexp))
     ((eq (char-before) ?\") (backward-sexp ))
     ((looking-at ram-open-delimiters-re) (forward-sexp))
     ((looking-back ram-close-delimiters-re (max (- (point) 1) 1))
      (backward-sexp))
     (t (backward-up-list 1 'ESCAPE-STRINGS 'NO-SYNTAX-CROSSING)))))

;; copied from  https://github.com/abo-abo/lispy
(defun ram-up-list-forward (arg)
  "Move forward out of one level of parentheses ARG times.
Return nil on failure, (point) otherwise."
  ;; move out of the string
  (interactive "p")
  (let ((s (syntax-ppss)))
    (when (nth 3 s)
      (goto-char (nth 8 s))))
  (catch 'break
    (dotimes (_i arg)
      (when (not (ignore-errors (up-list) t))
        (when (looking-at-p ram-open-delimiters-re)
          (forward-list))
        (throw 'break nil)))
    (point)))

;; copied from  https://github.com/abo-abo/ace-link
(defun ram-up-list-backward (arg)
  "Move backward out of one level of parentheses ARG times.
Return nil on failure, (point) otherwise."
  (interactive "p")
  (let ((oldpt (point))
        newpt)
    (ram-up-list-forward arg)
    (when (looking-back ram-close-delimiters-re (line-beginning-position))
      (backward-list))
    (if (= oldpt (setq newpt (point)))
        nil
      newpt)))

(defun ram-beg-of-top-sexp (arg)
  "Jump to the beginning of top level sexp ARG times."
  (interactive "p")
  (push-mark)
  (let* ((point (point))
         (comment-bounds (ram-block-comment-bounds))
         (in-top-level-comments (and comment-bounds
                                     (save-excursion (= (car comment-bounds) (point-at-bol))))))
    (if in-top-level-comments
        (goto-char (car comment-bounds))
      (ram-up-list-forward 50)
      (forward-sexp (- arg)))))

(defun ram-end-of-top-sexp (arg)
  "Jump to the end of top level sexp ARG times."
  (interactive "p")
  (push-mark)
  (let* ((point (point)))
    (ram-up-list-forward 50)
    (if (= point (point))
        (forward-sexp arg)
      (forward-sexp (- arg 1)))))

(defun ram-next-defun (&optional arg)
  "Jump to the next top level thing.
If ARG is negative, reverse the final point location.
If ARG is 4, move to the end of defun."
  (interactive "p")
  (when (buffer-narrowed-p)
    (widen))
  (let ((at-end-p (ram-at-thing-end-p))
        next-bounds)
    (if (= arg 4)
        (ram-end-of-top-sexp 1)
      (ram-beg-of-top-sexp 1)
      (setq next-bounds (ram-forward-list))
      (when (< arg 0)                   ; reverse at-end-p value
        (setq at-end-p (not at-end-p)))
      (if at-end-p
          (goto-char (cdr next-bounds))
        (goto-char (car next-bounds))))))

(defun ram-prev-defun (&optional arg)
  "Jump to the previous top level thing.
If ARG is negative, reverse the final point location.
If ARG is 4, move to the beginning of defun."
  (interactive "p")
  (when (buffer-narrowed-p)
    (widen))
  (let ((at-end-p (ram-at-thing-end-p))
        prev-bounds)
    (if (= arg 4)
        (ram-beg-of-top-sexp 1)
      (ram-beg-of-top-sexp 1)
      (setq prev-bounds (ram-backward-list))
      (when (< arg 0)                   ; reverse at-end-p value
        (setq at-end-p (not at-end-p)))
      (if at-end-p
          (and prev-bounds
               (goto-char (cdr prev-bounds)))
        (and prev-bounds
             (goto-char (car prev-bounds)))) )))

;;** brackets, parentheses, parens, sexps: settings

(defvar ram-delimiters "{}()[]"
  "A string of open and close delimiters pairs.")

(defvar ram-open-delimiters
  (let ((delims))
    (dotimes (idx (length ram-delimiters))
      (when (= (% idx 2) 0)
        (setq delims (cons (aref ram-delimiters idx) delims))))
    delims)
  "A list of open delimiter characters.")

(defvar ram-open-delimiters-re (concat "[" (string-join (mapcar #'char-to-string ram-open-delimiters)) "]")
  "Regexp to match common open delimiters.")

(defvar ram-close-delimiters
  (let ((delims))
    (dotimes (idx (length ram-delimiters))
      (when (= (% idx 2) 1)
        (setq delims (cons (aref ram-delimiters idx) delims))))
    delims)
  "A list of close delimiter characters.")

(defvar ram-close-delimiters-re (concat "[" (string-join (mapcar #'char-to-string ram-close-delimiters)) "]")
  "Regexp to match common close delimiters.")

;; If true, #'paredit-blink-paren-match is slow
(setq blink-matching-paren nil)

;;** brackets, parentheses, parens, sexps: ram-highlight-sexps

(autoload 'ram-highlight-sexps-mode "ram-highlight-sexps")

(setq hl-sexp-highlight-adjacent t)
(add-hook 'emacs-lisp-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'lisp-interactive-mode #'ram-highlight-sexps-mode)
(add-hook 'clojure-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'cider-repl-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'racket-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'racket-repl-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'minibuffer-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'org-mode-hook #'ram-highlight-sexps-mode)

(with-eval-after-load "ram-highlight-sexps"
  (add-hook 'after-load-theme-hook #'hl-sexp-color-update))

;;** brackets, parentheses, parens, sexps: highlight

(defvar ram-copied-region-overlay nil
  "Highlight the region copied with `ram-copy-sexp'.")
(defun ram-create-copied-sexp-overlay ()
  "Create buffer local `ram-copied-region-overlay'
Add `pre-command-hook' to remove it."
  (make-variable-buffer-local 'ram-copied-region-overlay)
  (setq-local ram-copied-region-overlay (make-overlay 0 0))
  ;; (overlay-put ram-copied-region-overlay 'face (list :background
  ;;                                                   (face-background 'modus-themes-intense-green)))
  (overlay-put ram-copied-region-overlay 'face (list :background "yellow"))
  (overlay-put ram-copied-region-overlay 'priority 1)
  (add-hook 'pre-command-hook (lambda () (when (symbol-value 'ram-copied-region-overlay)
                                           (move-overlay ram-copied-region-overlay 1 1)))))

(add-hook 'prog-mode-hook #'ram-create-copied-sexp-overlay)
(add-hook 'org-mode-hook #'ram-create-copied-sexp-overlay)
(add-hook 'minibuffer-mode-hook #'ram-create-copied-sexp-overlay)

;;** brackets, parentheses, parens, sexps: comments

(defun ram-inline-comment-bounds ()
  "Return a pair of the inline comment beginning and end. "
  (if-let (beg (and (ram-in-comment-p)
                    (save-excursion
                      (ram-goto-beg-of-comment)
                      (re-search-backward "[^[:space:]]" (point-at-bol) t 1))))
      (cons beg (point-at-eol))))

(defun ram-block-comment-bounds ()
  "Return the beginning and end pair of a comment block."
  (if-let ((beg (save-excursion
                  (let ((beg (ram-goto-comment-block-beg)))
                    (when (and beg
                               (looking-back "^[[:space:]]*" (point-at-bol)))
                      beg))))
           (end (save-excursion (ram-goto-comment-block-end))))
      (cons beg end)))

;; adopted from https://github.com/abo-abo/lispy
(defun ram-in-comment-p ()
  (save-excursion
    (when (not (eolp))
      (forward-char 1))
    (nth 4 (syntax-ppss))))

(defun ram-goto-beg-of-comment ()
  "Go to comment beginning if in comment. Do Nothing otherwise."
  (if-let ((comment-start
            (when (not (minibufferp))
              (comment-normalize-vars)
              (save-excursion
                (end-of-line)
                (and (comment-beginning)
                     (comment-search-backward (point-at-bol) t))))))
      (goto-char comment-start)))

(defun ram-goto-comment-block-beg ()
  (when (ram-goto-beg-of-comment)
    (let ((pt (point)))
      (while (and (ram-in-comment-p)
                  (forward-comment -1)
                  (looking-back "^[[:space:]]*" (point-at-bol))
                  (= 1 (- (count-lines (point) pt)
                          (if (bolp) 0 1))))
        (setq pt (point)))
      (goto-char pt))))

(defun ram-goto-comment-block-end ()
  (when (ram-goto-beg-of-comment)
    (let ((pt (point))
          (col (current-column)))
      (while (and (ram-in-comment-p)
                  (forward-comment 1)
                  (ram-goto-beg-of-comment)
                  (and (= 1 (- (count-lines pt (point))
                               (if (bolp) 0 1)))
                       ;; only same indent level
                       (= col (current-column))
                       (looking-back "^[[:space:]]*" (point-at-bol))))
        (setq pt (point)))
      (goto-char pt)
      (end-of-line)
      (point))))

;;** brackets, parentheses, parens, sexps: at beg? at end?

(defun ram-at-delimited-beg-p ()
  "Return non `nil' if the point is after `ram-open-delimiters-re'."
  (when (not (ram-in-comment-p))
    (memq (char-after) ram-open-delimiters)))

(defun ram-at-delimited-end-p ()
  "Return non `nil' if the point is after `ram-close-delimiters-re'."
  (when (not (ram-in-comment-p))
    (memq (char-before) ram-close-delimiters)))

(defun ram-at-string-beg-p ()
  "Return non `nil' if `looking-at' a double quote character. "
  (and (eq (char-after) ?\")
       (not (eq ?\\ (char-before)))
       (point)))

(defun ram-at-string-end-p ()
  "Return non `nil' if `char-before' the point is a double quote."
  (and (eq ?\" (char-before))
       (not (eq ?\\ (char-before (- (point) 1))))
       (point)))

(defun ram-at-comment-block-beg-p ()
  (if-let ((beg (save-excursion (ram-goto-comment-block-beg))))
      (= (point) beg)))

(defun ram-at-comment-block-end-p ()
  (if-let ((end (save-excursion (ram-goto-comment-block-end))))
      (= (point) end)))

(defun ram-at-thing-beg-p ()
  (or (ram-at-delimited-beg-p)
      (ram-at-string-beg-p)
      (ram-at-comment-block-beg-p)))

(defun ram-at-thing-end-p ()
  (or (ram-at-delimited-end-p)
      (ram-at-string-end-p)
      (ram-at-comment-block-end-p)))

;;** brackets, parentheses, parens, sexps: <enter>, newline-and-indent

;; adopted from https://github.com/abo-abo/lispy #'lispy-alt-line
(defun ram-newline-and-indent (&optional arg)
  "Automate context aware tasks before invoking `newline-and-indent'.
Context aware actions:
- exit the minibuffer,
Before invoking `newline-and-indent':
- when in a comment, do nothing
- when in a string, go to the end of enclosing list
- inside or at border of a single line list.
  (setq foo| (bar)) -> (setq foo (bar)|)
...
"
  (interactive "p")
  (when (bound-and-true-p abbrev-mode)
    (expand-abbrev))
  (let ((bounds (ram-thing-bounds)))
    (cond
     ((> (minibuffer-depth) 0)
      (exit-minibuffer))
     ((ram-in-comment-p))
     ((ram-in-string-p) (goto-char (cdr bounds)))
     ;; inside a list or at its border,
     ;; the list spans a single line: go to its end
     ((let ((bounds (ram-delimited-sexp-bounds))
            (line-num (line-number-at-pos)))
        (when (and bounds
                   (= line-num (line-number-at-pos (car bounds)))
                   (= line-num (line-number-at-pos (cdr bounds))))
          (goto-char (cdr bounds)))))
     ;; captured by previous clause
     ;; ((ram-at-delimited-beg-p) (goto-char (cdr bounds)))
     ;; empty list: (|) -> ()|
     ;; captured by previous clause
     ;; ((and (memq (char-after (point)) ram-close-delimiters)
     ;;       (memq (char-before (point)) ram-open-delimiters))
     ;;  (goto-char (cdr bounds)))
     ;; blank line with ")" at the end
     ((and (looking-back "^ +" (point-at-bol))
           (looking-at (concat "[[:space:]]*" ram-close-delimiters-re)))
      (ram-remove-whitespace-between-regexps "[^[:space:]\n]" ram-close-delimiters-re)
      (forward-char))
     ;; default case handles this as well.
     ;; indented line: go to before closing paren or end of line
     ;; ((looking-back "^ +" (line-beginning-position))
     ;;  (let ((eol (point-at-eol)))
     ;;    (while (and (condition-case nil
     ;;                    (forward-sexp)
     ;;                  (scan-error nil)
     ;;                  (:success t))
     ;;                (< (point) eol)))))
     (t (let ((end (min (line-end-position)
                        (cdr (ram-delimited-sexp-bounds)))))
          (while (< (point) (1- end))
            (forward-sexp)))))
    (newline-and-indent)
    (indent-according-to-mode)))

(define-key emacs-lisp-mode-map (kbd "<return>") #'ram-newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "S-<return>") #'newline-and-indent)

(define-key lisp-interaction-mode-map (kbd "<return>") #'ram-newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "S-<return>") #'newline-and-indent)

;;** brackets, parentheses, parens, sexps: bounds

(defun ram-in-string-p ()
  "Return non-`nil' if inside a string or at its borders."
  (or (nth 3 (syntax-ppss))
      (ram-at-string-beg-p)
      (ram-at-string-end-p)))

(defun ram-string-bounds ()
  "Return the beginning and end of a string as a pair."
  (if-let ((ppss (syntax-ppss))
           (beg (or (when (nth 3 ppss)
                      (nth 8 ppss))
                    (or
                     (when (ram-at-string-beg-p)
                       (point))
                     (when (ram-at-string-end-p)
                       (save-excursion (backward-sexp) (point))))))
           (end (save-excursion (goto-char beg) (forward-sexp) (point))))
      (cons beg end)))


(defun ram-delimited-sexp-bounds (&optional select-nth-ancestor)
  "Return bounds delimited by `ram-open-delimiters-re' and `ram-close-delimiters-re'.
With SELECT-NTH-ANCESTOR value greater than zero, return bounds
for than ancestor."
  (if-let ((ppss (syntax-ppss))
           (beg (cond
                 ((> (or select-nth-ancestor 0) 0)
                  ;; when between defuns, there is no parent, select previous defun
                  (if (= (nth 0 ppss) 0)
                      (save-excursion (beginning-of-defun) (when (not (bobp)) (point)))
                    (let* ((ancestor-open-parens (nth 9 ppss))
                           (max-idx (1- (length ancestor-open-parens)))
                           (idx (max 0 (- max-idx (1- select-nth-ancestor))))
                           (required-ancestor-level (nth idx ancestor-open-parens)))
                      required-ancestor-level)))
                 ;; at open paren
                 ((looking-at-p ram-open-delimiters-re)
                  (point))
                 ;; in front of indented list, return the beg of indented list
                 ((and (looking-back "^[[:space:]]*" (point-at-bol))
                       (looking-at-p (concat "[[:space:]]*" ram-open-delimiters-re)))
                  (save-excursion
                    (re-search-forward ram-open-delimiters-re (point-at-eol) t 1)
                    (backward-char)
                    (point)))
                 ;; at close paren
                 ((ram-at-delimited-end-p)
                  (save-excursion (backward-sexp)
                                  (point)))
                 ;; inside parens
                 (t (if (= (nth 0 ppss) 0) ; not in a list
                        (save-excursion (beginning-of-defun) (when (not (bobp)) (point)))
                      (nth 1 ppss)))))
           (end (save-excursion (goto-char beg) (forward-sexp) (point))))
      (cons beg end)))

(defun ram-thing-bounds (&optional select-nth-ancestor)
  "Return the beginning and the end of a thing at point.
Only a line comment, string and list are valid choices."
  (if (> (or select-nth-ancestor 0) 0)
      (ram-delimited-sexp-bounds select-nth-ancestor)
    (cond
     ((ram-block-comment-bounds))
     ((ram-string-bounds))
     ((ram-delimited-sexp-bounds))
     (t (error "`ram-thing-bounds': All `cond' clauses failed")))))

(defun ram-prev-thing-bounds ()
  "Return the bounds of the previous thing.
The search must start outside the current thing bounds."
  (when (re-search-backward (concat "[^"
                                    (string-join (mapcar #'char-to-string ram-open-delimiters))
                                    "#'[:space:]" "\n]") nil t 1)
    (forward-char)
    (cond
     ((thing-at-point 'symbol)
      (progn (forward-symbol -1)
             (ram-prev-thing-bounds)))
     ((ram-inline-comment-bounds)
      (goto-char (car (ram-inline-comment-bounds)))
      (ram-prev-thing-bounds))
     (t (ram-thing-bounds)))))

(defun ram-next-thing-bounds ()
  "Return the bounds of the next thing.
The search must start outside the current thing bounds."
  (when (re-search-forward (concat "[^"
                                   (string-join (mapcar #'char-to-string ram-close-delimiters))
                                   "#'[:space:]" "\n]") nil t 1)
    (backward-char)
    (cond
     ((thing-at-point 'symbol)
      (forward-symbol 1)
      (ram-next-thing-bounds))
     ((ram-inline-comment-bounds)
      (end-of-line)
      (ram-next-thing-bounds))
     (t (ram-thing-bounds)))))

(defun ram-sexp-bounds (&optional select-nth-ancestor)
  "Return the bounds of comment block, string, sexp or list.
Whichever happens to be first."
  (if (> (or select-nth-ancestor 0) 0)
      (ram-delimited-sexp-bounds select-nth-ancestor)
    (cond
     ((ram-block-comment-bounds))
     ((ram-string-bounds))
     ((bounds-of-thing-at-point 'sexp))
     ((ram-delimited-sexp-bounds))
     (t (error "`ram-sexp-bounds': All `cond' clauses failed")))))

;;** brackets, parentheses, parens, sexps: select, copy, clone, kill

(defun ram-mark-sexp ()
  "Mark sexp.
The beginning and end of sexp is defined by return value of
`ram-sexp-bounds'."
  (interactive)
  (if-let ((bounds (if (eq last-command this-command)
                       (ram-delimited-sexp-bounds 1)
                     (ram-sexp-bounds))))
      (progn
        (if (ram-at-thing-end-p)
            (progn
              (set-mark (car bounds))
              (goto-char (cdr bounds)))
          (set-mark (cdr bounds))
          (goto-char (car bounds))))))

(defun ram-copy-sexp ()
  "Save sexp to `kill-ring'.
The beginning and end of sexp is defined by return value of
`ram-sexp-bounds'."
  (interactive)
  (let* ((repeated-p (eq last-command this-command))
         (bounds (if repeated-p
                     (ram-delimited-sexp-bounds 1)
                   (ram-sexp-bounds)))
         (str (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds)))))
    (when str
      (if repeated-p (kill-new str t) (kill-new str))
      (move-overlay ram-copied-region-overlay (car bounds) (cdr bounds))
      (if (ram-at-thing-end-p)
          (goto-char (cdr bounds))
        (goto-char (car bounds))))))

(defun ram-clone-sexp-forward ()
  "Clone sexp. Place the copy below the original."
  (interactive)
  (let* ((repeated-p (eq last-command this-command))
         (get-bounds #'ram-sexp-bounds)
         ;; repeated command selects the ancestor
         (bounds (funcall get-bounds (max 0 (1- (or ram-num-of-repeated-command-calls 0)))))
         (str (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (location (- (point) (car bounds)))
         ;; (at-end-p (ram-at-thing-end-p))
         )
    (when str
      (when repeated-p
        (delete-region (car bounds) (cdr bounds))
        ;; clean whitespaces
        (if (looking-at "[[:space:]]*$")
            (ram-remove-whitespace-between-regexps "[[:space:]]" "[^[:space:]\n]")
          (ram-remove-whitespace-between-regexps "[^[:space:]\n]" "[^[:space:]\n]")
          (when (not (memq (char-after) ram-close-delimiters)) (insert " ")))
        ;; search back for str
        (re-search-backward (regexp-quote str) nil nil 1)
        (goto-char (+ (point) location))
        ;; reset variables for the ancestor
        (setq bounds (ram-delimited-sexp-bounds ram-num-of-repeated-command-calls))
        (setq str (buffer-substring-no-properties (car bounds) (cdr bounds)))
        (setq location (- (point) (car bounds))))
      (goto-char (cdr bounds))
      (newline-and-indent)
      (insert str)
      ;; format the insert
      (when (not (or (memq (char-before) '(10 32))
                     (memq (char-after) '(10 32))
                     (memq (char-after) ram-close-delimiters) ))
        (insert " ")
        (backward-char))
      (indent-according-to-mode)
      ;; insert newline between top level clones
      (when (= (nth 0 (syntax-ppss)) 0)
        (goto-char (cdr bounds))
        (newline-and-indent)
        (forward-char))
      ;; move overlay over cloned sexp
      (let ((new-bounds (funcall get-bounds)))
        (move-overlay ram-copied-region-overlay
                      (car new-bounds) (cdr new-bounds))
        (goto-char (+ (car new-bounds) location))))))

(defun ram-clone-sexp-backward ()
  "Clone sexp. Place the copy above the original."
  (interactive)
  (let* ((bounds (ram-thing-bounds))
         (str (when bounds (buffer-substring-no-properties (car bounds) (cdr bounds))))
         (location (- (point) (car bounds)))
         ;; (at-end-p (ram-at-thing-end-p))
         )
    (when str
      (goto-char (car bounds))
      (newline-and-indent)
      (forward-line -1)
      (end-of-line)
      (when (not (or (= (char-before) 10)   ; beginning of line
                     (= (char-before) 32))) ; white space
        (insert " "))
      (insert str)
      (indent-according-to-mode)
      ;; insert newline for defun clones
      (when (= (nth 0 (syntax-ppss)) 0)
        (newline-and-indent)
        (backward-char))
      (let ((new-bounds (ram-thing-bounds)))
        (move-overlay ram-copied-region-overlay
                      (car new-bounds)
                      (cdr new-bounds))
        (goto-char (+ (car new-bounds) location))
        (indent-according-to-mode)
        ;; (if at-end-p
        ;;     (goto-char (cdr new-bounds))
        ;;   (goto-char (car new-bounds)))
        ))))

(defun ram-kill-at-point ()
  "Kill `ram-sexp-bounds' region."
  (interactive)
  (let* ((bounds (ram-sexp-bounds))
         (add-newline-p (save-excursion
                          (goto-char (car bounds))
                          (looking-back "^[[:space:]]*" (point-at-bol))))
         (repeated-p (eq last-command this-command))
         (str (let ((str (buffer-substring (car bounds) (cdr bounds))))
                (if add-newline-p
                    (concat "\n " str)
                  str)))
         (location (let ((loc (- (point) (car bounds))))
                     (if add-newline-p
                         (+ 2 loc)
                       loc))))
    (when str
      (if repeated-p
          (kill-new (concat
                     (substring str 0 location)
                     (current-kill 0 'DO-NOT-MOVE)
                     (substring str location (length str))))
        (kill-new str))
      (when (not buffer-read-only)
        (delete-region (car bounds) (cdr bounds))
        (let ((point (point))
              (blank-line-p (save-excursion
                              (beginning-of-line)
                              (re-search-forward "^[[:space:]]*$" (point-at-eol) t 1))))
          (when blank-line-p
            (delete-region blank-line-p (1+ (point-at-eol)))
            (indent-according-to-mode))
          (save-excursion
            (ram-remove-whitespace-between-regexps ram-close-delimiters-re ram-close-delimiters-re)))))))

;; adopted from https://github.com/abo-abo/lispy
(defun ram--swap-regions (bounds1 bounds2)
  "Swap buffer regions BOUNDS1 and BOUNDS2.
Return a cons of the new text cordinates."
  ;; cover cases for moving sexp up or down
  (when (> (car bounds1) (car bounds2))
    (cl-rotatef bounds1 bounds2))
  (let ((str1 (buffer-substring-no-properties (car bounds1) (cdr bounds1)))
        (str2 (buffer-substring-no-properties (car bounds2) (cdr bounds2))))
    (goto-char (car bounds2))
    (delete-region (car bounds2) (cdr bounds2))
    (insert str1)
    (when (ram-in-comment-p)
      (unless (eolp)
        (newline-and-indent)))
    (goto-char (car bounds1))
    (delete-region (car bounds1) (cdr bounds1))
    (insert str2)
    (goto-char (car bounds1)))
  (let* ((l1 (- (cdr bounds1) (car bounds1)))
         (l2 (- (cdr bounds2) (car bounds2)))
         (new-beg (+ (car bounds2) (- l2 l1)))
         (new-end (+ new-beg l1)))
    (cons
     (cons (car bounds1) (+ (car bounds1) l2))
     (cons new-beg new-end))))

(defun ram-move-sexp-up ()
  (interactive)
  (let* ((bounds1 (ram-thing-bounds))
         (limits (save-excursion
                   (deactivate-mark)
                   (goto-char (car bounds1))
                   (if (ignore-errors (up-list) t)
                       (ram-thing-bounds)
                     (cons (point-min) (point-max)))))
         ;; (at-end-p (ram-at-thing-end-p))
         (location (- (point) (car bounds1)))
         bounds2
         new-bounds)
    (goto-char (car bounds1))
    (when (re-search-backward "[^ \t\n]" (car limits) t)
      (progn
        (deactivate-mark)
        (forward-char)
        (setq bounds2 (ram-sexp-bounds))
        (setq new-bounds (ram--swap-regions bounds1 bounds2))
        (setq deactivate-mark nil)
        (goto-char (cdr bounds2))
        ;; (set-mark (point))
        ;; (backward-char (- (cdr bounds1) (car bounds1)))
        (goto-char (+ (caar new-bounds) location))
        ;; (if at-end-p
        ;;     (goto-char (cdar new-bounds))
        ;;   (goto-char (caar new-bounds)))
        ))))

(defun ram-move-sexp-down ()
  (interactive)
  (let* ((bounds1 (ram-thing-bounds))
         (limits (save-excursion
                   (deactivate-mark)
                   (goto-char (car bounds1))
                   (if (ignore-errors (up-list) t)
                       (ram-thing-bounds)
                     (cons (point-min) (point-max)))))
         ;; (at-end-p (ram-at-thing-end-p))
         (location (- (point) (car bounds1)))
         bounds2
         new-bounds)
    (goto-char (cdr bounds1))
    (when (re-search-forward "[^ \t\n]" (max (1- (cdr limits))
                                             (point)) t)
      (progn
        (deactivate-mark)
        (backward-char 1)
        (setq bounds2 (ram-sexp-bounds))
        (setq new-bounds (ram--swap-regions bounds1 bounds2))
        (setq deactivate-mark nil)
        (goto-char (cdr bounds2))
        ;; (set-mark (point))
        (goto-char (+ (cadr new-bounds) location))
        ;; (if at-end-p
        ;;     (goto-char (cddr new-bounds))
        ;;   (goto-char (cadr new-bounds)))
        ;; (backward-char (- (cdr bounds1) (car bounds1)))
        ))))

;;** brackets, parentheses, parens, sexps: move up, down

;;** brackets, parentheses, parens, sexps: bindings

;; ------
(define-key global-map (kbd "<end>" ) #'ram-mark-sexp)
(define-key global-map (kbd "<home>" ) #'ram-copy-sexp)

;; (define-key prog-mode-map (kbd "<end>" ) #'ram-mark-sexp)
;; (define-key prog-mode-map (kbd "<home>" ) #'ram-copy-sexp)

;; (define-key minibuffer-mode-map (kbd "<end>" ) #'ram-mark-sexp)
;; (define-key minibuffer-mode-map (kbd "<home>" ) #'ram-copy-sexp)

;; ------
(define-key global-map (kbd "<down>") #'ram-forward-list)
(define-key global-map (kbd "<up>") #'ram-backward-list)

;; (define-key prog-mode-map (kbd "<down>") #'ram-forward-list)
;; (define-key prog-mode-map (kbd "<up>") #'ram-backward-list)

;; (define-key minibuffer-mode-map (kbd "<down>") #'ram-forward-list)
;; (define-key minibuffer-mode-map (kbd "<up>") #'ram-backward-list)

;; ------
(define-key prog-mode-map (kbd "M-<down>" ) #'ram-move-sexp-down)
(define-key prog-mode-map (kbd "M-<up>" ) #'ram-move-sexp-up)

;; ------
(define-key global-map (kbd "S-<down>") #'ram-next-defun)
(define-key global-map (kbd "S-<up>") #'ram-prev-defun)

;; (define-key prog-mode-map (kbd "S-<down>") #'ram-next-defun)
;; (define-key prog-mode-map (kbd "S-<up>") #'ram-prev-defun)

;; ------
(define-key prog-mode-map (kbd "C-<down>" ) #'ram-clone-sexp-forward)
(define-key prog-mode-map (kbd "C-<up>" ) #'ram-clone-sexp-backward)

;; ------
(define-key global-map (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
(define-key global-map (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

;; (define-key prog-mode-map (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
;; (define-key prog-mode-map (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

;; (define-key minibuffer-mode-map (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
;; (define-key minibuffer-mode-map (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

;; ------
(define-key global-map (kbd "M-<left>") #'ram-jump-backward-to-close-delimiter)
(define-key global-map (kbd "M-<right>") #'ram-jump-forward-to-close-delimiter)

;; (define-key prog-mode-map (kbd "M-<left>") #'ram-jump-backward-to-close-delimiter)
;; (define-key prog-mode-map (kbd "M-<right>") #'ram-jump-forward-to-close-delimiter)

;; ------
(define-key global-map (kbd "S-<left>" ) #'ram-beg-of-top-sexp)
(define-key global-map (kbd "S-<right>" ) #'ram-end-of-top-sexp)

;; (define-key prog-mode-map (kbd "S-<left>" ) #'ram-beg-of-top-sexp)
;; (define-key prog-mode-map (kbd "S-<right>" ) #'ram-end-of-top-sexp)

;; ------
(define-key global-map (kbd "C-<left>" ) #'ram-up-list-backward)
(define-key global-map (kbd "C-<right>" ) #'ram-up-list-forward)

;; (define-key prog-mode-map (kbd "C-<left>" ) #'ram-up-list-backward)
;; (define-key prog-mode-map (kbd "C-<right>" ) #'ram-up-list-forward)

;; ------
;; (define-key prog-mode-map (kbd "C-," ) #'ram-kill-at-point)
(define-key global-map (kbd "C-," ) #'ram-kill-at-point)

(define-key prog-mode-map (kbd "C-:" ) #'ram-toggle-multiline-delimited-sexp)

;;* linters

;;* linters: flycheck-clj-kondo
;; First install the package:
;; (straight-use-package
;;  '(flycheck-clj-kondo :type git :flavor melpa :host github :repo "borkdude/flycheck-clj-kondo"))

;;* clojure

(straight-use-package
 '(clojure-mode :type git :flavor melpa
                :files ("clojure-mode.el" "clojure-mode-pkg.el")
                :host github :repo "clojure-emacs/clojure-mode"))

(defun ram-switch-to-clojure-repl ()
  "Switch to Clojure REPL if it is running.

If there is no Clojure REPL, send warning."
  (interactive)
  (let* ((repl-regexp "^\\*cider-repl .*$")
         (repl (car (seq-filter
                     (lambda (b) (string-match-p repl-regexp (buffer-name b)))
                     (buffer-list)))))
    (if repl
        (switch-to-buffer repl)
      (error "No Clojure REPL buffer found with name: %s" repl-regexp))))

(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)
  (define-key clojure-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  (define-key clojure-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  (define-key clojure-mode-map (kbd "s-E") #'ram-switch-to-clojure-repl))
  ;; (require 'flycheck-clj-kondo)


;;* python

;;** python: ide

(straight-use-package
 '(elpy :type git :flavor melpa
        :files ("*.el" "NEWS.rst" "snippets" "elpy" "elpy-pkg.el")
        :host github :repo "jorgenschaefer/elpy"))
(elpy-enable)

;;** python: outlines
(defun python-mode-outline-hook ()
  (setq outline-level 'python-outline-level)

  (setq outline-regexp
        (rx (or
             ;; Commented outline heading
             (group
              (* space)                 ; 0 or more spaces
              (one-or-more (syntax comment-start))
              (one-or-more space)
              ;; Heading level
              (group (repeat 1 8 "\*")) ; Outline stars
              (one-or-more space))

             ;; Python keyword heading
             (group
              ;; Heading level
              (group (* space))         ; 0 or more spaces
              bow
              ;; Keywords
              (or "class" "def" "else" "elif" "except" "for" "if" "try" "while")
              eow)))))

(defun python-outline-level ()
  (or
   ;; Commented outline heading
   (and (string-match (rx
                       (* space)
                       (one-or-more (syntax comment-start))
                       (one-or-more space)
                       (group (one-or-more "\*"))
                       (one-or-more space))
                      (match-string 0))
        (- (match-end 0) (match-beginning 0)))

   ;; Python keyword heading, set by number of indentions
   ;; Add 8 (the highest standard outline level) to every Python keyword heading
   (+ 8 (- (match-end 0) (match-beginning 0)))))

;; (with-eval-after-load "outline"
;;   (define-key outline-minor-mode-map (kbd "<tab>") #'bicycle-cycle))

(add-hook 'python-mode-hook #'outline-minor-mode)

(add-hook 'python-mode-hook 'python-mode-outline-hook)

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "<M-f5>") 'ram-jump-to-outline))
  ;; (define-key python-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  ;; (define-key python-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  ;; (require 'flycheck-clj-kondo)


;;* prolog

;;** prolog: prolog-mode

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)  ; optional, the system you are using;
                           ; see `prolog-system' below for possible values

;; (setq prolog-electric-if-then-else-flag t)

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(with-eval-after-load "prolog"
  (define-key prolog-mode-map (kbd "C-c l") (lambda ()
                                              (interactive)
                                              (insert ":- use_module(library(()).")
                                              (forward-char -3))))

;;** prolog: ediprolog

(straight-use-package
 '(ediprolog :type git :host github :repo "emacs-straight/ediprolog" :files ("*" (:exclude ".git"))))

(with-eval-after-load "ediprolog"
  (define-key prolog-mode-map (kbd "C-x C-e") #'ediprolog-dwim)
  (define-key prolog-mode-map (kbd "C-M-x") #'ediprolog-dwim)

  (custom-set-variables
   '(ediprolog-system 'swi)
   '(ediprolog-program "/usr/bin/swipl")))

;;* super-save

;; (use-package super-save
;;   :config
;;   ;; save on find-file
;;   (add-to-list 'super-save-hook-triggers 'find-file-hook)
;;   (add-to-list 'super-save-triggers 'switch-to-prev-buffer)
;;   (add-to-list 'super-save-triggers 'switch-to-next-buffer)
;;   (add-to-list 'super-save-triggers 'winner-undo)
;;   (add-to-list 'super-save-triggers 'winner-redo)
;;   (add-to-list 'super-save-triggers 'counsel-M-x)
;;   (super-save-mode 1))

(straight-use-package
 '(super-save :type git :flavor melpa :host github :repo "bbatsov/super-save"))
(run-with-idle-timer 1 nil #'super-save-mode)
(with-eval-after-load 'super-save
  (add-hook 'after-init-hook 'super-save-mode)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  ;; TODO: the following does not seem to have an effect
  (add-to-list 'super-save-triggers 'execute-extended-command)
  (add-to-list 'super-save-triggers 'switch-to-prev-buffer)
  (add-to-list 'super-save-triggers 'switch-to-next-buffer)
  (add-to-list 'super-save-triggers 'winner-undo)
  (add-to-list 'super-save-triggers 'winner-redo)
  (add-to-list 'super-save-triggers 'counsel-M-x)

  (add-to-list 'super-save-triggers #'org-roam-node-find)

  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-today)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-tomorrow)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-yesterday)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-date)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-next-note)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-previous-note)

  (add-to-list 'super-save-triggers #'org-roam-dailies-capture-today)
  (add-to-list 'super-save-triggers #'org-roam-dailies-capture-tomorrow)
  (add-to-list 'super-save-triggers #'org-roam-dailies-capture-yesterday)
  (add-to-list 'super-save-triggers #'org-roam-dailies-capture-date)

  (add-to-list 'super-save-triggers #'org-agenda))

;;* ivy, swiper, counsel:
;; https://oremacs.com/swiper/
;; installing counsel would install swipel, ivy as dependencies
(straight-use-package
 '(counsel :type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper"))
;;** ivy
;;*** ivy bindings

;;*** ivy Settings
;; (ivy-mode 1)
(setq ivy-use-virtual-buffers t)
;; credit to https://gitlab.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/emacs-init.org
;; gives an error: Debugger entered--Lisp error: (wrong-type-argument symbolp "%-4d ")
;; (set ivy-count-format "(%d/%d) ")
(setq ivy-height-alist '((t lambda (_caller) (/ (window-height) 4))))
(setq ivy-wrap nil)
(setq ivy-display-style 'fancy)
(setq ivy-use-selectable-prompt t)
(setq ivy-fixed-height-minibuffer nil)

(with-eval-after-load 'ivy

  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  (ivy-set-occur 'swiper-multi 'swiper-occur) ; TODO does not work

  (add-hook 'ivy-occur-mode #'hl-line-mode)

  (define-key ram-leader-map-tap-global (kbd "<up>") 'ivy-push-view)
  (define-key ram-leader-map-tap-global (kbd "<down>") 'ivy-switch-view)
  (define-key global-map (kbd "C-S-r") 'ivy-resume)

  (define-key ivy-occur-mode-map  (kbd "f") 'forward-char)
  (define-key ivy-occur-mode-map  (kbd "b") 'backward-char)
  (define-key ivy-occur-mode-map  (kbd "n") 'ivy-occur-next-line)
  (define-key ivy-occur-mode-map  (kbd "p") 'ivy-occur-previous-line)
  (define-key ivy-occur-mode-map  (kbd "<C-return>") 'ivy-occur-press))

(define-key global-map (kbd "C-c C-r") 'ivy-resume)

;;*** ivy packages

;;**** ivy: ivy-dired-history

;; (straight-use-package
;;  '(ivy-dired-history :type git :flavor melpa :host github :repo "jixiuf/ivy-dired-history"))

;; (with-eval-after-load 'dired
;;   (require 'ivy-dired-history)
;;   ;; if you are using ido,you'd better disable ido for dired
;;   ;; (define-key (cdr ido-minor-mode-map-entry) [remap dired] nil) ;in ido-setup-hook
;;   (define-key dired-mode-map "," 'dired))

;;**** ivy: ivy-rich

;; (straight-use-package
;;  '(ivy-rich :type git :flavor melpa :files ("*.el" "ivy-rich-pkg.el") :host github :repo "Yevgnen/ivy-rich"))
;; (setcdr (assq t ivy-format-functions-alist)
;;         #'ivy-format-function-line)
;; (ivy-rich-mode 1)
;; (add-hook 'after-init-hook 'ivy-rich-mode)

;;**** ivy: ivy-posframe

(straight-use-package
 '(ivy-posframe :type git :flavor melpa :host github :repo "tumashu/ivy-posframe"))
(setq ivy-posframe-height-alist
      '((swiper . 15)
        (swiper-isearch . 15)
        (t . 10)))
(setq ivy-posframe-display-functions-alist
      '((complete-symbol . ivy-posframe-display-at-point)
        (swiper . nil)
        (ivy-switch-buffer-other-window . ivy-posframe-display-at-frame-center)
        (ivy-switch-buffer . ivy-posframe-display-at-frame-center)
        (counsel-describe-variable . ivy-posframe-display-at-frame-center)
        (counsel-describe-function . ivy-posframe-display-at-frame-center)
        (counsel-find-file . ivy-posframe-display-at-frame-center)
        (counsel-M-x . ivy-posframe-display-at-frame-center)
        (describe-symbol . ivy-posframe-display-at-frame-center)
        (display-buffer . ivy-posframe-display-at-frame-center)
        ;; (swiper-isearch . nil)
        ;; (t . ivy-posframe-display-at-frame-center)
        (t . nil)))

(setq ivy-posframe-parameters
      '((width . 90)))

(ivy-posframe-mode 1)

;;**** ivy: prescient

(straight-use-package
 '(prescient :type git :flavor melpa :files ("prescient.el" "prescient-pkg.el") :host github :repo "raxod502/prescient.el"))

(with-eval-after-load 'counsel
  (require 'prescient))

(with-eval-after-load "prescient"
  (setq prescient-history-length 50)
  (setq prescient-save-file "~/.emacs.d/prescient-items")
  ;; (setq prescient-filter-method '(literal regexp fuzzy))
  (setq prescient-filter-method '(literal regexp initialism))
  (prescient-persist-mode 1))

(straight-use-package
 '(ivy-prescient :type git :flavor melpa :files ("ivy-prescient.el" "ivy-prescient-pkg.el") :host github :repo "raxod502/prescient.el"))
(ivy-prescient-mode 1)

(with-eval-after-load 'prescient
  (with-eval-after-load 'counsel
    (with-eval-after-load 'ivy
      (defun prot/ivy-prescient-filters (str)
        "Specify an exception for `prescient-filter-method'.
This new rule can be used to tailor the results of individual
Ivy-powered commands, using `ivy-prescient-re-builder'."
        (let ((prescient-filter-method '(literal regexp)))
          (ivy-prescient-re-builder str)))
      (setq ivy-re-builders-alist
            '((counsel-rg . prot/ivy-prescient-filters)
              (counsel-grep . prot/ivy-prescient-filters)
              (counsel-yank-pop . prot/ivy-prescient-filters)
              (swiper . prot/ivy-prescient-filters)
              (swiper-isearch . prot/ivy-prescient-filters)
              (swiper-all . prot/ivy-prescient-filters)
              (ivy-org-jump-to-headline . prot/ivy-prescient-filters)
              (ram-jump-to-outline . prot/ivy-prescient-filters)
              (ivy-switch-buffer . prot/ivy-prescient-filters)
              (t . ivy-prescient-re-builder)))
      (setq ivy-prescient-sort-commands
            '(:not swiper ivy-switch-buffer counsel-switch-buffer))
      (setq ivy-prescient-retain-classic-highlighting t)
      (setq ivy-prescient-enable-filtering t)
      (setq ivy-prescient-enable-sorting t))))

;;** swiper

(eval-after-load "ivy"
  '(progn
     (setq swiper-action-recenter t)
     (setq swiper-goto-start-of-match t)
     (setq swiper-include-line-number-in-search t)
     (define-key global-map (kbd "C-S-s") 'swiper)
     (define-key global-map (kbd "M-s-s") 'swiper-multi)
     (define-key global-map (kbd "M-s-w") 'swiper-thing-at-point)))

;;** counsel

(with-eval-after-load "ivy"
  (defun prot/counsel-fzf-rg-files (&optional input dir)
    "Run `fzf' in tandem with `ripgrep' to find files in the
present directory.  If invoked from inside a version-controlled
repository, then the corresponding root is used instead."
    (interactive)
    (let* ((process-environment
            (cons (concat "FZF_DEFAULT_COMMAND=rg -Sn --color never --files --no-follow --hidden")
                  process-environment))
           (vc (vc-root-dir)))
      (if dir
          (counsel-fzf input dir)
        (if (eq vc nil)
            (counsel-fzf input default-directory)
          (counsel-fzf input vc)))))

  (defun prot/counsel-fzf-dir (arg)
    "Specify root directory for `counsel-fzf'."
    (prot/counsel-fzf-rg-files ivy-text
                               (read-directory-name
                                (concat (car (split-string counsel-fzf-cmd))
                                        " in directory: "))))

  (defun prot/counsel-rg-dir (arg)
    "Specify root directory for `counsel-rg'."
    (let ((current-prefix-arg '(4)))
      (counsel-rg ivy-text nil "")))

  ;; TODO generalise for all relevant file/buffer counsel-*?
  (defun prot/counsel-fzf-ace-window (arg)
    "Use `ace-window' on `prot/counsel-fzf-rg-files' candidate."
    (ace-window t)
    (let ((default-directory (if (eq (vc-root-dir) nil)
                                 counsel--fzf-dir
                               (vc-root-dir))))
      (if (> (length (aw-window-list)) 1)
          (progn
            (find-file arg))
        (find-file-other-window arg))
      (balance-windows)))
  ;; Pass functions as appropriate Ivy actions (accessed via M-o)
  (ivy-add-actions
   'counsel-fzf
   '(("r" prot/counsel-fzf-dir "change root directory")
     ("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("a" prot/counsel-fzf-ace-window "ace-window switch")))

  (ivy-add-actions
   'counsel-rg
   '(("r" prot/counsel-rg-dir "change root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  (ivy-add-actions
   'counsel-find-file
   '(("g" prot/counsel-rg-dir "use ripgrep in root directory")
     ("z" prot/counsel-fzf-dir "find file with fzf in root directory")))

  ;; Remove commands that only work with key bindings
  (put 'counsel-find-symbol 'no-counsel-M-x t))

(setq counsel-yank-pop-preselect-last t)
(setq counsel-yank-pop-separator "\n—————————\n")
(setq counsel-rg-base-command
 "rg -SHn --no-heading --color never --no-follow --hidden %s")
(setq counsel-find-file-occur-cmd            ; TODO Simplify this
 "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")

;;*** counsel: bindings

;; (progn

;;   (define-key global-map (kbd "M-x") 'counsel-M-x)
;;   (define-key global-map (kbd "C-x f") 'counsel-find-file)
;;   (define-key global-map (kbd "C-x C-f") 'counsel-find-file)
;;   (define-key global-map (kbd "C-x d") 'counsel-dired)

;;   (define-key global-map (kbd "C-x b") 'counsel-switch-buffer)
;;   (define-key global-map (kbd "C-x B") 'counsel-switch-buffer-other-window)

;;   (define-key global-map (kbd "C-x C-r") 'counsel-recentf)
;;   (define-key global-map (kbd "C-h f") 'counsel-describe-function)
;;   (define-key global-map (kbd "C-h v") 'counsel-describe-variable)

;;   (define-key global-map (kbd "s-f") 'counsel-find-file)
;;   (define-key global-map (kbd "s-F") 'find-file-other-window)

;;   ;; the preview of the buffer slows down the 'counsel-switch-buffer
;;   ;; (define-key global-map (kbd "s-b") 'counsel-switch-buffer)

;;   (define-key global-map (kbd "s-b") 'ivy-switch-buffer)
;;   ;; seems like my custom open buffers messed up 'counsel-switch-buffer-other-window
;;   ;; buffer preview functionality: it opens it in a new frame.
;;   ;; (define-key global-map (kbd "s-B") 'counsel-switch-buffer-other-window)
;;   (define-key global-map (kbd "s-B") 'ivy-switch-buffer-other-window)
;;   (define-key global-map (kbd "C-s-b") 'display-buffer)

;;   (define-key ram-leader-map-tap-global (kbd "y") 'counsel-yank-pop)

;;   ;; counsel-dired does not include prescient entries
;;   (define-key global-map (kbd "s-l") 'dired)
;;   (define-key global-map (kbd "s-L") 'dired-other-window)
;;   ;; (define-key global-map (kbd "s-r") 'counsel-recentf)
;;   ;; (define-key global-map (kbd "s-M-z") 'prot/counsel-fzf-rg-files)
;;   ;; (define-key global-map (kbd "s-M-r") 'counsel-rg)
;;   (define-key global-map (kbd "s-M-g") 'counsel-git-grep)

;;   (define-key ivy-minibuffer-map (kbd "C-r") 'counsel-minibuffer-history)
;;   (define-key ivy-minibuffer-map (kbd "s-y") 'ivy-next-line) ; Avoid 2× `counsel-yank-pop'
;;   (define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches))

;; (define-key global-map (kbd "<f1> l") 'counsel-find-library)
;; (define-key global-map (kbd "<f2> u") 'counsel-unicode-char)
;; (define-key global-map (kbd "C-c g") 'counsel-git)
;; (define-key global-map (kbd "C-c j") 'counsel-git-grep)
;; (define-key global-map (kbd "C-c k") 'counsel-ag)

;;* mode-line

;; update vc-mode info (e.g., current branch)
(setq auto-revert-check-vc-info t)
;; default is 5 seconds
(setq auto-revert-interval 3)

;;** mode-line: selected-window

;; credit to https://emacs.stackexchange.com/a/13874/31822

(defvar ram-selwin nil)

(defun ram-get-selected-window (windows)
  (when (not (minibuffer-window-active-p (frame-selected-window)))
    (setq ram-selwin (selected-window))))

(add-function :before pre-redisplay-function #'ram-get-selected-window)

;;** mode-line: bitcoin price ticker

;; based on https://github.com/niedbalski/emacs-btc-ticker

;; (require 'json)
;; (require 'request)

;; (request
;;  "https://www.bitstamp.net/api/ticker/"
;;  :parser 'json-read
;;  :success (function*
;;            (lambda(&key data &allow-other-keys)
;; 	     (setq btc-ticker-mode-line
;; 		   (concat " $" (assoc-default 'last data))))))

;;** mode-line: battery

(customize-set-variable 'battery-mode-line-format "%b%p%%%")

(display-battery-mode t)

;; (assoc "Percentage" (dbus-get-all-properties :system
;;                           "org.freedesktop.UPower"
;;                           (expand-file-name
;;                            (setq bat0 (car (dbus-call-method :system
;;                                                              "org.freedesktop.UPower"
;;                                                              "/org/freedesktop/UPower"
;;                                                              "org.freedesktop.UPower"
;;                                                              "EnumerateDevices")))
;;                            "/org/freedesktop/UPower")
;;                           "org.freedesktop.UPower.Device"))

(defun ram-get-battery-status ()
  "Modify `battery-mode-line-string'."
  ;; refresh dbus service for devices
  ;; in my experience, it gets stuck sometimes
  ;; it seems to cause some misbehavior in exwm
  ;; (cl-dolist (device (dbus-call-method
  ;;                     :system
  ;;                     "org.freedesktop.UPower"
  ;;                     "/org/freedesktop/UPower"
  ;;                     "org.freedesktop.UPower"
  ;;                     "EnumerateDevices"))
  ;;   (dbus-call-method :system
  ;;                     "org.freedesktop.UPower"
  ;;                     (expand-file-name  device "/org/freedesktop/UPower")
  ;;                     "org.freedesktop.UPower.Device"
  ;;                     "Refresh"))
  (if (string-match "[0-9.]+" battery-mode-line-string)
      (let* ((matched-number (match-string 0 battery-mode-line-string))
             (charge-percent (floor (string-to-number matched-number)))
             (new-str (replace-regexp-in-string (regexp-quote matched-number)
                                                (number-to-string charge-percent)
                                                battery-mode-line-string
                                                nil
                                                'literal)))
        (if (< charge-percent 20)
            (propertize new-str 'face '((:foreground "brown2")))
          (propertize new-str 'face '((:foreground "PaleGreen3")))))))

;;** mode-line: cpu temp

(defvar ram-sensors-cmd-output nil)
(defvar ram-shell-sensors-cmd-name "ram-shell-cpu-temp"
  "A name for a shell process to query cpu temperatures.")
(defvar ram-cpu-temp-mode-line-str nil)

(defvar sh-proc (make-process
                 :name ram-shell-sensors-cmd-name
                 :buffer nil
                 :command '("bash")
                 :connection-type 'pipe
                 :filter (lambda (proc output)
                           (setq ram-sensors-cmd-output (concat output ram-sensors-cmd-output)))))

(defun ram-get-cpu-temp ()
  "Set `ram-cpu-temp-mode-line-str' to current cpu temperature."
  (let ((output ram-sensors-cmd-output))
    (setq ram-sensors-cmd-output nil)
    (process-send-string ram-shell-sensors-cmd-name "sensors\n")
    (when output
      (let* ((temps
              (mapcar
               (lambda (l) (floor (string-to-number (car (split-string (cadr l))))))
               (seq-filter (lambda (l) (cl-member "Core [0-9]+" l :test #'string-match-p))
                           (mapcar (lambda (s) (split-string s ":" t "[ ]+"))
                                   (split-string output "\n" t "[ \f\t\r\v]+")))))
             (average (/ (cl-reduce #'+ temps) (length temps)))
             (temp-str (if (< average 60)
                           (propertize (number-to-string average)
                                       'face '((:foreground "pink2")))
                         (propertize (number-to-string average)
                                     'face '((:foreground "brown2")))))
             mode-line-cpu-temp)
        (setq ram-cpu-temp-mode-line-str temp-str)))))

;;** mode-line: memory

(defvar ram-free-cmd-output nil
  "Holds the modified output of linux \"free\" command. ")
(defvar ram-shell-free-cmd-name "ram-shell-free"
  "A name for a shell process to run \"free\" command.")
(defvar ram-memory-mode-line-str nil)

(defvar sh-memory-query-proc
  (make-process
   :name ram-shell-free-cmd-name
   :buffer nil
   :command '("bash")
   :connection-type 'pipe
   :filter (lambda (proc output)
             (setq ram-free-cmd-output (concat output ram-free-cmd-output)))))

(defun ram-get-memory-stats ()
  "Set `ram-memory-mode-line-str' to current memory stats."
  (let ((output ram-free-cmd-output))
    (setq ram-free-cmd-output nil)
    (process-send-string ram-shell-free-cmd-name "free -m\n")
    (when output
      (let ((mem-output
             (mapcar (lambda (s)
                       (let ((l (split-string s)))
                         (cons (car l) (mapcar #'string-to-number (cdr l)))))
                     (split-string output "\n" t "[ \f\t\r\v]+"))))
        (setq ram-memory-mode-line-str
              (format " %4.1f "
                      (/ (nth 5 (cdr (assoc "Mem:" mem-output))) 1000.0)))))))

;;** mode-line: number of monitors
(defvar ram-number-of-monitors (string-to-number
                                (shell-command-to-string
                                 "xrandr --listmonitors | awk 'NR==1{printf \"%s\",$2}'"))
  "Holds the number of connected monitors according to \"xrandr\" command.")

;;** mode-line: time

(display-time-mode t)

;; Time format
;; (setq display-time-format "%H%M")
(customize-set-variable 'display-time-string-forms
                        '((concat day dayname
                                  " " 24-hours ":" minutes)))

;; Update display-time-string
(display-time-update)

;;** mode-line: version control (vc)

(defface my/mode:vc-added
  `(
    (((class color))
     (:background "#FFAA55" :foreground "black"))
    (t
     (:weight bold :underline t)))
  "VC status tag face for files that have just been added to
version-control."
  :group 'MY/mode)

(defface my/mode:vc-edited
  `(
    (((class color))
     (:background "#F05B80" :foreground "black")) ; "#F04040" maybe?
    (t
     (:weight bold :underline t)))
  "VC status tag face for files that are under version control
but which have been edited."
  :group 'MY/mode)

(defface my/mode:vc-in-sync
  `(
    (((class color))
     (:background "#60CC60" :foreground "black"))
    (t
     (:weight bold :underline t)))
  "VC status tag face for files that are under version control
and which are in sync with the respository."
  :group 'MY/mode)

(defface my/mode:vc-none
  `(
    (((class color))
     (:background "#70A0D0" :foreground "black"))
    (t
     (:weight bold :underline t)))
  "VC status tag face for files that are not under version
control"
  :group 'MY/mode)

(defface my/mode:vc-unknown
  `(
    (((class color))
     (:background "#FF0000" :foreground "white"))
    (t
     (:weight bold :underline t)))
  "VC status tag face for files whose version-control status
cannot be determined."
  :group 'MY/mode)

(defvar my-vc-mode-attrs
  '(("" . (" NoVC " my/mode:vc-none))
    ("-" . (" VC = " my/mode:vc-in-sync))
    (":" . (" VC > " my/mode:vc-edited))
    ("@" . (" VC + " my/mode:vc-added))
    ("?" . (" ?VC? " my/mode:vc-unknown)))
  "Lookup table to translate vc-mode character into another string/face.")

;; This function helps me understand the version-control status.
(defun my-mode-line-vc-info ()
  "Return version-control status information about the file in
the current buffer, as a fontified string.

The mode-line variable `vc-mode' is nil if the file is not under
version control, and displays a hyphen or a colon depending on whether
the file has been modified since check-in.  I can never keep those
straight.

This function returns \"NoVC\" if the file is not under version
control.  It displays a string with an = sign if the file is in sync
with its version control, and a string with a > sign if the file has
been modified since its last check-in."
  (let* ((class
          (cond
           ;; If not under version-control
           ((not vc-mode)
            "")

           ;; If under version-control decode the -:@ character
           ((string-match "\\` ?\\(?:CVS\\|Git\\)\\([-:@]\\)\\([^^:~ \x00-\x1F\\\\]+\\)?" vc-mode)
            (match-string-no-properties 1 vc-mode))))


           ;;  ;; Otherwise, indicate confusion
           ;;  (t
           ;;   "?")


         (branch
          (if (member class '("-" ":" "@"))
              (concat " " (match-string-no-properties 2 vc-mode))
            " NoVC"))

         ;; Fetch properties list for the class character above
         (props (cdr (assoc class my-vc-mode-attrs))))
    ;; (cadr (cdr (assoc ":" my-vc-mode-attrs)))

    ;; (concat (propertize (car props) 'face (cadr props))
    ;;         branch)
    (concat (propertize (format "%s " branch) 'face (cadr props)))))

;;** mode-line: 'face

(face-spec-set 'mode-line
               `((((class color) (min-colors 88))
                  ;; :box (:line-width 2 :color "grey55" :style nil)
                  :box nil
                  :weight light
                  :background "grey55" :foreground "black"
                  :height
                  ,(if (>= (x-display-pixel-width) large-sreen-width)
                       155
                     100))))

(face-spec-set 'mode-line-inactive
               `(;; (default
                 ;;   :inherit mode-line)
                 (((class color) (min-colors 88))
                  ;; :box (:line-width 2 :color "grey90" :style nil)
                  :box nil
                  :weight light
                  :foreground "grey20" :background "grey90"
                  :height
                  ,(if (>= (x-display-pixel-width) large-sreen-width)
                       155
                     100))))

;;** mode-line: timer

;; (cancel-timer mode-line-timer)
(run-at-time 1 nil #'(lambda () (force-mode-line-update t)))
(defvar mode-line-timer
  (run-with-timer 2 4 #'(lambda () (force-mode-line-update t))))
(defvar mode-line-cpu-temp-timer
  (run-with-timer 2 3 #'ram-get-cpu-temp))
(defvar mode-line-memory-stats-timer
  (run-with-timer 2 3.1 #'ram-get-memory-stats))

;;** mode-line: format

;; credit to:
;; https://github.com/xiongtx/.emacs.d/blob/347d9990a394fbcb222e4cda9759743e17b1977a/init.org#mode-line
;; use this function to right align mode-line input
(defun *-mode-line-fill (reserve)
  "Return empty space using FACE and leaving RESERVE space on the right."
  (unless reserve
    (setq reserve 20))
  (when (and window-system
             (eq 'right (get-scroll-bar-mode)))
    (setq reserve (- reserve 3)))
  (propertize " "
              'display `((space :align-to (- (+ right right-fringe right-margin) ,reserve)))))

(setq-default mode-line-format
              '(
                ;; show workspaces

                (:eval (if (and (window-at-side-p (get-buffer-window) 'bottom)
                                (window-at-side-p (get-buffer-window) 'left))
                           (propertize (format " %s " (exwm-workspace--position
                                                       (window-frame (get-buffer-window))))
                                       'face (if (eq ram-selwin (get-buffer-window))
                                                 '((:foreground "green1"))
                                               '((:foreground "green4"))))
                         "   "))
                (:eval (when (buffer-narrowed-p)
                         (progn (propertize " %n " 'face '((:background "green"))))))

                ;; on remote machine?
                "[" "%@" "]"
                ;; mode-line-modified
                ;; %, * or - for read only, changed, saved
                " %*%*%* "
                ;; mode-line-buffer-identification

                (:eval (propertize "%b " 'face font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
                (:eval (if (boundp 'git-gutter-mode)
                           (propertize (format "+%s " (car (git-gutter:statistic)))
                                       'face '((:foreground "chartreuse3")))))
                (:eval (if (boundp 'git-gutter-mode)
                           (propertize (format "-%s " (cdr (git-gutter:statistic)))
                                       'face '((:foreground "chocolate")))))
                (:eval (my-mode-line-vc-info))

                ;; (:eval (propertize vc-mode 'face '((:foreground "DarkGoldenrod2"))))
                ;; line and column
                " ("
                "%02l" "," "%01c" "," "%01P%"
                ") "
                ;; value of `mode-name'
                "%["
                (:eval (propertize " %m " 'face '((:foreground "plum3"))))
                "%]"
                mode-line-process
                ;; mode-line-modes
                ;; "-- user: "
                ;; value of user
                ;; (getenv "USER")
                " -- "

                (:eval
                 ;; display only if frame is right of other frame or there is only one monitor
                 (when (or
                        ;; there is only one monitor
                        (= 1 ram-number-of-monitors)
                        ;; frame X coord is not 0, thus, it is not the top left frame
                        (not (= 0 (car (frame-position (window-frame (get-buffer-window)))))))
                   (when (and (window-at-side-p (get-buffer-window) 'bottom)
                              (window-at-side-p (get-buffer-window) 'right))
                     (let* ((mem (propertize (or ram-memory-mode-line-str "")
                                             'face (if (eq ram-selwin (get-buffer-window))
                                                       '((:foreground "grey90"))
                                                     '((:foreground "grey60")))))
                            (cpu-temp (or ram-cpu-temp-mode-line-str ""))
                            (bat (ram-get-battery-status))
                            (time (propertize display-time-string
                                              'face (if (eq ram-selwin (get-buffer-window))
                                                        '((:foreground "grey90"))
                                                      '((:foreground "grey60")))))
                            (right-align-str (*-mode-line-fill (+ (length mem)
                                                                  (length cpu-temp)
                                                                  1
                                                                  (length bat)
                                                                  (length time)))))
                       (format "%s%s%s %s %s "
                               right-align-str
                               mem
                               cpu-temp
                               bat
                               time)))))))

;;* themes

(defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;** themes: modus-themes

;; consult https://protesilaos.com/emacs/modus-themes for customization
;; starting Emacs 28.1 the Modus themes are built in.

(progn
  ;; (setq modus-operandi-theme-diffs nil)
  (setq modus-operandi-theme-diffs 'desaturated)
  ;; (setq modus-operandi-theme-diffs 'fg-only)
  (setq modus-themes-hl-line '(accented intense underline))
  (load-theme 'modus-operandi)
  ;; (load-theme 'modus-vivendi)
  )

;;* search

;;** search: functions

(defun prot/find-file-vc-or-dir (&optional arg)
    "Find file by name that belongs to the current project or dir.
With \\[universal-argument] match files by contents.  This
requires the command-line executable called 'rg' or 'ripgrep'."
    (interactive "P")
    (let* ((default-directory (file-name-directory
                               (or (locate-dominating-file "." ".git")
                                   default-directory))))
      (if arg
          (let* ((regexp (read-regexp
                          (concat "File contents matching REGEXP in "
                                  (propertize default-directory 'face 'bold)
                                  ": ")))
                 (results (process-lines "rg" "-l" "--hidden" "-m" "1" "-M" "120" regexp)))
            (find-file
             (icomplete-vertical-do ()
               (completing-read (concat
                                 "Files with contents matching "
                                 (propertize regexp 'face 'success)
                                 (format " (%s)" (length results))
                                 ": ")
                                results nil t))))
        (let* ((filenames-all (directory-files-recursively default-directory ".*" nil t))
               (filenames (cl-remove-if (lambda (x)
                                          (string-match-p "\\.git" x))
                                        filenames-all)))
          (icomplete-vertical-do ()
            (find-file
             (completing-read "Find file recursively: " filenames nil t)))))))

(define-key global-map (kbd "M-s f") #'prot/find-file-vc-or-dir)

;;* isearch

;;** isearch: settings

;; always exit isearch when <RET> is pressed.
;; even if the search string is empty
(setq search-nonincremental-instead nil)
(setq isearch-hide-immediately nil)
;; default mode to use when starting isearch
;; (setq search-default-mode #'char-fold-to-regexp)
;; credit to:
;; https://gitlab.com/protesilaos/dotfiles/blob/master/emacs/.emacs.d/emacs-init.org
(setq search-whitespace-regexp ".*?")
(setq isearch-lax-whitespace t)
(setq isearch-regexp-lax-whitespace nil)

;;** isearch: functions

(defun prot/isearch-mark-and-exit ()
  "Marks the current search string.  Can be used as a building
block for a more complex chain, such as to kill a region, or
place multiple cursors."
  (interactive)
  (push-mark isearch-other-end t 'activate)
  (setq deactivate-mark nil)
  (isearch-done))

(defun stribb/isearch-region (&optional not-regexp no-recursive-edit)
  "If a region is active, make this the isearch default search
pattern."
  (interactive "P\np")
  (when (use-region-p)
    (let ((search (buffer-substring-no-properties
                   (region-beginning)
                   (region-end))))
      (message "stribb/ir: %s %d %d" search (region-beginning) (region-end))
      (setq deactivate-mark t)
      (isearch-yank-string search))))
(advice-add 'isearch-forward-regexp :after 'stribb/isearch-region)
(advice-add 'isearch-forward :after 'stribb/isearch-region)
(advice-add 'isearch-backward-regexp :after 'stribb/isearch-region)
(advice-add 'isearch-backward :after 'stribb/isearch-region)

(defun contrib/isearchp-remove-failed-part-or-last-char ()
  "Remove failed part of search string, or last char if successful.
Do nothing if search string is empty to start with."
  (interactive)
  (if (equal isearch-string "")
      (isearch-update)
    (if isearch-success
        (isearch-delete-char)
      (while (isearch-fail-pos) (isearch-pop-state)))
    (isearch-update)))

(defun contrib/isearch-done-opposite-end (&optional nopush edit)
  "End current search in the opposite side of the match.
Particularly useful when the match does not fall within the
confines of word boundaries (e.g. multiple words)."
  (interactive)
  (funcall #'isearch-done nopush edit)
  (when isearch-other-end (goto-char isearch-other-end)))

(define-key global-map (kbd "M-s M-o") 'multi-occur)
(define-key isearch-mode-map (kbd "C-SPC")  'prot/isearch-mark-and-exit)
(define-key isearch-mode-map (kbd "DEL")  'contrib/isearchp-remove-failed-part-or-last-char)
(define-key isearch-mode-map (kbd "<C-return>")  'contrib/isearch-done-opposite-end)

;; Prevents issue where you have to press backspace twice when
;; trying to remove the first character that fails a search
;; credit to https://stackoverflow.com/a/36707038/9913235
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;;*** isearch/functions: ram-isearch-done-to-open/close-paren

(defun ram-isearch-done-to-open-paren (&optional nopush edit)
  "End current search at open parens that wrap the match."
  (interactive)
  (funcall #'isearch-done nopush edit)
  (if (and
       ;; depth in parens is greater than 0
       (> (nth 0 (syntax-ppss)) 0)
       ;; not in string
       (not (nth 3 (syntax-ppss))))
      (backward-up-list 1)))

(defun ram-isearch-done-to-close-paren (&optional nopush edit)
  "End current search at open parens that wrap the match."
  (interactive)
  (funcall #'isearch-done nopush edit)
  (if (and
       ;; depth in parens is greater than 0
       (> (nth 0 (syntax-ppss)) 0)
       ;; not in string
       (not (nth 3 (syntax-ppss))))
      (backward-up-list -1)))

;; (define-key isearch-mode-map (kbd "[") #'ram-isearch-done-to-open-paren)
;; (define-key isearch-mode-map (kbd "]") #'ram-isearch-done-to-close-paren)

;;* packages

;;** packages: ace-link

(straight-use-package
 '(ace-link :type git :flavor melpa :host github :repo "abo-abo/ace-link"))

;;** packages: diff

;; credit to https://gitlab.com/protesilaos/dotfiles/-/tree/master
;; (autoload 'dired-mode "dired")
(defun prot/diff-buffer-with-file (&optional arg)
    "Compare buffer to its file, else run `vc-diff'.
With \\[universal-argument] also enable highlighting of word-wise
changes, local to the current buffer."
    (interactive "P")
    (let ((buf nil))   ; this method will "fail" if multi diff buffers
      (if (buffer-modified-p)
          (progn
            (diff-buffer-with-file (current-buffer))
            (setq buf "*Diff*"))
        (vc-diff)
        (setq buf "*vc-diff*"))
      (when arg
        (with-current-buffer (get-buffer buf)
          (setq-local diff-refine 'font-lock)))))

;; `prot/diff-buffer-with-file' replaces the default for `vc-diff'
;; (which I bind to another key---see VC section).
(define-key global-map (kbd "C-x v =") #'prot/diff-buffer-with-file)

(with-eval-after-load 'diff
  (setq diff-default-read-only t)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  ;; The following are from Emacs 27.1
  (setq diff-refine nil)                ; I do it on demand
  (setq diff-font-lock-prettify nil)    ; better for patches
  (setq diff-font-lock-syntax nil)      ; good for accessibility



  (defun prot/diff-refine-buffer ()
    "Produce word-wise, 'refined' diffs in `diff-mode' buffer.
Also see `prot/diff-refine-hunk-or-buf' that is a wrapper for the
current command."
    (interactive)
    (let ((position (point)))
      (when (derived-mode-p 'diff-mode)
        (setq-local diff-refine 'font-lock)
        (font-lock-flush (point-min) (point-max))
        (goto-char position))))

  (defun prot/diff-refine-hunk-or-buf (&optional arg)
    "Apply word-wise, 'refined' diffs to hunk or buffer.
With prefix ARG (\\[universal-argument]), refine the entire
buffer, else the diff hunk at point.

This is a wrapper around `prot/diff-refine-buffer' and
`diff-refine-hunk', meant to economise on key bindings."
    (interactive "P")
    (if arg
        (prot/diff-refine-buffer)
      (diff-refine-hunk)))

  (defun prot/diff-restrict-view-dwim (&optional arg)
    "Use `diff-restrict-view', or widen when already narrowed.
By default the narrowing effect applies to the focused diff hunk.
With \\[universal-argument] do it for the current file instead."
    (interactive "P")
    (when (derived-mode-p 'diff-mode)
      (if (buffer-narrowed-p)
          (progn
            (widen)
            (message "Widened the view"))
        (if arg
            (progn
              (diff-restrict-view arg)
              (message "Narrowed to file"))
          (diff-restrict-view)
          (message "Narrowed to diff hunk")))))


  (define-key diff-mode-map (kbd "C-c C-b") #'prot/diff-refine-hunk-or-buf) ; replace `diff-refine-hunk'
  (define-key diff-mode-map (kbd "C-c C-n") #'prot/diff-restrict-view-dwim))

;;** packages: esup
(straight-use-package
 '(esup :type git :flavor melpa :host github :repo "jschaf/esup"))

;;** packages: eval-in-repl

(straight-use-package
 '(eval-in-repl :type git :flavor melpa :host github :repo "kaz-yos/eval-in-repl"))
(require 'racket-mode)
(require 'eval-in-repl-racket)

;;** packages: expand-region
(straight-use-package
 '(expand-region :type git :flavor melpa :host github :repo "magnars/expand-region.el"))
(define-key global-map (kbd "C-'") #'er/expand-region)

;;** packages: flycheck
(straight-use-package
 '(flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck"))
(add-hook 'clojure-mode-hook #'flycheck-mode)

;;** packages: iedit
(straight-use-package
 '(iedit :type git :flavor melpa :host github :repo "victorhge/iedit"))

;;** packages: imenu

(setq imenu-use-markers t)
(setq imenu-auto-rescan t)
(setq imenu-auto-rescan-maxout 600000)
(setq imenu-max-item-length 100)
(setq imenu-use-popup-menu nil)
(setq imenu-eager-completion-buffer t)
(setq imenu-space-replacement " ")
(setq imenu-level-separator "/")

;; based on prot/imenu-vertical
;; https://gitlab.com/protesilaos/dotfiles/
;; until /commit/960bcf8a5df304f77517ee92ade1627c0c57336f
(defun prot/imenu-vertical ()
  "Use a vertical Icomplete layout for `imenu'.
Configure `orderless-matching-styles' for this command."
  (interactive)
  (let ((orderless-matching-styles
         '(orderless-literal
           orderless-regexp
           orderless-prefixes)))
    (setq this-command 'imenu)        ; let `embark' know what this is
    (icomplete-vertical-do (:height (/ (frame-height) 4))
      (call-interactively 'imenu))))

;; based on prot/imenu-recenter-pulse
;; https://gitlab.com/protesilaos/dotfiles/
;; until /commit/960bcf8a5df304f77517ee92ade1627c0c57336f
(defun prot/imenu-recenter-pulse ()
  "Recenter `imenu' position at the top with subtle feedback."
  (let ((pulse-delay 0.1))
    ;; (recenter 0)
    (recenter)
    (pulse-momentary-highlight-one-line (point) 'modus-theme-intense-red)))

;; (add-hook 'imenu-after-jump-hook #'prot/imenu-recenter-pulse)
(add-hook 'imenu-after-jump-hook (lambda ()
                                   (when (and (eq major-mode 'org-mode)
                                              (org-at-heading-p))
                                     (org-show-entry)
                                     (org-reveal t))))
(define-key global-map (kbd "M-g d") #'prot/imenu-vertical)
(define-key global-map (kbd "M-g M-d") #'prot/imenu-vertical)

;;*** packages/imenu: customize

;; do not include sub menus for Variables and Types
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local imenu-generic-expression
                                 '((nil "^\\s-*(\\(cl-def\\(?:generic\\|ine-compiler-macro\\|m\\(?:acro\\|ethod\\)\\|subst\\|un\\)\\|def\\(?:advice\\|generic\\|ine-\\(?:advice\\|compil\\(?:ation-mode\\|er-macro\\)\\|derived-mode\\|g\\(?:\\(?:eneric\\|lobal\\(?:\\(?:ized\\)?-minor\\)\\)-mode\\)\\|inline\\|m\\(?:ethod-combination\\|inor-mode\\|odify-macro\\)\\|s\\(?:etf-expander\\|keleton\\)\\)\\|m\\(?:acro\\|ethod\\)\\|s\\(?:etf\\|ubst\\)\\|un\\*?\\)\\|ert-deftest\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                                  ;; vars
                                   (nil "^\\s-*(\\(def\\(?:c\\(?:onst\\(?:ant\\)?\\|ustom\\)\\|ine-symbol-macro\\|parameter\\)\\)\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)
                                  ;; vars
                                   (nil "^\\s-*(defvar\\(?:-local\\)?\\s-+\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)[[:space:]
]+[^)]" 1)
                                  ;; types
                                   (nil "^\\s-*(\\(cl-def\\(?:struct\\|type\\)\\|def\\(?:class\\|face\\|group\\|ine-\\(?:condition\\|error\\|widget\\)\\|package\\|struct\\|t\\(?:\\(?:hem\\|yp\\)e\\)\\)\\)\\s-+'?\\(\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)" 2)))))


;;** packages: iy-go-to-char

(straight-use-package
 '(iy-go-to-char :type git :flavor melpa :host github :repo "doitian/iy-go-to-char"))

(define-key global-map (kbd "s-d") #'iy-go-up-to-char)
(define-key global-map (kbd "s-D") #'iy-go-to-char-backward)
(define-key global-map (kbd "C-s-d") #'iy-go-to-or-up-to-continue)
(define-key global-map (kbd "C-S-s-d") #'iy-go-to-or-up-to-continue-backward)


;;** packages: haskell-mode

(straight-use-package
 '(haskell-mode :type git :flavor melpa
                :files (:defaults "NEWS" "logo.svg" "haskell-mode-pkg.el")
                :host github :repo "haskell/haskell-mode"))
(require 'haskell)
(setq haskell-interactive-mode-eval-mode 'haskell-mode)

;; add capability to submit code to interpreter and mark errors
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; add missing keybindings for navigating errors
(define-key interactive-haskell-mode-map (kbd "M-n") 'haskell-goto-next-error)
(define-key interactive-haskell-mode-map (kbd "M-p") 'haskell-goto-prev-error)
(define-key interactive-haskell-mode-map (kbd "C-c M-p")
 'haskell-goto-first-error)

;; merge this with your existing custom-set-variables
(custom-set-variables

 ;; NOTE: include following line to work around haskell-mode
 ;; bug if using GHC >= 8.2.1.
 ;; See: https://github.com/haskell/haskell-mode/issues/1553
 '(haskell-process-args-stack-ghci
   '("--ghci-options=-ferror-spans -fshow-loaded-modules"
     "--no-build" "--no-load"))

 ;; some options suggested in the haskell-mode documentation
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t))
;;** packages: keycast

(straight-use-package
 '(moody :type git :flavor melpa :host github :repo "tarsius/moody"))
(require 'moody)

(straight-use-package
 '(keycast :type git :flavor melpa :host github :repo "tarsius/keycast"))
(setq keycast-insert-after " -- ")
(setq keycast-remove-tail-elements nil)
(setq keycast-separator-width 2)
(setq keycast-window-predicate 'moody-window-active-p)
(keycast-mode)



;;** packages: markdown-mode

(straight-use-package
 '(markdown-mode :type git :flavor melpa :host github :repo "jrblevin/markdown-mode"))

;;** packages: ocaml


;; (add-to-list 'load-path "/home/sam/.opam/4.10.0/share/emacs/site-lisp")
;; (require 'ocp-indent)

;; ;; https://github.com/ocaml/tuareg
;; (load "/home/sam/.opam/4.10.0/share/emacs/site-lisp/tuareg-site-file")

;; (with-eval-after-load 'tuareg
;;   (define-key tuareg-mode-map (kbd "C-M-x") #'tuareg-eval-phrase)
;;   (setq tuareg-indent-align-with-first-arg t))

;; ;; https://github.com/ocaml/merlin
;; ;; https://github.com/OCamlPro/opam-user-setup
;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;       (when (and opam-share (file-directory-p opam-share))
;;        ;; Register Merlin
;;        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;        (autoload 'merlin-mode "merlin" nil t nil)
;;        ;; Automatically start it in OCaml buffers
;;        (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;        (add-hook 'caml-mode-hook 'merlin-mode t)
;;        ;; Use opam switch to lookup ocamlmerlin binary
;;        (setq merlin-command 'opam)))
;; ;; (require 'merlin-company)

;; https://github.com/ocaml-ppx/ocamlformat#emacs-setup

;;** packages: paredit

(defvar electrify-return-match
    "[\]}\)\"]"
    "If this regexp matches the text after the cursor, do an \"electric\"
  return.")

(defun electrify-return-if-match (arg)
    "If the text after the cursor matches `electrify-return-match' then
  open and indent an empty line between the cursor and the text.  Move the
  cursor to the new line."
    (interactive "P")
    (let ((case-fold-search nil))
      (if (looking-at electrify-return-match)
      (save-excursion (newline-and-indent)))
      (newline arg)
      (indent-according-to-mode)))

(define-key paredit-mode-map (kbd "M-s") nil)
(define-key paredit-mode-map (kbd "<f13>") #'paredit-splice-sexp)
;; slurp
(define-key paredit-mode-map (kbd "C-<right>") nil)
(define-key paredit-mode-map (kbd "C-M-<right>") nil)
;; barf
(define-key paredit-mode-map (kbd "C-<left>") nil)
(define-key paredit-mode-map (kbd "C-M-<left>") nil)

;; paredit-splice-sexp-killing-forward
(define-key paredit-mode-map (kbd "M-<down>") nil)
(define-key paredit-mode-map (kbd "C->") #'paredit-splice-sexp-killing-forward)

;; paredit-splice-sexp-killing-backward
(define-key paredit-mode-map (kbd "M-<up>") nil)
(define-key paredit-mode-map (kbd "C-<") #'paredit-splice-sexp-killing-backward)

(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
(add-hook 'clojure-mode-hook #'enable-paredit-mode)
(add-hook 'cider-repl-mode-hook #'enable-paredit-mode)
(add-hook 'minibuffer-setup-hook (lambda () (when (eq this-command 'eval-expression) (enable-paredit-mode))))
(add-hook 'racket-mode-hook #'enable-paredit-mode)
(add-hook 'racket-repl-mode-hook #'enable-paredit-mode)

;; (define-key lisp-interaction-mode-map (kbd "RET" ) #'electrify-return-if-match)

;;** packages: recentf

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-saved-items 4000)
(run-at-time nil (* 1 60) 'recentf-save-list)
(add-to-list 'recentf-exclude (format "%s.+" (expand-file-name org-roam-dailies-directory org-roam-directory)))
(add-to-list 'recentf-exclude (format "%s.+" (expand-file-name org-roam-directory)))

;; do not show message in minibuffer
;; credit to
;; https://lists.gnu.org/archive/html/bug-gnu-emacs/2016-08/msg00367.html
(defun recentf-save-silently-advice (original &rest args)
  (let ((inhibit-message t)
        (message-log-max nil))
    (apply original args)))

(advice-add 'recentf-save-list :around #'recentf-save-silently-advice)

;;*** packages/recentf: functions

;;**** packages/recentf/functions: add dired to recentf

;; credit to https://www.emacswiki.org/emacs/RecentFiles
;; directories visited through dired buffers will also be put to recentf. – vibrys.
(defun recentd-track-opened-file ()
  "Insert the name of the directory just opened into the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-add-file default-directory))
  ;; Must return nil because it is run from `write-file-functions'.
  nil)

(defun recentd-track-closed-file ()
  "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
  (and (derived-mode-p 'dired-mode) default-directory
       (recentf-remove-if-non-kept default-directory)))

(add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
(add-hook 'kill-buffer-hook 'recentd-track-closed-file)

;;**** packages/recentf/functions: update recentf on dired rename

;; Magic advice to rename entries in recentf when moving files in
;; dired.
(defun rjs/recentf-rename-notify (oldname newname &rest args)
  (if (file-directory-p newname)
      (rjs/recentf-rename-directory oldname newname)
    (rjs/recentf-rename-file oldname newname)))

(defun rjs/recentf-rename-file (oldname newname)
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-equal name oldname)
                      newname
                    name))
                recentf-list)))

(defun rjs/recentf-rename-directory (oldname newname)
  ;; oldname, newname and all entries of recentf-list should already
  ;; be absolute and normalised so I think this can just test whether
  ;; oldname is a prefix of the element.
  (setq recentf-list
        (mapcar (lambda (name)
                  (if (string-prefix-p oldname name)
                      (concat newname (substring name (length oldname)))
                    name))
                recentf-list)))

(advice-add 'dired-rename-file :after #'rjs/recentf-rename-notify)

;;**** packages/recentf/functions: use completion for recentf

;; based on https://stackoverflow.com/a/6995886/9913235
(defun ram-choose-from-recentf (arg)
  "Select a recently opened file from the `recentf-list'."
  (interactive "P")
  (require 'orderless)
  (let ((f (completing-read "Recent files: "
                            recentf-list nil t)))
    (if arg
        (progn
          (find-file-other-window f))
      (find-file f))))

(define-key global-map (kbd "s-f") #'ram-choose-from-recentf)

;;** packages: rg (ripgrep)

(straight-use-package
 '(rg :type git :flavor melpa :host github :repo "dajva/rg.el"))

(global-set-key (kbd "C-c s") #'rg-menu)

(with-eval-after-load 'rg
  (setq rg-group-result t)
  (setq rg-hide-command nil)
  (setq rg-show-columns nil)
  (setq rg-custom-type-aliases nil)
  (setq rg-default-alias-fallback "all")
  (setq rg-show-header t)
  ;; credit to https://www.youtube.com/watch?v=4qLD4oHOrlc&t=1s

  (rg-define-search prot/grep-vc-or-dir
    :query ask
    :format regexp
    :files "everything"
    :dir (let ((vc (vc-root-dir)))
           (if vc
               vc                   ; search root project dir
             default-directory))   ; or from the current dir
    :confirm prefix
    :flags ("--hidden --glob=!.git --glob=!savehist --no-ignore-vcs"))

  (rg-define-search rg-word
   :format literal
   :flags ("--word-regexp")
   :menu ("Custom" "w" "Word"))

  (defun prot/rg-save-search-as-name ()
    "Save `rg' search results."
    (interactive)
    (let ((pattern (car rg-pattern-history)))
      (rg-save-search-as-name (concat "<<" pattern ">>"))))

  (define-key rg-mode-map (kbd "s") #'prot/rg-save-search-as-name)
  (define-key rg-mode-map (kbd "C-n") #'next-line)
  (define-key rg-mode-map (kbd "C-p") #'previous-line)
  (define-key rg-mode-map (kbd "M-n") #'rg-next-file)
  (define-key rg-mode-map (kbd "M-p") #'rg-prev-file)

  (define-key global-map (kbd "M-s g") #'prot/grep-vc-or-dir))


;;** packages: sudo-edit
(straight-use-package
 '(sudo-edit :type git :flavor melpa :host github :repo "nflath/sudo-edit"))

;;** packages: which-key
(straight-use-package
 '(which-key :type git :flavor melpa :host github :repo "justbur/emacs-which-key"))
(run-with-idle-timer 1 nil (lambda () (which-key-mode t)))

;;* spelling

;;** spelling: ispell, aspell, hunspell

(with-eval-after-load "ispell"
  ;; (setq ispell-program-name "/usr/bin/ispell")
  ;; (setq ispell-program-name "/usr/bin/aspell")
  (setenv "DICPATH" (expand-file-name "~/backup/emacs/dictionary"))
  ;; (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "/usr/bin/hunspell")
  ;; ispell-set-spellchecker-params must be called before
  ;; ispell-hunspell-add-multi-dic
  ;; (setq ispell-hunspell-dict-paths-alist
  ;;       `(("pali" ,(expand-file-name "~/backup/emacs/dictionary/pali.aff"))
  ;;         ("en_US" "/usr/share/hunspell/en_US.aff")
  ;;         ))
  ;; (ispell-set-spellchecker-params)
  ;; (ispell-hunspell-add-multi-dic "en_US,pali")
  ;; (setq ispell-dictionary-alist
  ;;       '(("pali")
  ;;         ("en_US" #1="[[:alpha:]]" #2="[^[:alpha:]]" #3="[0-9]" t #4=("-d" "en_US")
  ;;         nil utf-8)))
  ;; (setq ispell-dictionary-alist
  ;;   '((nil "[A-Za-z]" "[^A-Za-z]" "[']" t
  ;;      ("-d" "en_US" "-i" "utf-8") nil utf-8)
  ;;     ("american"
  ;;      "[A-Za-z]" "[^A-Za-z]" "[']" nil
  ;;      ("-d" "en_US") nil utf-8)
  ;;     ("english"
  ;;      "[A-Za-z]" "[^A-Za-z]" "[']" nil
  ;;      ("-d" "en_GB") nil utf-8)
  ;;     ("british"
  ;;      "[A-Za-z]" "[^A-Za-z]" "[']" nil
  ;;      ("-d" "en_GB") nil utf-8)
  ;;     ("norsk"
  ;;      "[A-Za-zÉÆØÅéæøå]" "[^A-Za-zÉÆØÅéæøå]" "[\"]" nil
  ;;      ("-d" "nb_NO") "~list" utf-8)))
  ;; (setq ispell-personal-dictionary "~/backup/emacs/.ispell_default")
  ;; (setq ispell-personal-dictionary "~/backup/emacs/.aspell.en.pws")
  (setq ispell-personal-dictionary "~/backup/emacs/dictionary/personal_dict")
  (unless (file-exists-p ispell-personal-dictionary)
    (write-region "" nil ispell-personal-dictionary nil 0))
  ;; (setq ispell-dictionary "english")
  (setq ispell-silently-savep t)
  ;; pali part has a million entries, seem like it slows
  ;; the some commands: like delete char, backward-char etc
  ;; (setq ispell-dictionary "en_US_pali")
  (setq ispell-dictionary "en_US")
  ;; (setq ispell-extra-args '("-a" "-i" "utf-8" "-d" "/usr/share/hunspell/en_US,/home/sam/backup/emacs/dictionary/pali"))
  )
;;** spelling: flycheck

;;*** spelling/flycheck: settings

;; credit to https://joelkuiper.eu/spellcheck_emacs
(dolist (hook '(text-mode-hook
                org-mode))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(emacs-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook))
  (add-hook hook
            #'(lambda ()
                (flyspell-prog-mode))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;*** spelling/flycheck: binding

(with-eval-after-load "flyspell"
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key global-map (kbd "s-M-c") 'ispell-word)

  (define-key global-map (kbd "s-M-C") 'flyspell-check-next-highlighted-word))

  ;; hyper key is disables in favor of f1, ... keys
  ;; (define-key global-map (kbd "C-H-e") 'flycheck-next-error)
  ;; (define-key global-map (kbd "C-H-E") 'flycheck-previous-error)


;;*** spelling/flycheck: functions

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to arg previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))
        (forward-word)))))

(define-key global-map (kbd "<M-f11>") 'flyspell-goto-previous-error)

;;* system

;;** system: associate major-mode with file names

(add-to-list 'auto-mode-alist '("zshrc\\'" . shell-script-mode))

;;** system: commands

;;*** system/commands: repeated, consecutive

(defvar ram-num-of-repeated-command-calls 0
  "Hold the number of repeated command invocations.
Reset to zero if the current command is different from the
previous.")

(defun ram-modify-num-of-repeated-calls-var ()
  (if (eq last-command real-this-command)
      (setq ram-num-of-repeated-command-calls (1+ ram-num-of-repeated-command-calls))
    (setq ram-num-of-repeated-command-calls 0)))

(add-hook 'pre-command-hook #'ram-modify-num-of-repeated-calls-var)

;;** system: comments

;;** system/comments:

;; credit to Artur Malabarba
;; https://endlessparentheses.com/a-comment-or-uncomment-sexp-command.html
(defun uncomment-sexp (&optional n)
  "Uncomment a sexp around point."
  (interactive "P")
  (let* ((initial-point (point-marker))
         (inhibit-field-text-motion t)
         (p)
         (end (save-excursion
                (when (elt (syntax-ppss) 4)
                  (re-search-backward comment-start-skip
                                      (line-beginning-position)
                                      t))
                (setq p (point-marker))
                (comment-forward (point-max))
                (point-marker)))
         (beg (save-excursion
                (forward-line 0)
                (while (and (not (bobp))
                            (= end (save-excursion
                                     (comment-forward (point-max))
                                     (point))))
                  (forward-line -1))
                (goto-char (line-end-position))
                (re-search-backward comment-start-skip
                                    (line-beginning-position)
                                    t)
                (ignore-errors
                  (while (looking-at-p comment-start-skip)
                    (forward-char -1)))
                (point-marker))))
    (unless (= beg end)
      (uncomment-region beg end)
      (goto-char p)
      ;; Indentify the "top-level" sexp inside the comment.
      (while (and (ignore-errors (backward-up-list) t)
                  (>= (point) beg))
        (skip-chars-backward (rx (syntax expression-prefix)))
        (setq p (point-marker)))
      ;; Re-comment everything before it.
      (ignore-errors
        (comment-region beg p))
      ;; And everything after it.
      (goto-char p)
      (forward-sexp (or n 1))
      (skip-chars-forward "\r\n[:blank:]")
      (if (< (point) end)
          (ignore-errors
            (comment-region (point) end))
        ;; If this is a closing delimiter, pull it up.
        (goto-char end)
        (skip-chars-forward "\r\n[:blank:]")
        (when (eq 5 (car (syntax-after (point))))
          (delete-indentation))))
    ;; Without a prefix, it's more useful to leave point where
    ;; it was.
    (unless n
      (goto-char initial-point))))

(defun comment-sexp--raw ()
  "Comment the sexp at point or ahead of point."
  (pcase (or (bounds-of-thing-at-point 'sexp)
             (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (bounds-of-thing-at-point 'sexp)))
    (`(,l . ,r)
     (goto-char r)
     (skip-chars-forward "\r\n[:blank:]")
     (save-excursion
       (comment-region l r))
     (skip-chars-forward "\r\n[:blank:]"))))

(defun comment-or-uncomment-sexp (&optional n)
  "Comment the sexp at point and move past it.
If already inside (or before) a comment, uncomment instead.
With a prefix argument N, (un)comment that many sexps."
  (interactive "P")
  (if (or (elt (syntax-ppss) 4)
          (< (save-excursion
               (skip-chars-forward "\r\n[:blank:]")
               (point))
             (save-excursion
               (comment-forward 1)
               (point))))
      (uncomment-sexp n)
    (dotimes (_ (or n 1))
      (comment-sexp--raw))))

(define-key global-map (kbd "C-M-;") #'comment-or-uncomment-sexp)

;;** system/comments: functions

(defun ram-next-comment (num)
  "Jump comment beginnings and ends."
  (interactive "p")
  (comment-normalize-vars t)
  (dotimes (_ num)
    (move-end-of-line 1)
    ;; if not in comment go to next comment
    ;; if in comment go to the last comment in this block
    (if (not (nth 4 (syntax-ppss)))
        (goto-char (or (comment-search-forward (point-max) t) (point)))
      (move-end-of-line 2)
      (if (not (nth 4 (syntax-ppss)))
          (goto-char (or (comment-search-forward (point-max) t) (point)))
        (while (nth 4 (syntax-ppss))
          (move-end-of-line 2))
        (goto-char (or (comment-search-backward (point-min) t) (point)))))))

(defun ram-previous-comment (num)
  "Jump to comment ends and beginnings."
  (interactive "p")
  (comment-normalize-vars t)
  ;; if not in comment go to previous comment
  ;; if in comment go to the first comment in this block
  (dotimes (_ num)
    (move-end-of-line 1)
    (if (not (nth 4 (syntax-ppss)))
       (goto-char (or (comment-search-backward (point-min) t) (point)))
     (move-end-of-line 0)
     (if (not (nth 4 (syntax-ppss)))
         (goto-char (or (comment-search-backward (point-min) t) (point)))
       (while (nth 4 (syntax-ppss))
         (move-end-of-line 0))
       (goto-char (or (comment-search-forward (point-max) t) (point)))))))

(define-key global-map (kbd "s-k") #'ram-next-comment)
(define-key global-map (kbd "s-K") #'ram-previous-comment)

;;** system: calendar

(setq calendar-week-start-day 1)

;;** system: cursor

(set-default 'cursor-type 'box)
(defun ram-change-cursor-color ()
  (let* ((bg (face-background 'default))
         (curs-color (if (< (color-distance "black" bg)
                            (color-distance "white" bg))
                         "yellow"
                       "black")))
    ;; (add-to-list 'default-frame-alist `(cursor-color . ,curs-color))
    (set-cursor-color curs-color)))

(run-with-idle-timer 2 nil 'ram-change-cursor-color)

;; (add-hook 'after-load-theme-hook #'ram-change-cursor-color)

(setq blink-cursor-mode nil)
(setq blink-cursor-blinks 1)

;;** system: faces, fonts

;; credit to:
;; https://coderwall.com/p/ifgyag/change-font-size-in-emacs-dynamically-based-on-screen-resolution
(defun fontify-frame (frame)
  (interactive)
  (if window-system
      (progn
        (if (>= (x-display-pixel-width) large-sreen-width)
            (progn
              (set-frame-parameter frame 'font "Operator Mono Medium-19")
              (custom-set-faces
               '(font-lock-comment-face ((t (:family "Operator Mono Light-19")))))
              (custom-set-faces
               '(font-lock-doc-face ((t (:family "Operator Mono Light-19" :slant italic)))))
              (custom-set-faces
               '(font-lock-string-face ((t (:family "Operator Mono Light-19" :slant italic))))))
          (progn
            (set-frame-parameter frame 'font "Operator Mono Medium-12")
            (custom-set-faces
             '(font-lock-comment-face ((t (:family "Operator Mono Light-12")))))
            (custom-set-faces
             '(font-lock-doc-face ((t (:family "Operator Mono Light-12" :slant italic)))))
            (custom-set-faces
             '(font-lock-string-face ((t (:family "Operator Mono Light-12" :slant italic))))))))))

;; Fontify current frame
(fontify-frame nil)

;; Fontify any future frames
(push 'fontify-frame after-make-frame-functions)

;; "C-u C-x =" shortcut for what-cursor-position command gives you the font info

;; (set-face-attribute 'default nil :font "AnonymousPro-20")
;; (set-face-attribute 'default nil :font "Apercu Mono Pro-19")
;; (set-face-attribute 'default nil :font "Calibri-20")
;; (set-face-attribute 'default nil :font "CascadiaCode-18")
;; (set-face-attribute 'default nil :font "Consolas-19")
;; (set-face-attribute 'default nil :font "DankMono-20")
;; (set-face-attribute 'default nil :font "DroidSansMono-18")
;; (set-face-attribute 'default nil :font "Envy Code R-18")
;; (set-face-attribute 'default nil :font "FiraCode-18")
;; (set-face-attribute 'default nil :font "FiraGo Book-18")
;; (set-face-attribute 'default nil :font "Hack-18:style=Regular")
;; (set-face-attribute 'default nil :font "Inconsolata-20")
;; (set-face-attribute 'default nil :font "Input Mono-18:style=Regular")
;; (set-face-attribute 'default nil :font "InputMono Light-18:style=Regular")
;; (set-face-attribute 'default nil :font "Iosevka-19")
;; (set-face-attribute 'default nil :font "JetBrainsMono-19")
;; (set-face-attribute 'default nil :font "LiberationMono-19")
;; (set-face-attribute 'default nil :font "Menlo-19")
;; (set-face-attribute 'default nil :font "Monaco-18")
;; (set-face-attribute 'default nil :font "Monoid-16")
;; (set-face-attribute 'default nil :font "MonoLisa-Regular-18")
;; (set-face-attribute 'default nil :font "Operator Mono Medium-19")
;; (set-face-attribute 'default nil :font "Operator Mono Book-20")
;; (set-face-attribute 'default nil :font "PragmataPro-19")
;; (set-face-attribute 'default nil :font "SourceCodePro-19")
;; (set-face-attribute 'default nil :font "SFNS Display-20")
;; (set-face-attribute 'default nil :font "Terminus (TTF)-22")
;; (set-face-attribute 'default nil :font "Victor Mono Medium-18")
;; (set-face-attribute 'default nil :font "UbuntuMono-20")

;; (set-face-attribute 'fixed-pitch nil :font "FiraCode-18")
;; (set-face-attribute 'fixed-pitch nil :font "Operator Mono Medium-19")
;; (set-face-attribute 'fixed-pitch nil :font "Operator Mono Medium")
(set-face-attribute 'fixed-pitch nil :family "Operator Mono Medium-19")
;; (set-face-attribute 'fixed-pitch nil :font "-misc-operator mono medium-medium-r-normal--0-90-0-0-m-0-iso10646-1")
;(set-face-attribute 'fixed-pitch nil :font "-adobe-courier-medium-r-normal--25-180-100-100-m-150-iso10646-1")




;; (set-face-attribute 'variable-pitch nil :font "FiraGo-18")

;; (set-face-attribute 'variable-pitch nil :font "Calibri-20")
(set-face-attribute 'variable-pitch nil :family "Verdana-19")

(with-eval-after-load "org"
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
  (with-eval-after-load 'org-indent-mode
    (set-face-attribute 'org-indent nil :inherit 'fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch))

;;** system: general settings

(setq confirm-nonexistent-file-or-buffer t)

(setq vc-follow-symlinks t)

(setq confirm-kill-emacs 'y-or-n-p)

(setq large-file-warning-threshold nil)

(run-with-idle-timer 2 nil (lambda () (fringe-mode '(20 . 20))))
;; use "y", "n" for confirmations requiring "yes", "no"
(defalias 'yes-or-no-p 'y-or-n-p)
(when tool-bar-mode (tool-bar-mode -1))
(when menu-bar-mode (menu-bar-mode -1))
;; (when scroll-bar-mode (scroll-bar-mode -1))
(scroll-bar-mode -1)

;; (global-linum-mode t)
(setq inhibit-startup-message t)

(delete-selection-mode t)

;;*** system/general settings: scroll

(setq find-function-recenter-line nil)
(setq scroll-preserve-screen-position nil)
;; https://stackoverflow.com/questions/18386824/emacs-how-do-you-disable-auto-recentering
;; stop auto scrolling
(setq scroll-step 0)
;; (setq scroll-conservatively 10000)
(setq scroll-conservatively 0)
(setq auto-window-vscroll nil)


;;*** system/general settings: hl (highlight) line

(defun ram-remap-hl-line-face-in-find-file-hook ()
  "Add face remap for `hl-line' face.

This face remap is supposed to identify different buffer types.

Call this function from a \".*-mode-hook\" type
of hooks."
  (when (not (boundp 'ram-face-remapping-cookie))
    (defvar ram-face-remapping-cookie nil
      "Hold the return value of `face-remap-add-relative'."))
  (make-local-variable 'ram-face-remapping-cookie)
  (cond
   ;; org-roam dailies note
   ((string-match "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\.org$" (buffer-name))
    (let ((new-face '(:background "LightSteelBlue1" :underline "LightSteelBlue3" :extend t)))
      (when (not (member new-face (cdr (assoc 'hl-line face-remapping-alist))))
        (face-remap-add-relative 'hl-line new-face))))
   ;; org-roam note
   ((string-match "^20[0-9]\\{12\\}-[^[:space:]]+\\.org$" (buffer-name))
    (let ((new-face '(:background "DarkSeaGreen1" :underline "DarkSeaGreen3" :extend t)))
      (when (not (member new-face (cdr (assoc 'hl-line face-remapping-alist))))
        (face-remap-add-relative 'hl-line new-face))))
   ((derived-mode-p 'prog-mode)
    (let ((new-face '(:background "LemonChiffon1" :underline "LemonChiffon3" :extend t)))
      (when (not (member new-face (cdr (assoc 'hl-line face-remapping-alist))))
        (face-remap-add-relative 'hl-line new-face))))
   ;; (t (message "hl-line face remapping is not defined for %s buffer modes" (buffer-local-value 'major-mode (current-buffer))))
   (t (message "hl-line face remapping is not defined for \"%s\" buffer modes" major-mode))))

(defun ram-add-remap-face-to-hl-line-in-capture-hook ()
  "Add face remap for `hl-line' face.

This face remap is supposed to identify buffers where
`org-capture-mode' is a minor mode.

The function sets buffer-local `ram-face-remapping-cookie'
variable to the `face-remap-add-relative' return value. Use this
variable to remove the face."
  (when (not (boundp 'ram-face-remapping-cookie))
    (defvar ram-face-remapping-cookie nil
      "Hold the return value of `face-remap-add-relative'."))
  (make-local-variable 'ram-face-remapping-cookie)
  (setq ram-face-remapping-cookie
        (face-remap-add-relative 'hl-line
                                 '(:background "thistle1"
                                               :underline "thistle3"
                                               :extend t))))

(defun ram-remove-face-remapping ()
  "Removes remap face identified by `ram-face-remapping-cookie'.

Call `face-remap-remove-relative' with a value stored in
buffer-local `ram-face-remapping-cookie'."
  (when (not (boundp 'ram-face-remapping-cookie))
    (defvar ram-face-remapping-cookie nil
      "Hold the return value of `face-remap-add-relative'."))
  (make-local-variable 'ram-face-remapping-cookie)
  (when ram-face-remapping-cookie
    (face-remap-remove-relative ram-face-remapping-cookie)
    (setq ram-face-remapping-cookie nil)))

;; (setq nlinum-highlight-current-line t)
;; (setq-default display-line-numbers t)

;; (setq hl-line-range-function #'(lambda () (cons (line-beginning-position) (line-beginning-position 2))))
;; (set-face-attribute 'hl-line nil :inherit nil :background "LightCyan1" :underline "LightCyan3" :extend t)
;; !!! exwm would no load if this option is
;; apparently a backtrace is displayed which is off screen,
;; pressing 'q' or 'C-x k' would continue exwm loading. But it will load with limited functionality.
;; (add-hook 'after-load-theme-hook
;;           (set-face-attribute 'hl-line nil :inherit nil :background "LemonChiffon1" :underline "LemonChiffon3" :extend t))
(when window-system (global-hl-line-mode 1))
;; (global-hl-line-mode)

;;*** system/general settings: backup files

;; do not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)

;;*** system/general settings: show-paren

;; highlights matching parens
;; disable
(show-paren-mode -1)

;; (setq show-paren-delay 0)
;; (setq show-paren-style 'expression)
;; (setq show-paren-style 'parenthesis)
;; (set-face-background 'show-paren-match "#42444a")

;;*** system/general settings: time

(setq time-stamp-format "[%Y-%02m-%02d %3a %02H:%02M]")

;;*** system/general settings: misc

;; Default Browser
(setq browse-url-browser-function 'browse-url-generic
    browse-url-generic-program "qutebrowser")

;; kill line and newline char
(setq kill-whole-line t)

;;*** system/general settings: ring-bell

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;*** system/general settings: language

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;*** system/general settings: *scratch*

(defun ram-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(define-key lisp-interaction-mode-map (kbd "<S-return>") 'newline-and-indent)

;; (define-key ram-leader-map-tap-global (kbd "'") 'ram-switch-to-scratch)

;;*** system/general settings: eval

(setq eval-expression-print-length 100)

;;*** system/general settings: info/manual

(define-key Info-mode-map (kbd "s-l") #'ace-link)


;;*** system/general settings: errors, warnings

(setq warning-minimum-level :error)
;; (setq native-comp-async-report-warnings-errors 'silent)


;;** system: savehist

(setq savehist-file "~/.emacs.d/savehist")
(setq history-length 200)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
             '(kill-ring
               search-ring
               regexp-search-ring))
;; (run-with-idle-timer 1 nil #'savehist-mode)
(savehist-mode t)

;;** system: hooks

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;;** system: indent

(setq-default indent-tabs-mode nil)
(add-hook 'clojure-mode-hook
          (lambda () (setq-local evil-shift-width 2)))
(add-hook 'emacs-lisp-mode-hook
          (lambda () (setq-local evil-shift-width 2)))
(add-hook 'lisp-interaction-mode-hook
          (lambda () (setq-local evil-shift-width 2)))

;;** system: region

(with-eval-after-load "simple"
  ;; this differs from the default version only by
  ;; (overlay-put 'priority ...) line
  (setq redisplay-highlight-region-function
        (lambda (start end window rol)
          (if (not (overlayp rol))
              (let ((nrol (make-overlay start end)))
                (funcall redisplay-unhighlight-region-function rol)
                (overlay-put nrol 'window window)
                (overlay-put nrol 'face 'region)
                ;; Normal priority so that a large region doesn't hide all the
                ;; overlays within it, but high secondary priority so that if it
                ;; ends/starts in the middle of a small overlay, that small overlay
                ;; won't hide the region's boundaries.
                (overlay-put nrol 'priority '(50 . 100))
                nrol)
            (unless (and (eq (overlay-buffer rol) (current-buffer))
                         (eq (overlay-start rol) start)
                         (eq (overlay-end rol) end))
              (move-overlay rol start end (current-buffer)))
            rol))))

(add-hook 'ram-load-theme-hook (lambda ()
                                 (set-face-attribute 'region nil :extend nil)))

;;** system: position registers

(defun ram-point-to-register-x ()
  (interactive)
  (point-to-register ?x))

(defun ram-jump-to-register-x ()
  (interactive)
  (jump-to-register ?x))

(define-key global-map (kbd "S-<f14>") #'ram-point-to-register-x)

(define-key global-map (kbd "<f14>") #'ram-jump-to-register-x)

;;** system: syntax tables

;; treat "_", "-" as part of the word
;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word/9584
;; (defadvice evil-inner-word (around underscore-as-word activate)
;;   (let ((table (copy-syntax-table (syntax-table))))
;;     (modify-syntax-entry ?_ "w" table)
;;     (modify-syntax-entry ?- "w" table)
;;     (with-syntax-table table
;;       ad-do-it)))
(modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
;; (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)


;; does not seem to call modify-syntax-entry for clojure???
;; doing so by hand, works
;; modify-syntax-entry for clojure ?# char causes an error for lispy-tab, lispy-multiline commands
;; (add-hook 'clojure-mode-hook (lambda () (
;;                                          (progn
;;                                            (modify-syntax-entry ?# "w" clojure-mode-syntax-table)))))
(add-hook 'python-mode-hook (lambda () (progn
                                         (modify-syntax-entry ?_ "w" python-mode-syntax-table))))
                                         ;; (modify-syntax-entry ?- "w" python-mode-syntax-table)


;;** system: tramp

;;** system: whitespace

(setq-default show-trailing-whitespace t)
(add-hook 'cider-test-report-mode-hook #'(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'buffer-menu-mode-hook #'(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'minibuffer-setup-hook #'(lambda () (setq show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook #'(lambda () (setq show-trailing-whitespace t)))

;; (add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (remove-hook 'before-save-hook 'delete-trailing-whitespace)

;;* Navigate marks

;; go to previous location in buffer and return back
;; credit to  https://github.com/freetonik/emacs-dotfiles
(defun my-pop-local-mark-ring ()
  (interactive)
  (set-mark-command t))

(defun unpop-to-mark-command ()
  "Unpop off mark ring. Does nothing if mark ring is empty."
  (interactive)
  (when mark-ring
    (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
    (set-marker (mark-marker) (car (last mark-ring)) (current-buffer))
    (when (null (mark t)) (ding))
    (setq mark-ring (nbutlast mark-ring))
    (goto-char (marker-position (car (last mark-ring))))))

;; f11 is bound toggle-frame-full-screen by default
(global-unset-key (kbd "<f11>"))

(define-key ram-leader-map-tap-global (kbd "-") 'my-pop-local-mark-ring)
(define-key global-map (kbd "<f11>") 'my-pop-local-mark-ring)
(define-key global-map (kbd "S-<f11>") #'pop-global-mark)
(define-key ram-leader-map-tap-global (kbd ".") 'unpop-to-mark-command)
(define-key global-map (kbd "<f21>") 'unpop-to-mark-command)


;;* Open a new line

;; credit to https://emacsredux.com/blog/2013/03/26/smarter-open-line/
(defun smart-open-line ()
  "Insert an empty line after the current line. Position the cursor at its beginning, according to the current mode."
  (interactive)
  (move-end-of-line nil)
  (newline-and-indent))

(defun smart-open-line-above ()
  "Insert an empty line above the current line. Position the cursor at it's beginning, according to the current mode."
  (interactive)
  (move-beginning-of-line nil)
  (newline-and-indent)
  (forward-line -1)
  (indent-according-to-mode))

(define-key global-map (kbd "<s-return>") 'smart-open-line)
(define-key global-map (kbd "<S-s-return>") 'smart-open-line-above)
(with-eval-after-load 'clojure-mode
  (define-key clojure-mode-map (kbd "<S-return>") 'newline-and-indent))

;;* xah-fly-keys

;; load lisp/xah-fly-keys.el
;; (setq xah-fly-use-control-key nil)
;; (require 'xah-fly-keys)

;; (xah-fly-keys-set-layout "mybeakl10")

;; (xah-fly-keys 1)

;;* scimax hydra

;; load lisp/scimax-hydra.el
;; (my-with-elapsed-timer "Loading lisp/scimax-hydra.el"
;;   (when (file-readable-p "~/.emacs.d/lisp/scimax-hydra.el")
;;     (load-file (expand-file-name "~/.emacs.d/lisp/scimax-hydra.el"))))

;; (my-with-elapsed-timer "Loading ../lisp/emacs-keybinding-command-tooltip-mode.el"
;;   (when (file-readable-p "~/.emacs.d/lisp/emacs-keybinding-command-tooltip-mode.el")
;;     (load-file (expand-file-name "~/.emacs.d/lisp/emacs-keybinding-command-tooltip-mode.el"))))

;;* sayid
(straight-use-package
 '(sayid :type git :flavor melpa :files ("src/el/*.el" "sayid-pkg.el") :host github :repo "clojure-emacs/sayid"))

(eval-after-load 'clojure-mode
  '(sayid-setup-package))

;;* dired

;; (require 'dired)

(autoload 'dired-mode "dired")

;;** dired settings

;; use the other dired window at the paste/move target
(setq dired-dwim-target t)

;; do not ask y/n
;; “always” means no asking
(setq dired-recursive-copies 'always)
;; “top” means ask once
(setq dired-recursive-deletes 'top)

(setq dired-isearch-filenames 'dwim)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;** dired bindings

;; default behavior would create a new buffer for each visited dir, change it with the following:
;; was dired-advertised-find-file
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; "^" default binding is to #'dired-up-directory
;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

(global-set-key (kbd "C-c j") 'dired-jump)

;;* hippie-expand

;;** hippie-expand:  diacritic, accented

(setq ram-diacritical-marked
      '(("AA" . "Ā")
        ("aa" . "ā")
        ("II" . "Ī")
        ("ii" . "ī")
        ("UU" . "Ū")
        ("uu" . "ū")
        ("\"N" . "Ṅ")
        ("\"n" . "ṅ")
        (".M" . "Ṃ")
        (".m" . "ṃ")
        ("~N" . "Ñ")
        ("~n" . "ñ")
        (".T" . "Ṭ")
        (".t" . "ṭ")
        (".D" . "Ḍ")
        (".d" . "ḍ")
        (".N" . "Ṇ")
        (".n" . "ṇ")
        (".L" . "Ḷ")
        (".l" . "ḷ")))

(defun ram-diacritical-marked-beg ()
  (save-excursion
    ;; (re-search-forward ".a" (- (point) 2) t -1)
    (- (point) 2)))

(defun ram-try-expand-diacritical-marked (old)
  (unless old
    (he-init-string (ram-diacritical-marked-beg) (point))
    (setq he-expand-list (sort
                          (let ((completion-ignore-case nil))
                            (all-completions he-search-string ram-diacritical-marked))
                          'string-lessp)))
  (while (and he-expand-list
              (he-string-member (car he-expand-list) he-tried-table))
    (setq he-expand-list (cdr he-expand-list)))
  (if (null he-expand-list)
      (progn
        (when old (he-reset-string))
        ())
    (he-substitute-string (cdr (assoc (car he-expand-list) ram-diacritical-marked)))
    (setq he-tried-table (cons (car he-expand-list) (cdr he-tried-table)))
    (setq he-expand-list (cdr he-expand-list))
    t))

;;** hippie-expand: make-hippie-expand-function

(define-key global-map (kbd "M-/") (make-hippie-expand-function
                                    '(
                                      ;; try-expand-all-abbrevs
                                      ram-try-expand-diacritical-marked
                                      try-expand-dabbrev-visible
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev
                                      try-complete-file-name-partially
                                      try-complete-file-name
                                      try-complete-lisp-symbol-partially
                                      try-complete-lisp-symbol
                                      try-expand-line) t))

(eval-after-load "org"
  '(define-key org-mode-map (kbd "M-/") (make-hippie-expand-function
                                         '(
                                           ram-try-expand-diacritical-marked
                                           try-expand-all-abbrevs
                                           try-expand-dabbrev-visible
                                           try-expand-dabbrev-all-buffers
                                           try-expand-dabbrev
                                           try-complete-file-name-partially
                                           try-complete-file-name
                                           try-expand-line) t)))

;; (
;;  try-expand-list
;;  ;;  try-expand-dabbrev-from-kill
;;* custom

;;** custom: copy

;;*** custom/copy: line

;; credit to https://www.emacswiki.org/emacs/CopyingWholeLines
;; see the link for more #'copy-line commands

(defun copy-line (arg)
    "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
    (interactive "p")
    (let ((beg (line-beginning-position))
          (end (line-end-position arg)))
      (when mark-active
        (if (> (point) (mark))
            (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
          (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
      (if (eq last-command 'copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(define-key global-map (kbd "<M-f16>") #'copy-line)

;;** custom: ram-up-list-insert-space

(defun ram-forward-over-whitespace-to-regexp (regexp)
  "Return  `match-beginning' of regexp if preceded by whitespace."
  (if-let* ((bound (save-excursion (re-search-forward "[^[:space:]\n]" nil t 1)))
            (new-point (save-excursion
                         (re-search-forward (concat "\\([[:space:]]*\\|\n*\\)" "\\(" regexp "\\)")
                                            bound
                                            t 1))))
    (goto-char (match-beginning 2))))

(defun ram-backward-over-whitespace-to-regexp (regexp)
  "Return  `match-end' of regexp if followed by whitespace."
  (if-let* ((bound (save-excursion (re-search-backward "[^[:space:]\n]" nil t 1)))
            (new-point (save-excursion
                         (re-search-backward (concat  "\\(" regexp "\\)" "\\([[:space:]]*\\|\n*\\)")
                                            bound
                                            t 1))))
      (goto-char (match-end 1))))

(defun ram-remove-whitespace-between-regexps (regexp1 regexp2)
  "Remove all whitespace between REGEXP1 and REGEXP2."
  (let ((beg (ram-backward-over-whitespace-to-regexp regexp1) )
        (end (ram-forward-over-whitespace-to-regexp regexp2)))
    (when (and beg end)
      (goto-char beg)
     (delete-char (- end beg)))))

(defun ram-up-list-insert-space ()
  "When inside a list or string, jump out of it and insert space."
  (interactive)
  (let* ((ppss (syntax-ppss))
         (inside-str (nth 3 ppss))
         (inside-parens (nth 1 ppss)))
    (cond
     (inside-str
      (backward-up-list -1 t t))
     ;; same as 'inside-str only removes white space between parens
     (inside-parens
      (ram-remove-whitespace-between-regexps (concat "\"" "\\|" ram-close-delimiters-re) ram-close-delimiters-re)
      (backward-up-list -1 t t)))
    (cond
     ((and (char-after)
           (= (char-after) 10))
      (newline-and-indent)
      (indent-according-to-mode))
     ((and (char-after)
           (= (char-after) 32))
      (forward-char))
     (t (insert " ")))))

(define-key global-map (kbd "S-SPC") #'ram-up-list-insert-space)

;;** custom: narrow

(defun ram-toggle-narrow-to-defun ()
  "Toggle `narrow-to-defun'."
  (interactive)
  (if (not (buffer-narrowed-p))
      (narrow-to-defun)
    (widen)
    (recenter)))

;;** custom: ram-cursor-mode
(autoload 'global-ram-cursor-mode "ram-cursor")
(global-ram-cursor-mode 1)

;; credit to https://emacs.stackexchange.com/a/37648
;; (defun yf/replace-or-delete-pair (open)
;;   "Replace pair at point by OPEN and its corresponding closing character.
;; The closing character is lookup in the syntax table or asked to
;; the user if not found."
;;   (interactive
;;    (list
;;     (read-char
;;      (format "Replacing pair %c%c by (or hit RET to delete pair):"
;;              (char-after)
;;              (save-excursion
;;                (forward-sexp 1)
;;                (char-before))))))
;;   (if (memq open '(?\n ?\r))
;;       (delete-pair)
;;     (let ((close (cdr (aref (syntax-table) open))))
;;       (when (not close)
;;         (setq close
;;               (read-char
;;                (format "Don't know how to close character %s (#%d) ; please provide a closing character: "
;;                        (single-key-description open 'no-angles)
;;                        open))))
;;       (yf/replace-pair open close))))

;; (defun yf/replace-pair (open close)
;;   "Replace pair at point by respective chars OPEN and CLOSE.
;; If CLOSE is nil, lookup the syntax table. If that fails, signal
;; an error."
;;   (let ((close (or close
;;                    (cdr-safe (aref (syntax-table) open))
;;                    (error "No matching closing char for character %s (#%d)"
;;                           (single-key-description open t)
;;                           open)))
;;         (parens-require-spaces))
;;     (insert-pair 1 open close))
;;   (delete-pair)
;;   (backward-char 1))

;; (define-key ram-leader-map-tap-global (kbd "/") 'yf/replace-or-delete-pair)
