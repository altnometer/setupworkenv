;; packages.el --- Package configuration -*- lexical-binding: t -*-

(set-language-environment "UTF-8")
;; (set-language-environment "Latin-1")

;; these are screen widths for FHD, QHD(2K), UHD(4K) screens
;; use them to adjust font size.
(defconst fhd-width 1920
  "`fhd-width' integer used to compare current monitor size.")
(defconst qhd-width 2560
  "`qhd-width' integer used to compare current monitor size.")
(defconst uhd-width 3840
  "`uhd-width' integer used to compare current monitor size.")

;;* color-themes

;; TODO:after-load-theme-hook change to ram-after-load-theme-hook
(defvar after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

;;** color-themes: modus-themes

;; consult https://protesilaos.com/emacs/modus-themes for customization
;; starting Emacs 28.1 the Modus themes are built in.

;; (setq modus-operandi-theme-diffs nil)
(setq modus-operandi-theme-diffs 'desaturated)
(setq modus-operandi-theme-diffs 'fg-only)
;; (setq modus-themes-hl-line '(accented intense underline))
;; enable inheritance from ‘fixed-pitch’ in some faces
;; (setq modus-themes-mixed-fonts t)
;; use italic font forms in more code constructs
;; (setq modus-themes-italic-constructs t)
;; (setq modus-themes-org-blocks 'tinted-background)
(setq modus-themes-org-blocks 'gray-background)
(setq modus-themes-markup '(background))
(setq modus-themes-headings
      '((1 . (background rainbow overline (height . 1.1)))
        (2 . (background rainbow overline (height . 1.05)))
        (3 . (background rainbow overline (height . 1.05)))
        (t . (rainbow (height . 1.0) ))))
(setq modus-themes-completions
      '((matches . (extrabold background intense))
        (selection . (semibold accented intense))
        (popup . (accented))))
(setq modus-themes-links '(neutral-underline faint))
(setq modus-themes-region '(accented no-extend))
(setq modus-themes-syntaxt '(alt-syntax))
;; (setq modus-themes-syntax '(yellow-comments green-strings))
(defface org-code '((t :background "grey80")) "a stub to stop org overriding it.")
(load-theme 'modus-operandi)
;; (load-theme 'modus-vivendi)

;; seems that 'modus-themes-markup has no effect
;; if Org is loaded after the theme.. However, loading a theme itakes
;; long time, Seek other ways to fix the problem.

;;* abbrev

;;** abbrev: table

;; "~/.emacs.d/lisp/ram-abbrev.el" abbrevs loaded by init.el

;;** abbrev: settings

(setq abbrev-file-name "~/.emacs.d/lisp/abbrev-defs")

(setq-default abbrev-mode t)
;; (setq save-abbrevs 'silently)
(setq save-abbrevs nil)

;;** abbrev: functions

(defun ram-abbrev-custom-expand-function (expand)
  (let* ((org-src-block-lang (and (eq 'org-mode major-mode)
                                  (with-syntax-table org-mode-syntax-table
                                        (when (org-in-src-block-p 'INSIDE)
                                          (car (org-babel-get-src-block-info))))))
         (org-src-block-abbrevs (if-let* ((org-src-block-lang)
                                          (sym (intern-soft (concat org-src-block-lang "-mode-abbrev-table")))
                                          ((boundp sym)))
                                    (symbol-value sym)
                                  (when (and org-src-block-lang
                                             (boundp 'prog-mode-abbrev-table))
                                    prog-mode-abbrev-table)))
         match tempo-templ str-p comment-p)
    (undo-boundary)
    ;; set str-p and comment-p
    (if org-src-block-lang
        (with-syntax-table
            (or (eval (intern-soft
                       (concat org-src-block-lang "-mode-syntax-table")))
                (syntax-table))
          (setq str-p (nth 3 (syntax-ppss)))
          (setq comment-p (nth 4 (syntax-ppss))))
      (setq str-p (nth 3 (syntax-ppss)))
      (setq comment-p (nth 4 (syntax-ppss))))
    (setq match (tempo-find-match-string tempo-match-finder))
    ;; search in each list in tempo-local-tags
    ;; stop when found,
    ;; this complexity is for the sake of efficiency.
    (setq tempo-templ (and match
                           (cl-loop named 'search-for-tempo-template
                                    with templ
                                    with tags
                                    with tags-lists = tempo-local-tags
                                    while (and (not templ)
                                               (setq tags (car tags-lists)))
                                    do
                                    (setq templ (assoc (car match) (eval (car tags) t)))
                                    (setq tags-lists (cdr tags-lists))
                                    finally return templ)))
    ;; (message "????? org-src-block-abbrevs %S" org-src-block-abbrevs)
    (cond
     (str-p (let ((local-abbrev-table org-mode-abbrev-table))
              (funcall expand)))
     (comment-p (let ((local-abbrev-table org-mode-abbrev-table))
                  (funcall expand)))
     (org-src-block-abbrevs (let ((local-abbrev-table org-src-block-abbrevs))
                              (funcall expand)))
     ((and tempo-templ (not str-p) (not comment-p))
      (delete-region (cdr match) (point))
      (funcall (cdr tempo-templ))
      'ram-abbrev-custom-expand-function-alias
      ;;
      )
     (t (funcall expand))))
  ;;
  )
;; the alias seem vital to handle 'no-self-insert properly by abbrev
(fset 'ram-abbrev-custom-expand-function-alias 'ram-abbrev-custom-expand-function)
(put 'ram-abbrev-custom-expand-function 'no-self-insert t)

;;** abbrev: hooks, advice, timers

;; enable #'ram-abbrev-custom-expand-function-alias for every buffer
;; you may consider separating Org part of the logic if it proves to be too slow
;; (add-function :around (local 'abbrev-expand-function) #'ram-abbrev-custom-expand-function)

(defun ram-abbrev-set-custom-expand-fn ()
  "Add `ram-abbrev-custom-expand-function' to default `abbrev-expand-function'."
  (add-function :around (local 'abbrev-expand-function) #'ram-abbrev-custom-expand-function))

(with-eval-after-load 'org
  (add-hook 'org-mode-hook #'ram-abbrev-set-custom-expand-fn))

(add-hook 'emacs-lisp-mode-hook #'ram-abbrev-set-custom-expand-fn)

;; (setq save-abbrevs nil)

;;** abbrev: auto-correct typo

;;*** abbrev/auto-correct: auto-correct-typo-abbrev-table

;; define auto-correct-typo-abbrev-table if it is not defined
(when (not (boundp 'auto-correct-typo-abbrev-table))
  (if (file-exists-p abbrev-file-name)
      (quietly-read-abbrev-file abbrev-file-name))
  (when (not (boundp 'auto-correct-typo-abbrev-table))
    (define-abbrev-table 'auto-correct-typo-abbrev-table
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
            (if p local-abbrev-table auto-correct-typo-abbrev-table ;; global-abbrev-table
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
  (when (not (equal last-command #'ram-back-sentence))
    (push-mark))
  (backward-sentence arg))

(defun ram-kill-ring-save ()
  "Remove marks after `kill-ring-save' call."
  (interactive)
  (call-interactively #'kill-ring-save)
  (cond
   ((equal last-command #'ram-org-mark-element)
    (set-mark-command '(4))
    (set-mark-command '(4)))))

(define-key global-map "\M-w" #'ram-kill-ring-save)

(define-key global-map (kbd "M-a") #'ram-back-sentence)
;; "M-(" was originally bound to #'insert-parentheses
(global-unset-key (kbd "M-("))
(define-key global-map (kbd "C-%") #'repeat)
(define-key global-map (kbd "C-h a") 'apropos)

;;** bindings: frame
(when (eq (lookup-key (current-global-map) "\C-z")
          #'suspend-frame)
  (define-key global-map "\C-z" nil))

(when (eq (lookup-key (current-global-map) (kbd "C-x C-z"))
          #'suspend-frame)
  (define-key global-map (kbd "C-x C-z") nil))
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

(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(add-hook 'clojure-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 (list '(company-capf)))))
(add-hook 'cider-repl-mode-hook
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
(add-hook 'emacs-lisp-mode-hook 'ram-remap-hl-line-face-in-find-file-hook 0 t)
;; <f19> key is between <f8> and <f9>
(define-key emacs-lisp-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)

;; display eval result inline, use cider for that
;; credit to https://endlessparentheses.com/eval-result-overlays-in-emacs-lisp.html

;; (autoload 'cider--make-result-overlay "cider-overlays")

;; (defun endless/eval-overlay (value point)
;;   (cider--make-result-overlay (format "%S" value)
;;     :where point
;;     :duration 'command)
;;   ;; Preserve the return value.
;;   value)

;; (advice-add 'eval-region :around
;;             (lambda (f beg end &rest r)
;;               (endless/eval-overlay
;;                (apply f beg end r)
;;                end)))

;; (advice-add 'eval-last-sexp :filter-return
;;             (lambda (r)
;;               (endless/eval-overlay r (point))))

;; !!! for some reason, if advice is set without delay (see the line after),
;; it has no effect when emacs starts up.
;; (defun set-displaying-eval-defun-result-inline ()
;;   "Display `eval-defun' results inline."
;;   (advice-add 'eval-defun :filter-return
;;               (lambda (r)
;;                 (endless/eval-overlay
;;                  r
;;                  (save-excursion
;;                    (end-of-defun)
;;                    (point))))))

;; (run-with-idle-timer 1 nil #'set-displaying-eval-defun-result-inline)

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
(define-key vterm-mode-map (kbd "C-.") #'vterm-send-C-x)

(define-key vterm-copy-mode-map (kbd "M-<f9>") #'vterm-copy-mode-done)

;;* exwm

(straight-use-package
 '(exwm :type git :host github :repo "emacs-straight/exwm"))

(require 'exwm)
(require 'exwm-randr)

;; ** exwm: exwm-randr

(setq exwm-randr-workspace-monitor-plist '(
                                           1 "HDMI-1"
                                           2 "HDMI-1"
                                           3 "HDMI-1"
                                           4 "HDMI-1"
                                           5 "HDMI-1"
                                           6 "DP-1"
                                           7 "DP-1"
                                           8 "DP-1"
                                           9 "DP-1"
                                           0 "DP-1"
                                           ))

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

(setq exwm-workspace-show-all-buffers t)

;;*** exwm/settings: mouse pointer

;; when moving workspaces, mouse pointer will follow
(setq exwm-workspace-warp-cursor t)

;; moving mouse pointer will select the window
(setq mouse-autoselect-window t)
(setq focus-follows-mouse t)


;;** exwm: bindings

;; Global keybindings.
(setq exwm-input-global-keys
      `(

        (,(kbd "C-^") . top-level)      ; close all minibuffers
        ;; <XF86Copy> key is in layer where F1-F12 defined, in place of "w" key
        (,(kbd "<M-XF86Copy>") . delete-other-windows)
        (,(kbd "<M-S-XF86Copy>") . delete-window)

        ;; TODO: check if in Portal (Ghrome, or webkit) buffer
        ;; (,(kbd "C-o" . portal.ui.commands/clear-filter))

        ;; (,(kbd "s-a") . org-agenda)

        (,(kbd "<f2> c") . ram-reveal-clear)

        (,(kbd "s-c n") . org-roam-dailies-capture-today)
        (,(kbd "s-c d") . org-roam-dailies-goto-today)
        (,(kbd "s-c f") . ram-org-roam-next-note-dwim)
        (,(kbd "s-c b") . ram-org-roam-prev-note-dwim)
        (,(kbd "s-c c") . org-roam-dailies-goto-date)
        (,(kbd "s-c v") . org-roam-dailies-capture-date)
        (,(kbd "s-c t") . org-roam-dailies-goto-tomorrow)
        (,(kbd "s-c y") . org-roam-dailies-goto-yesterday)

        (,(kbd "C-g") . keyboard-quit)
        ;; (,(kbd "C-h") . help-command)
        ;; ([?\s-q] . kill-buffer-and-window)
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
        ;; (,(kbd "<f15>") . ram-other-workspace)

        ([?\s-t] . ram-org-roam-node-find)

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
                           (exwm-workspace-switch-create ,i)
                           ;; Chrome, Chromium browser looses focus
                           ;; when switching workspaces.
                           ;; https://github.com/ch11ng/exwm/issues/759
                           ;; comment out next line in exwm-layout.el
                           ;; (cl-pushnew xcb:Atom:_NET_WM_STATE_HIDDEN exwm--ewmh-state)
                           )))
                     (number-sequence 0 9)))))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(setq exwm-manage-configurations
      '(((member exwm-instance-name '("qutebrowser"))
         workspace 1)
        ((member exwm-instance-name '("java" "Prolog debugger"))
         workspace 4)
        ;; exwm-instance-name for Google-chrome Portal buffer is localhost
        ((member exwm-class-name '("Google-chrome" "Prolog debugger"))
         workspace 4)))

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

;;*** exwm: hooks, advice, timers

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
              (exwm-layout-toggle-fullscreen))
             ((and exwm-class-name
                   (string-match-p "^Google-chrome\\(<[0-9]+>\\)\\{0,1\\}$" exwm-class-name))
              (exwm-workspace-switch 4)
              (exwm-input-set-local-simulation-keys nil)
              (set-window-fringes (selected-window) 0 0)
              (exwm-layout-toggle-fullscreen))
             ;; ((and exwm-class-name
             ;;       (string= exwm-class-name "MuPDF"))
             ;;  (exwm-workspace-switch 4)
             ;;  (set-window-fringes (selected-window) 0 3)
             ;;  ;; git-gutter adds right fringe
             ;;  (git-gutter-mode -1)
             ;;  (exwm-layout-toggle-fullscreen))
             )))
              ;; (setq mode-line-format nil)

(add-hook 'exwm-manage-finish-hook #'ram-set-org-faces)

(add-hook 'exwm-init-hook (lambda () (setq ram-org-results-table-max-width
                                            ;; allow for column separators,
                                            ;; shrink overlays
                                            (- (window-width) 20))))

;;*** exwm: must be last in exwm settings

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

;; all <M-f*> keys in layer activated with <tab> hold (right outer thumb key)
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

;; !!! You may have to run ~make info~ to create info and dir files
;; read more in the note for "magit info"
;; (add-to-list 'Info-default-directory-list "~/.local/share/emacs/my.emacs.d/straight/repos/magit/docs")
(add-to-list 'Info-directory-list "~/.local/share/emacs/my.emacs.d/straight/repos/magit/docs")

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
      (with-minibuffer-selected-window
        (unless (buffer-local-value 'buffer-read-only (current-buffer))
          (insert candidate)))
      ;; exit minibuffer
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

(defmacro ram-add-to-history-cmd (fn-name history command)
  `(defun ,fn-name ()
     ,(format "Add search string entered in minibuffer to `%s'." (eval history))
     (interactive)
     (let ((search-str (buffer-substring
                        (line-beginning-position) (line-end-position 1))))
       (if (< 3 (length search-str))
           (progn
             (add-to-history ,history search-str))))
     (,(symbol-function command))))

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
       (ram-add-to-history-cmd ram-add-to-describe-variable-history
                               'ram-describe-variable-history
                               minibuffer-force-complete-and-exit))

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

  (let ((default history-add-new-input)
        (history-add-new-input nil))
    ;; (setq history-add-new-input nil)
    (describe-variable variable)
    ;; (setq history-add-new-input default)
    ))

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
       (ram-add-to-history-cmd ram-add-to-describe-function-history
                               'ram-describe-function-history
                               minibuffer-force-complete-and-exit))

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
   (let* ((headlines '())
          (buffer (if (minibufferp)
                      (with-minibuffer-selected-window
                        (current-buffer))
                    (current-buffer)))
          (headline-regex
           ;; use regexp that allow extracting info from headings
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
             "^[[:blank:]]*;;[[:space:]]?\\(?:\\(;+\\)\\|\\(\\*+\\)\\)\\(?: +\\(.*?\\)\\)?[ ]*$")))
          (old-binding-to-return (cdr (assoc 'return (cdr minibuffer-local-completion-map))))
          (reset-keybindings (lambda ()
                               (if old-binding-to-return
                                   (setf (alist-get 'return (cdr minibuffer-local-completion-map)) old-binding-to-return)
                                 (assq-delete-all 'return (cdr minibuffer-local-completion-map)))))
          (hist-item (car ram-jump-to-outline-history)))
     (setf (alist-get 'return (cdr minibuffer-local-completion-map))
           (ram-add-to-history-cmd ram-add-to-jump-to-outline-history
                                   'ram-jump-to-outline-history
                                   minibuffer-force-complete-and-exit))
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
                                      headlines))))
       (error
        (funcall reset-keybindings)
        (signal (car err) (cdr err)))
       (quit
        (funcall reset-keybindings)
        (signal 'quit nil))
       (:success
        (funcall reset-keybindings)
        ;; this returned list is mapped to command args: OUTLINE and SWAP-HISTORY-P
        ;; FIXME: confusing logic, rewrite without using these args
        (list val
              ;; if two items are inserted, swap them so that the search str is first
              (let ((third-element (caddr ram-org-jump-to-name-history))
                    (second-element (cadr ram-org-jump-to-name-history)))
                (and second-element (equal hist-item third-element)))))))

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
    (outline-show-entry)
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
                     "add-to-list"
                     "cl-defmethod"
                     "defalias"
                     "defconst"
                     "defcustom"
                     "defface"
                     "define-abbrev-table"
                     "define-error"
                     "defun"
                     "defun-motion"
                     "defvar"
                     "defvar-local"
                     "define-minor-mode"
                     "defmacro"
                     "defsubst"
                     "cl-defun"
                     "cl-defstruct"
                     "cl-defmethod"
                     "set"
                     "setq"
                     "set-default"
                     "setq-default")
                    (+ space)           ; 1 or more whitespaces
                    (regexp name-regex)))))
   ((or (eq major-mode 'clojure-mode)
        (eq major-mode 'clojurec-mode))
    (rx line-start
        (* space)
        (seq "("
             (group (or
                     "def"
                     "defmacro"
                     "defn"
                     "defn-")
                    (+ space)           ; 1 or more whitespaces
                    (* (or "^:private"))
                    (* space)
                    (regexp name-regex)))))
   ((eq major-mode 'racket-mode)
    (format "^(\\(def\\(?:ine\\) +%s\\)" name-regex))
   (t (error (format "%s is not supported, add regex to `ram-jump-to-def'" major-mode)))))

(defun ram-jump-to-def (def-str &optional swap-history-p)
  "Jump to def."
  (interactive
   (let* ((defs '())
          ;; get the buffer to search through, even if minibuffer is active
          ;; in case `ram-jump-to-def' is invoked from a minibuffer,
          ;; e.g. decided to do the "def" search when doing "headers" search
          (buffer (if (minibufferp)
                      (with-minibuffer-selected-window
                        (current-buffer))
                    (current-buffer)))
          (def-regex (with-current-buffer buffer
                       (ram-jump-to-def-get-regexs major-mode "\\([^[:blank:]\t\r\n\v\f)]+\\)")))
          (hist-item (car ram-jump-to-def-history))
          (str-at-point (thing-at-point 'symbol))
          (old-binding-to-return (cdr (assoc 'return (cdr minibuffer-local-completion-map))))
          (old-binding-to-C-w (cdr (assoc ?\C-w (cdr minibuffer-local-completion-map))))
          (reset-keybindings (lambda ()
                               (if old-binding-to-return
                                   (setf (alist-get 'return (cdr minibuffer-local-completion-map)) old-binding-to-return)
                                 (assq-delete-all 'return (cdr minibuffer-local-completion-map)))
                               (if old-binding-to-C-w
                                   (setf (alist-get ?\C-w (cdr minibuffer-local-completion-map)) old-binding-to-C-w)
                                 (assq-delete-all ?\C-w (cdr minibuffer-local-completion-map))))))
     (setf (alist-get ?\C-w (cdr minibuffer-local-completion-map))
           (lambda (arg)
             "Insert selection and exit."
             (interactive "p")
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
     (setf (alist-get 'return (cdr minibuffer-local-completion-map))
           (ram-add-to-history-cmd ram-add-to-jump-to-def-history
                                   'ram-jump-to-def-history
                                   minibuffer-force-complete-and-exit))
     ;; (setf (alist-get ?\C-w (cdr minibuffer-local-completion-map))
     ;;       (ram-add-to-history-cmd ram-add-to-jump-org-name-history-on-kill
     ;;                               'ram-org-jump-to-name-history
     ;;                               ram-kill-minibuffer-candidate))
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
                                      defs))))
       (error
        (funcall reset-keybindings)
        (signal (car err) (cdr err)))
       (quit
        (funcall reset-keybindings)
        (signal 'quit nil))
       (:success
        (funcall reset-keybindings)
        ;; this returned list is mapped to command args: OUTLINE and SWAP-HISTORY-P
        ;; FIXME: confusing logic, rewrite without using these args
        (list val
              ;; if two items are inserted, swap them so that the search str is first
              (let ((third-element (caddr ram-org-jump-to-name-history))
                    (second-element (cadr ram-org-jump-to-name-history)))
                (and second-element (equal hist-item third-element))))))))

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
(setq icomplete-in-buffer t)
(setq completions-max-height nil)
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

(remove-hook 'icomplete-minibuffer-setup-hook #'prot/icomplete-minibuffer-truncate)

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

(require 'help-fns)

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
  (let* ((win-list (window-list-1 nil 'nomini))
         (win-info-buf-list (seq-filter (lambda (w) (ram-info-buffer-p (window-buffer w) nil)) win-list))
         (win-interactive-buf-list (seq-filter (lambda (w) (ram-interactive-buffer-p (window-buffer w) nil)) win-list)))
    (cond
     ((car win-interactive-buf-list) (quit-window nil (car win-interactive-buf-list)))
     ((car win-info-buf-list) (quit-window nil (car win-info-buf-list)))
     ((cadr win-list) (quit-window nil (cadr win-list))))))

;; (define-key global-map (kbd "s-w") 'ram-quit-other-windows)
;; <XF86Copy> key is in layer where F1-F12 defined, in place of "w" key
(define-key global-map (kbd "<M-XF86Copy>") 'delete-other-windows)
(define-key global-map (kbd "<M-S-XF86Copy>") 'delete-window)
;; (define-key global-map (kbd "s-W") 'delete-window)

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

(defun ram-windmove-swap-up-or-down ()
  "Swap windows up or down."
  (interactive)
  (condition-case err
      (windmove-swap-states-up)
    (user-error (if (string= (error-message-string err)
                             "No window up from selected window")
                    (condition-case err
                        (windmove-swap-states-down)
                      (user-error (if (not (string= (error-message-string err)
                                                    "No window down from selected window"))
                                      (error (signal (car err) (cdr err)))))
                      (error (signal (car err) (cdr err))))
                  (signal (car err) (cdr err))))
    (error (signal (car err) (cdr err)))
    ;; (:success
    ;;  (ram-agenda-files-remove file-path))
    ))

(define-key global-map (kbd "s-w") #'ram-windmove-swap-up-or-down)

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
  (interactive "P")
  (let ((buf (current-buffer)))
    (when (> (length (window-list-1 nil 'nomini)) 1)
      (delete-window))
    ;; do not save buffers that strat with "*"
    (if (or (string-match "^\\*.*$" (buffer-name buf))
            (ram-info-buffer-p buf nil)
            (ram-interactive-buffer-p buf nil))
        (kill-buffer buf)
      (when arg
        (with-current-buffer buf
          (save-buffer)))
      (kill-buffer buf))))
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
    (dolist (window (window-list-1 nil 'nomini))
      (when (ram-info-buffer-p (window-buffer window) nil)
        (push window info-windows)))
    ;; (set-window-buffer (car info-windows) buf)
    (when info-windows
      (window--display-buffer buf (car info-windows) 'reuse alist))))

(defun ram-display-buffer-in-interactive-window (buf alist)
  "Display buffer in window with `ram-interactive-buffer-p' buffer."
  (let (interactive-windows)
    (dolist (window (window-list-1 nil 'nomini))
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
      (dolist (window (window-list-1 nil 'nomini))
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
    (dolist (window (window-list-1 nil 'nomini))
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

(defun ram-create-display-buffer-in-specific-workspace-alist-element (test-buffer-p workspace-idx)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the condition part of the element. The action
part returns a window for displaying the buffer in WORKSPACE-IDX
`exwm-mode' workspace."
  (list test-buffer-p
        `((lambda (buffer alist)
            ,(format "Display BUFFER in exwm desktop %d" workspace-idx)
            (when-let* ((frame (and (frame-parameter (selected-frame) 'exwm-active)
                                    (<= ,workspace-idx (exwm-workspace--count))
                                    (exwm-workspace--workspace-from-frame-or-index ,workspace-idx)))
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
            (when-let* ((primary-frame (and (frame-parameter (selected-frame) 'exwm-active)
                                            (<= ,primary (exwm-workspace--count))
                                            (<= ,secondary (exwm-workspace--count))
                                            (exwm-workspace--workspace-from-frame-or-index ,primary)))
                        (primary ,primary)
                        (secondary ,secondary)
                        (buffer-sameness-p (lambda (frm)
                                             (,test-buffer-p (window-buffer (frame-selected-window frm)))))
                        (selected-frm (selected-frame))
                        ;; decide between primary and secondary workspaces
                        (workspc (cond
                                  ;; with current-prefix-arg stay in
                                  ;; the same frame: return whatever
                                  ;; (primary or secondary) workspc
                                  ;; that shares the same frame as
                                  ;; selected-frame.
                                  (current-prefix-arg
                                   ;; (message "???????? case current-prefix-arg")
                                   ;; (message "???????? %S" alist)
                                   (if (equal (frame-parameter
                                               (selected-frame)
                                               'exwm-randr-monitor)
                                              (frame-parameter
                                               (exwm-workspace--workspace-from-frame-or-index primary)
                                               'exwm-randr-monitor))
                                       primary
                                     secondary))
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
                        ;; swap workspc when ALIST indicates it wants
                        ;; other window or frame, but only if there
                        ;; current-prefix-arg is not used.

                        ;; FIXME: after
                        ;; some use, swapping workspaces seem
                        ;; questionable. For example, Emacs decides to
                        ;; 'inhibit-same-window when opening elisp
                        ;; files. It breaks the expected behavior
                        ;; sometimes. I will comment out it for now.

                        ;; (workspc (if (and (not current-prefix-arg)
                        ;;                   (or (cdr (assq 'inhibit-same-window alist))
                        ;;                       (cdr (assq 'reusable-frames alist))))
                        ;;              (if (= workspc primary) secondary primary)
                        ;;            workspc))

                        ;; Rather, I have opted to remove these action
                        ;; list entries. the 'or' is needed in case
                        ;; assq-delete-all returns the 'nil' (in case
                        ;; of the empty list '())
                        (alist (or (assq-delete-all 'reusable-frames
                                                    (assq-delete-all 'inhibit-same-window alist))
                                   '(())))
                        (workspc-frm (exwm-workspace--workspace-from-frame-or-index workspc))
                        (window-to-display-in (car (window-list-1 nil 'nomini workspc-frm))))
              (when window-to-display-in
                (exwm-workspace-switch workspc-frm)
                ;; when new and selected frame share same monitor, keep new one active
                (when (not (string= (frame-parameter workspc-frm 'exwm-randr-monitor)
                                    (frame-parameter selected-frm 'exwm-randr-monitor)))
                  (exwm-workspace-switch selected-frm))
                (delete-other-windows window-to-display-in)
                (window--display-buffer buffer window-to-display-in 'reuse alist)))))))

(defun ram-create-display-buffer-in-other-monitor-alist-element (test-buffer-p primary secondary)
  "Return an element to be added to `display-buffer-alist'.

This element is of the form (CONDITION . ACTION) where
test-buffer-p is CONDITION. The ACTION function return a window
on either PRIMARY or SECONDARY `exwm-randr-monitor'."

  (list test-buffer-p
        `((lambda (buffer alist)
            ,(format "Display BUFFER in exwm workspace %d or %d" primary secondary)
            (when-let* ((primary-frame (and (frame-parameter (selected-frame) 'exwm-active)
                                            (<= ,primary (exwm-workspace--count))
                                            (<= ,secondary (exwm-workspace--count))
                                            (exwm-workspace--workspace-from-frame-or-index ,primary)))
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
                (delete-other-windows window-to-display-in)
                (window--display-buffer buffer window-to-display-in 'reuse alist)))))))

(defun ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element (test-buffer-p workspace-idx)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the CONDITION part of (CONDITION . ACTION). The
ACTION part returns a `exwm-mode' WORKSPACE-IDX window to display
the buffer.

It splits the window horizontally if it is not displaying the
same buffer or if TEST-BUFFER-P for the buffer is false."
  (list test-buffer-p
        `(lambda (buffer alist)
           ,(format "Display BUFFER in workspace %d in a horizontal split." workspace-idx)
           (when-let* ((target-frame (and
                                      ;; make sure 'exwm is running
                                      (frame-parameter (selected-frame) 'exwm-active)
                                      ;; make sure WORKSPACE-IDX does not exceed number of workspaces
                                      (<= ,workspace-idx (exwm-workspace--count))
                                      ;; get the target workspace
                                      (exwm-workspace--workspace-from-frame-or-index ,workspace-idx)))
                       (target-window (frame-selected-window target-frame))
                       (buffer-name (if (stringp buffer) buffer (buffer-name buffer)))
                       (next-to-target-window (next-window target-window 'nomini target-frame))
                       (windows-in-frame (window-list-1 nil 'nomini target-frame))
                       ;; defined as a fn only because of #'when-let*
                       (get-win-displaying-same-buf
                        (lambda (frame-windows)
                          (car
                           (seq-filter (lambda (w)
                                         (string= buffer-name
                                                  (buffer-name (window-buffer w))))
                                       frame-windows))))
                       (delete-windows-except
                        (lambda (exclude-this-window)
                          (let ((buffer-major-mode
                                 (buffer-local-value 'major-mode (get-buffer buffer))))
                            (when (> (length windows-in-frame) 1)
                              (dolist (w windows-in-frame)
                                (when (and (not (equal w exclude-this-window))
                                           (or
                                            ;; different major-mode
                                            (not (string=
                                                  buffer-major-mode
                                                  (buffer-local-value 'major-mode (window-buffer w))))
                                            ;; same buffer
                                            (string= buffer-name (buffer-name (window-buffer w)))))
                                  (delete-window w))))))))
             (exwm-workspace-switch-create ,workspace-idx)
             (cond
              ;; reuse the same window
              (current-prefix-arg (window--display-buffer buffer target-window 'reuse alist) )
              ;; reuse window displaying same buffer
              ((funcall get-win-displaying-same-buf windows-in-frame)
               (funcall delete-windows-except (funcall get-win-displaying-same-buf windows-in-frame))
               (window--display-buffer buffer (funcall get-win-displaying-same-buf windows-in-frame)
                                       'reuse alist))
              ;; reuse target-window displaying same buffer
              ;; ((string= (buffer-name (window-buffer target-window))
              ;;           (if (stringp buffer) buffer (buffer-name buffer)))
              ;;  (funcall delete-windows-except target-window)
              ;;  ;; (when (> (length windows-in-frame) 1)
              ;;  ;;   (delete-window next-to-target-window))
              ;;  (window--display-buffer buffer target-window 'reuse alist))
              ;; reuse target-window if TEST-BUFFER-P is false
              ((not (,test-buffer-p (window-buffer target-window) nil))
               (funcall delete-windows-except target-window)
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
           (when-let* ((primary-frame (and (frame-parameter (selected-frame) 'exwm-active)
                                           (<= ,primary (exwm-workspace--count))
                                           (<= ,secondary (exwm-workspace--count))
                                           (exwm-workspace--workspace-from-frame-or-index ,primary)))
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
              ;; reuse the same window
              (current-prefix-arg (window--display-buffer buffer target-window 'reuse alist) )
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

(defun ram-create-display-buffer-in-other-monitor-horiz-split-prefer-same-window-alist-element (test-buffer-p primary secondary)
  "Return an element to be added to `display-buffer-alist'.

TEST-BUFFER-P is the CONDITION part of (CONDITION . ACTION). The
ACTION part returns a window to display the buffer in either
PRIMARY or SECONDARY `exwm-mode' workspaces.

It splits the selected window horizontally if it is not
displaying TEST-BUFFER-P buffer.

If another split window exist, prefer the current window, unless
the `current-prefix-arg' is non nil"

  (list test-buffer-p
        `(lambda (buffer alist)
           ,(format "Display BUFFER in exwm desktop %d or %d with horizontal split." primary secondary)
           (if (frame-parameter (selected-frame) 'exwm-active) ; works only for exwm managed windows
               (let* ((primary ,primary)
                      (secondary ,secondary)
                      (buffer-sameness-p (lambda (frm)
                                           (,test-buffer-p (window-buffer (frame-selected-window frm)))))
                      (wkspc-displaying-same-buffer (cond
                                                     ((and
                                                       ;; workspace is visible
                                                       (frame-parameter
                                                        (exwm-workspace--workspace-from-frame-or-index primary) 'exwm-active)
                                                       ;; workspace is displaying the test-buffer-p
                                                       (cl-some
                                                        (lambda (w) (,test-buffer-p (window-buffer w)))
                                                        (window-list
                                                         (exwm-workspace--workspace-from-frame-or-index primary))))
                                                      primary)
                                                     ((and
                                                       ;; workspace is visible
                                                       (frame-parameter
                                                        (exwm-workspace--workspace-from-frame-or-index secondary) 'exwm-active)
                                                       ;; workspace is displaying the test-buffer-p
                                                       (cl-some
                                                        (lambda (w) (,test-buffer-p (window-buffer w)))
                                                        (window-list
                                                         (exwm-workspace--workspace-from-frame-or-index secondary))))
                                                      secondary)))
                      (workspc (cond
                                ;; select workspace that already displaying same buffer
                                (wkspc-displaying-same-buffer)
                                ;; selected frame is displaying sameness buffer, choose it
                                ;; seems like the oboe case shadows the following
                                ;; hence, comment it out for now.
                                ;; ((funcall buffer-sameness-p (selected-frame))
                                ;;  (if (eq (selected-frame) primary-frame)
                                ;;      primary
                                ;;    secondary))
                                (current-prefix-arg
                                 ;; stay in the same
                                 ;; exwm-randr-monitor which displays
                                 ;; the selected-frame
                                 (if (string= (frame-parameter
                                               (selected-frame) 'exwm-randr-monitor)
                                              (frame-parameter
                                               (exwm-workspace--workspace-from-frame-or-index primary)
                                               'exwm-randr-monitor))
                                     primary
                                   secondary))
                                (t
                                 ;; if selected workspace is in the same monitor as the primary,
                                 ;; choose secondary.
                                 (if (string= (frame-parameter
                                               (selected-frame) 'exwm-randr-monitor)
                                              (frame-parameter
                                               (exwm-workspace--workspace-from-frame-or-index primary)
                                               'exwm-randr-monitor))
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
                  ;; reuse next-to-target-window when current-prefix-arg is non nil
                  ((and current-prefix-arg
                        (window-live-p next-to-target-window)
                        (not (eq target-window next-to-target-window)))
                   (window--display-buffer buffer next-to-target-window 'reuse alist))
                  ;; stay in the target-window when next-to-target-window exist
                  ((and (window-live-p next-to-target-window)
                        (not (eq target-window next-to-target-window)))
                   (window--display-buffer buffer target-window 'reuse alist))
                  ;; split window
                  (t (let ((new-window (split-window-no-error target-window nil 'below)))
                       (when new-window
                         (setq new-window (window--display-buffer buffer new-window 'window alist))
                         (balance-windows-area)
                         new-window)))
                  ;; reuse 'target-window
                  (t (window--display-buffer buffer target-window 'reuse alist))))))))

;;***** buffers/display/alist: Help, info, Messages, magit, Completions

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (string-match-p "\\*Help\\*" buf-name)
                      (string-match-p "^\\*info\\*\\(<[0-9]+>\\)?$" buf-name)
                      (string-match-p "\\*Messages\\*" buf-name)
                      (string-match-p "^magit.*$" buf-name))))
              6 4))

;;***** buffers/display/alist: Completions

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (string-match-p "\\*Completions\\*" buf-name)))
              6 4))

;;****** buffers/display/alist: emacs-lisp, elisp

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-primary-workspace-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'emacs-lisp-mode mode)))
              2 8))

;;****** buffers/display/alist: deft

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-alist-element
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

;;******* buffers/display/alist/clojure: cider

;;******** buffers/display/alist/clojure: cider-repl, cider-stacktrace

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (eq 'cider-repl-mode mode)
                      (eq 'cider-stacktrace-mode mode))))
              8 2))

;;******** buffers/display/alist/clojure: cider-result, cider-doc

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-other-monitor-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (string-match-p (or (regexp-quote "*cider-result*")
                                       (regexp-quote "*cider-doc*"))
                                  (if (stringp buffer) buffer (buffer-name buffer)))
                  (string-match-p "\\(?:\\*cider-result\\*\\)\\|\\(?:\\*cider-doc\\*\\)"
                                  (if (stringp buffer) buffer (buffer-name buffer)))))
              8 2))

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

;; (add-to-list 'display-buffer-alist
;;              (ram-create-display-buffer-in-primary-workspace-alist-element
;;               (lambda (buffer &optional alist)
;;                 (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
;;                   (eq 'org-mode mode)))
;;               8 2))

;; #'ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
;; does not display the same buffer in two windows (it closes one). In
;; contrast, when using
;; #'ram-create-display-buffer-in-primary-workspace-alist-element for
;; example, closing a buffer displaying a code snippet of an org
;; document would display the same org document in it. You end up with
;; two windows display the same buffer.

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'org-mode mode)))
              8))

;;****** buffers/display/alist/org: src, source code block buffer

;;****** buffers/display/alist/org/src: clojure

 (add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-same-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (string-match-p "^\\*Org Src .+?\\[ clojure \\]\\*$" buf-name)))))

;;****** buffers/display/alist/org/src: emacs-lisp, elisp

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-same-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (string-match-p "^\\*Org Src .+?\\[ emacs-lisp \\]\\*$" buf-name)))))

;;****** buffers/display/alist: org monthly

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}.org$" buf-name)))
              7))

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
             (ram-create-display-buffer-in-other-monitor-horiz-split-prefer-same-window-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or (eq 'dired-mode mode)
                      (string-match-p "^\\*eshell\\*<[0-9]+>$" buf-name))))
              7 3))

;;****** buffers/display/alist: ESS Emacs Speaks Statistics

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-same-monitor-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
                  (eq 'inferior-ess-r-mode mode)))))

;;****** buffers/display/alist: java, Google-chrome, xwidget-webkit, portal

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-horiz-split-alist-element
              (lambda (buffer &optional alist)
                (let (;; (mode (buffer-local-value 'major-mode (get-buffer buffer)))
                      (buf-name (if (stringp buffer) buffer (buffer-name buffer))))
                  (or
                   (eq 'xwidget-webkit-mode (buffer-local-value 'major-mode (get-buffer buffer)))
                   (string-match-p "^Google-chrome\\(<[0-9]+>\\)\\{0,1\\}$" buf-name)
                   (string-match-p "^java\\(<[0-9]+>\\)\\{0,1\\}$" buf-name)
                   ;; (string-match-p "^MuPDF\\(<[0-9]+>\\)\\{0,1\\}$" buf-name)
                   )))
              4))

;;****** buffers/display/alist: *scratch*

(add-to-list 'display-buffer-alist
             (ram-create-display-buffer-in-specific-workspace-alist-element
              (lambda (buffer &optional alist)
                (string-match-p (regexp-quote "*scratch*")
                                (if (stringp buffer) buffer (buffer-name buffer)))) 0))

;; ****** buffers/display/alist: test

;; (setq display-buffer-alist nil)

;; (add-to-list 'display-buffer-alist
;;              `("*"
;;                ((lambda (buffer alist)
;;                   (progn
;;                     (print
;;                      (format "################ any buffer type : buffer name: %s, mode: %s" buffer
;;                              (buffer-local-value 'major-mode buffer)))

;;                     nil)))))

;; *** buffers: breadcrumbs

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

;;*** org-mode/functions: enable mode only in source code blocks

;; test it as being global for now
(defvar-local ram-org-block-selected-overlay (make-overlay 0 0)
  "An overlay to indicate an Org block with point inside.")

;; (face-attribute 'default :background)

(add-hook 'org-mode-hook (lambda () (setq-local ram-org-block-selected-overlay (make-overlay 0 0))
                           (overlay-put ram-org-block-selected-overlay 'face '(:background "#ffffff" :extend t))
                           (overlay-put ram-org-block-selected-overlay 'priority '(nil . -10))))

(defvar-local ram-last-active-org-bloc-edges nil
  "A record of the active Org code block edges.

The beginning and the end of the inside part of the code block.
Use it to check if you left the block or not.")

(defvar ram-edit-org-block-in-lisp-for-languages
  '("emacs-lisp" "clojure")
  "A list of languages for which to enable lisp editing in Org code blocks.")

(defun ram-toggle-lisp-editing-modes-in-org-code-block ()
  "Enable modes in Org code blocks.

Disable otherwise"
  (setq-local paredit-override-check-parens-function
              ;; suppress #'check-parens error
              (lambda (err) 'ignore-check-parens-error))
  (condition-case err
      (if-let* ((el (with-syntax-table org-mode-syntax-table
                      (org-element-with-disabled-cache (org-element-at-point))))
                ((eq 'src-block (org-element-type el)))
                (org-src-block-lang (org-element-property :language el))
                ((member org-src-block-lang ram-edit-org-block-in-lisp-for-languages))
                ;; content-begin is the point at the beginning of the next line from "#+begin_src"
                (content-begin (save-excursion (goto-char (org-element-post-affiliated el))
                                               (1+ (point-at-eol))))
                ;; content-end is the point at the end of the previous line from "#+end_src"
                (content-end (org-with-point-at (org-element-end el)
                               (skip-chars-backward " \t\n\r")
                               (point-at-bol)))
                (p (point)))
          ;; !!! Remember, only ONE expression for SUCCESS part
          (progn
            ;; (message ">>> if-let* with syntax table:        %s"
            ;;          (if (= 32 (char-syntax (string-to-char "\n")))
            ;;              (format "org-mode-syntax-table %S" (char-syntax (string-to-char "\n")) )
            ;;            (format "NOT org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))))
            ;; we are still in the same block
            (if ram-last-active-org-bloc-edges
                ;; we are still in the code block:
                (progn
                  ;; and we are inside the same code block which means
                  ;; we can keep the minor-modes and setting as they were.
                  ;;   - only move the overlay
                  ;;   - reset syntax table to language specific
                  ;;     because it could have been changed in pre-command hook
                  (if (<= content-begin p content-end)
                      (progn
                        ;; some commands (e.g., org-*) may change syntax table even when
                        ;; executed from withing the code block, reset it back.
                        (set-syntax-table (or (eval (intern-soft
                                                     (concat org-src-block-lang "-mode-syntax-table")))
                                              (syntax-table)))
                        ;; move background overlays unconditionally because
                        ;; block edged may be the same but the lines inside changes
                        (move-overlay ram-org-block-selected-overlay content-begin content-end))
                    ;; although we did not leave the same code block,
                    ;; we are not inside it (but on its borders)
                    ;; we have to disable everything enabled when we move in.
                    ;; the following is repeated code, refactor it.
                    (progn
                      ;; (message "    >>> just moved out of the block: deactivate  everything")
                      (setq-local ram-last-active-org-bloc-edges nil)
                      ;; (setq-local ram-last-active-org-bloc-edges (cons content-begin content-end))
                      (setq-local indent-line-function 'org-indent-line)
                      (move-overlay ram-org-block-selected-overlay content-begin content-end)
                      (ram-highlight-sexps-mode -1)
                      (paredit-mode -1)
                      (ram-manage-sexps-mode -1)
                      (set-syntax-table org-mode-syntax-table)
                      )
                    )
                  )
              ;; we have just moved into the block: activate everything
              ;;   - but only if inside the block
              ;; change the background of the block
              ;; (message "    >>> just moved to block: move overlays")
              ;; (message "        >>>                           indent-line-function %S" (eval 'indent-line-function))
              (move-overlay ram-org-block-selected-overlay content-begin content-end)
              ;; when inside the block activate the modes
              (if (<= content-begin p content-end)
                  (progn
                    ;; reset the syntax-table to one of the code block !!!
                    ;; WARNING, org commands may rely on
                    ;; org-mode-syntax-table
                    ;; !!! unexpected errors when messing with cache
                    ;; (setq org-element-use-cache nil)
                    (set-syntax-table (or (eval (intern-soft
                                                 (concat org-src-block-lang "-mode-syntax-table")))
                                          (syntax-table)))
                    (setq-local indent-line-function 'lisp-indent-line)
                    (ram-highlight-sexps-mode 1)
                    ;; activate first for ram-manage-sexps-mode
                    ;; keybinding precedence over paredit-mode
                    (ram-manage-sexps-mode 1)
                    ;; use save-excursion because paredit may leave point outside the block
                    (save-excursion (paredit-mode 1))
                    ;; (message "        >>>                           indent-line-function %S" (eval 'indent-line-function))
                    (setq-local ram-last-active-org-bloc-edges (cons content-begin content-end)))
                )
              ;;
              )
            ;; an error will be raised if cache is not active
            ;; or if element is not in the cache, so ignore them.
            (ignore-errors (org-element--cache-remove el)))
        ;; (message ">>> if-let* failed for block editing: %s" org-src-block-lang)
        ;; (message ">>> if-let* with syntax table:        %s"
        ;;          (if (= 32 (char-syntax (string-to-char "\n")))
        ;;              (format "org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))
        ;;            (format "NOT org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))
        ;;            ))
        ;; we are outside a code block
        ;; have we just left the block code? if not, do nothing.
        (if ram-last-active-org-bloc-edges
            ;; we just left the block code, deactivate everything
            (progn
              ;; (message "    >>> just moved out of the block: deactivate  everything")
              (setq-local ram-last-active-org-bloc-edges nil)
              (setq-local indent-line-function 'org-indent-line)
              (move-overlay ram-org-block-selected-overlay 1 1)
              (ram-highlight-sexps-mode -1)
              (paredit-mode -1)
              (ram-manage-sexps-mode -1)
              (set-syntax-table org-mode-syntax-table)))
        ;; (message "    >>> were not in block, do NOTHING")
        ;; (message "    >>>             but hide overlays")
        ;; we are not in the code block
        (move-overlay ram-org-block-selected-overlay 1 1)
        ;;
        )
    ;; (user-error (when (not (string= (error-message-string err)
    ;;                                 "No tag to remove"))
    ;;               (signal (car err) (cdr err))))
    ;; ((error user-error) (message ">>>> ram-toggle-lisp-editing-modes-in-org-code-block: %S %S " (car err) (cdr err)))
    ;; use this to get *Backtrace*, but it will evict this fn from the hook
    ((debug error user-error) (signal (car err) (cdr err)))
    (:success nil)))

;;*** org-mode/functions: search, jump to #+name

(defvar ram-org-jump-to-name-history nil
  "`ram-describe-variable' history list.")
(put 'ram-org-jump-to-name-history 'history-length 100)


(defun ram-org-jump-to-name (org-name &optional swap-history-p)
  "Jump to org #+name."
  (interactive
   (let* ((org-names '())
          (buffer (if (minibufferp)
                      (with-minibuffer-selected-window
                        (current-buffer))
                    (current-buffer)))
          (org-name-regex "^#\\+name:\\(?: *\\)\\(.*?\\)?[[:blank:]]*$"
                          ;; (cond ((eq major-mode 'org-mode)            "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$")))
                          )
          (old-binding-to-return (cdr (assoc 'return (cdr minibuffer-local-completion-map))))
          (old-binding-to-C-w (cdr (assoc ?\C-w (cdr minibuffer-local-completion-map))))
          (reset-keybindings (lambda ()
                               (if old-binding-to-return
                                   (setf (alist-get 'return (cdr minibuffer-local-completion-map)) old-binding-to-return)
                                 (assq-delete-all 'return (cdr minibuffer-local-completion-map)))
                               (if old-binding-to-C-w
                                   (setf (alist-get ?\C-w (cdr minibuffer-local-completion-map)) old-binding-to-C-w)
                                 (assq-delete-all ?\C-w (cdr minibuffer-local-completion-map)))))
          (hist-item (car ram-org-jump-to-name-history))
          val)
     (setf (alist-get 'return (cdr minibuffer-local-completion-map))
           (ram-add-to-history-cmd ram-add-to-jump-org-name-history-on-exit
                                   'ram-org-jump-to-name-history
                                   minibuffer-force-complete-and-exit))

     (setf (alist-get ?\C-w (cdr minibuffer-local-completion-map))
           (ram-add-to-history-cmd ram-add-to-jump-org-name-history-on-kill
                                   'ram-org-jump-to-name-history
                                   ram-kill-minibuffer-candidate))

     (condition-case err
         (progn (with-current-buffer buffer
                  (save-excursion
                    ;; 'save-restiction' means
                    ;;   - if the buffer is narrowed:
                    ;;     - we widen the buffer
                    ;;     - execute the code
                    ;;     - restore to the buffer to previous state
                    (save-restriction
                      (widen)
                      (goto-char (point-max))
                      (while (re-search-forward org-name-regex nil t -1)
                        (setq org-names (cons (cons (match-string-no-properties 1) (point)) org-names))))))
                (setq org-names (ram-make-duplicate-keys-unique org-names))
                (setq val (cdr (assoc (completing-read
                                       (format-prompt
                                        "Find #+name:" (car ram-org-jump-to-name-history))
                                       org-names
                                       nil t nil
                                       'ram-org-jump-to-name-history
                                       (car ram-org-jump-to-name-history))
                                      org-names))))
       (error
        (funcall reset-keybindings)
        (signal (car err) (cdr err)))
       (quit
        (funcall reset-keybindings)
        (signal 'quit nil))
       (:success
        (funcall reset-keybindings)
        ;; this returned list is mapped to command args: OUTLINE and SWAP-HISTORY-P
        ;; FIXME: confusing logic, rewrite without using these args
        (list val
              ;; if two items are inserted, swap them so that the search str is first
              (let ((third-element (caddr ram-org-jump-to-name-history))
                    (second-element (cadr ram-org-jump-to-name-history)))
                (and second-element (equal hist-item third-element)))))))

   ;; reorder history so that the search string is fist and the input is second.
   ;; Use it for different order when pressing <M-p> for previous history item.
   (when swap-history-p
     (setq ram-org-jump-to-name-history
           (cons (cadr ram-org-jump-to-name-history)
                 (cons (car ram-org-jump-to-name-history)
                       (cddr ram-org-jump-to-name-history))))))
  (when org-name
    (when (minibufferp)
      (let ((pre-minibuffer-buffer (with-minibuffer-selected-window
                                     (current-buffer))))
        (switch-to-buffer pre-minibuffer-buffer)))
    (push-mark)
    (goto-char org-name)
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

;;*** org-mode/functions: table

(defun ram-org-table-shrink (&optional max-table-width begin end)
  "Shrink columns proportional to MAX-TABLE-WIDTH.

Find max lengths of columns, divide MAX-TABLE-WIDTH proportional to the
max column lengths. Use these values to shrink the each column separately."
  (interactive)
  (unless (or begin (org-at-table-p)) (user-error "Not at a table"))
  (org-with-wide-buffer
   (let* ((max-table-width
           (or max-table-width
               ;; 7 to cover for 3 column separators and shrink overlays
               ;; adjust for your most used cases
               (- (window-width) 7)))
          (begin (or begin (org-table-begin)))
	  (end (or end (org-table-end)))
          (elisp-data (org-babel-read-table))
          ;; do not shrink short columns;
          ;; e.g., less than 50% of average width:
          ;; e.g., 15 char for 4 cols and 120 max width, (* 0.5 (/ 120 4))
          (min-possible-col-width (floor (* .5 (/ max-table-width
                                                  ;; number of columns
                                                  (length (car elisp-data))))))
          (wide-and-all-colls
           (cl-labels ((separate-cols
                         (table wide-cols all-cols)
                         (cond
                          ;; terminate recursion
                          ((null table) (list wide-cols all-cols))
                          ;; skip 'hline symbol
                          ((and (symbolp (car table)) (equal (car table) 'hline))
                           (separate-cols (cdr table) wide-cols all-cols))
                          ;; process the row
                          (t (cl-labels ((process-row (row counter)
                                           (if (null row)
                                               ;; terminate this recursion, pass control to outer recursion
                                               (separate-cols (cdr table) wide-cols all-cols)
                                             (let* ((el (car row))
                                                    (current-max (alist-get counter wide-cols 0))
                                                    (el-width (length (if (not (stringp el))
                                                                          (format "%s" el)
                                                                        el))))
                                               ;; update wide-cols with new values
                                               (when (and
                                                      ;; allow strings only, MIND than col names are always strings
                                                      (stringp el )
                                                      ;; exclude datetimes,
                                                      ;; is the regexp too narrow?
                                                      (not (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}.*$" el))
                                                      (>  el-width current-max)
                                                      (> el-width min-possible-col-width)
                                                      )
                                                 (progn
                                                   (setq wide-cols (assq-delete-all counter wide-cols))
                                                   (push (cons counter el-width) wide-cols)))
                                               ;; update all-cols with new max values
                                               (and (>  el-width current-max)
                                                    (progn
                                                      (setq all-cols (assq-delete-all counter all-cols))
                                                      (push (cons counter el-width) all-cols)))
                                               ;; next iteration
                                               (process-row (cdr row) (1+ counter))))))
                               (process-row (car table) 1)
                               ;;
                               ))
                          ;;
                          )
                         ))
             (separate-cols elisp-data nil nil)))
          (do-not-shrink-cols (cl-labels ((subset-alist (l-1 l-2)
                                            (cond
                                             ((null l-1) l-2)
                                             ((null l-2) l-1)
                                             (t (subset-alist (cdr l-1) (assq-delete-all (caar l-1) l-2))))))
                                (subset-alist (car wide-and-all-colls)
                                              (cadr wide-and-all-colls))))
          ;; subtract width of cols that you do not shrink
          ;; from max-table-width
          (total-width-of-non-shrink-cols (cl-labels ((add-alist-vals (val cols)
                                                        (if (null cols)
                                                            val
                                                          (add-alist-vals (+ val (cdar cols)) (cdr cols)))))
                                            (add-alist-vals 0 do-not-shrink-cols)))
          (max-table-width-minus-non-shrink-cols (- max-table-width total-width-of-non-shrink-cols))
          ;; some outlier be extremely long,
          ;; keep them withing max-possible-col-width
          (max-possible-col-width (floor (*
                                          ;; allow only 30%
                                          1.3
                                          ;; from average max col width, which is
                                          (/ max-table-width-minus-non-shrink-cols
                                             ;; divided by number of colums (but not zero, there might not be cols to shrink)
                                             (max 1 (length (car wide-and-all-colls)))))))
          ;; keep outlier widths withing max-possible-col-width
          (wide-cols (cl-labels ((update-widths (cols)
                                   (if (null cols)
                                       nil
                                     (let ((el (car cols)))
                                       (cons (cons (car el)
                                                   (min
                                                    (cdr el)
                                                    max-possible-col-width))
                                             (update-widths (cdr cols)))))))
                       (update-widths (car wide-and-all-colls))))
          (total-width-of-shrink-cols (cl-labels ((add-alist-vals (val cols)
                                                    (if (null cols)
                                                        val
                                                      (add-alist-vals (+ val (cdar cols)) (cdr cols)))))
                                        (add-alist-vals 0 wide-cols)))
          (cols-to-shrink-new-widths (cl-labels ((update-widths (cols)
                                                   (if (null cols)
                                                       nil
                                                     (let ((el (car cols)))
                                                       (cons (cons (car el)
                                                                   ;; new width is proportional to old width
                                                                   ;; but only if it is less than the old
                                                                   (min (cdr el)
                                                                        (floor (* (/ (float (cdr el)) total-width-of-shrink-cols )
                                                                                  max-table-width-minus-non-shrink-cols))))
                                                             (update-widths (cdr cols)))))))
                                       (update-widths wide-cols)))
          (sort-fn )
	  (columns)
          (removed-cols)
          (current-column))
     ;; get the max cell lengths in each column
     (org-table-expand begin end)
     ;; Make sure invisible characters in the table are at the right
     ;; place since column widths take them into account.
     (font-lock-ensure begin end)
     ;; do nothing when shrunk values are the same as non-shrunk,
     ;; otherwise, inserted overlays add clutter.
     (when (not (equal (sort cols-to-shrink-new-widths (lambda (a b) (< (car a) (car b))))
                       (sort (car wide-and-all-colls) (lambda (a b) (< (car a) (car b))))))
       (ram-org-table--shrink-columns (sort cols-to-shrink-new-widths (lambda (a b) (< (car a) (car b)))) begin end))
     )))

(defun ram-org-table--shrink-columns (columns-widths beg end)
  "Quick hack of `org-table--shrink-columns'.
COLUMNS-WIDTHS is an alist '((column-number . width)...)."
  (org-with-wide-buffer
   (font-lock-ensure beg end)
   (let ((columns (mapcar (lambda (el) (car el)) columns-widths)))
     (dolist (c columns)
       (goto-char beg)
       (let ((fields nil))
         (while (< (point) end)
	   (catch :continue
	     (let* ((hline? (org-at-table-hline-p))
		    (separator (if hline? "+" "|")))
	       ;; Move to COLUMN.
	       (search-forward "|")
	       (or (= c 1)              ;already there
		   (search-forward separator (line-end-position) t (1- c))
		   (throw :continue nil)) ;skip invalid columns
	       ;; Extract boundaries and contents from current field.
	       ;; Also set the column's width if we encounter a width
	       ;; cookie for the first time.
	       (let* ((start (point))
		      (end (progn
			     (skip-chars-forward (concat "^|" separator)
					         (line-end-position))
			     (point)))
		      (contents (if hline? 'hline
				  (org-trim (buffer-substring start end)))))
	         (push (list start end contents) fields)
	         )))
	   (forward-line))
         ;; Link overlays for current field to the other overlays in the
         ;; same column.
         (let ((chain (list 'siblings)))
	   (dolist (field fields)
	     (dolist (new (apply #'org-table--shrink-field
			         (or (cdr (assq c columns-widths)) 0) "l" field))
	       (push new (cdr chain))
	       (overlay-put new 'org-table-column-overlays chain)))))))))

;;*** org-mode/functions: manage time property

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

;;*** org-mode/functions: misc

(defun ram-hide-block-toggle ()
  "Toggle visibility from anywhere in the block."
  (interactive)
  (cond
   ((org-at-block-p) (org-hide-block-toggle))
   ((org-in-src-block-p)
    (org-previous-block 1)
    (org-hide-block-toggle))
   (t (org-hide-block-toggle))))

(defun ram-org-get-fist-id-property (file)
  "Return the first ID property found.
Based on `org-id-update-id-locations'."
  (let ((id-regexp
	 (rx (seq bol (0+ (any "\t ")) ":ID:" (1+ " ") (not (any " ")))))
        (id-found nil))
    (with-temp-buffer
      (delay-mode-hooks
        (org-mode)
        (when (file-exists-p file)
          (insert-file-contents file nil nil nil 'replace)
          (let ((case-fold-search t))
            (org-with-point-at 1
	      (while (and (re-search-forward id-regexp nil t)
                          (not id-found))
	        (when (org-at-property-p)
                  (setq id-found (org-entry-get (point) "ID"))))
	      )))))
    id-found))

(defun ram-assoc-clojure-org-code-block-buffer-with-file ()
  "Associate a Clojure Org source code block buffer with a file.

The file is a part of a Clojure project. This is done so that
Clojure editing tools, e.g., lsp-mode, are enabled."
  (when (string= major-mode 'clojure-mode)
    (write-region (point-min) (point-max)
                  "~/backup/projects/clojure/hello-clojure/src/org_code/example.clj"
                  nil       ; overwrite, do not append
                  t         ; mark buffer as visiting the file
                  nil       ; no name for locking, unlocking
                  nil       ; do not ask for confirmation to overwrite
                  )
    ;; the buffer is already in 'clojure-mode
    ;; is it ok to run the 'clojure-mode-hook the second time?
    ;; or should I just call #'lsp fn, (as the hook does).
    ;; For now, opted for running the hook because some other
    ;; functionality triggered in hooks might require
    ;; a visiting file rather than just a buffer
    (run-mode-hooks 'clojure-mode-hook)))

(defun ram-assoc-prolog-org-code-block-buffer-with-file ()
  "Associate a Prolog Org source code block buffer with a file.

This is done because prolog-consult-buffer fails on unsaved buffers."
  (when (string= major-mode 'prolog-mode)
    (write-region (point-min) (point-max)
                  "/tmp/org-code-block-for-prolog.pl"
                  nil       ; overwrite, do not append
                  t         ; mark buffer as visiting the file
                  nil       ; no name for locking, unlocking
                  nil       ; do not ask for confirmation to overwrite
                  )
    ;; (run-mode-hooks 'prolog-mode-hook)
    ))

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

;;*** org-mode/functions: wrap in symbols ~, =

(defun ram-wrap-in-~ ()
  "Insert \"~\" in front of a sexp and after it."
  (interactive)
  (let ((new-syntax-table (copy-syntax-table org-mode-syntax-table)))
    (with-syntax-table
        new-syntax-table
      (modify-syntax-entry ?\: "w" new-syntax-table)
      (backward-sexp 1)
      (cond
       ((= (char-before) ?\:) (backward-char)))
      (insert "~")
      (backward-sexp -1)
      (insert "~"))))

(defun ram-wrap-in-= ()
  "Insert \"=\" in front of a sexp and after it."
  (interactive)
  (let ((new-syntax-table (copy-syntax-table org-mode-syntax-table)))
    (with-syntax-table
        new-syntax-table
      (modify-syntax-entry ?\: "w" new-syntax-table)
      (backward-sexp 1)
      (cond
       ((= (char-before) ?\:) (backward-char)))
      (insert "=")
      (backward-sexp -1)
      (insert "="))))

;;*** org-mode/functions: navigate blocks

(setq ram-org-block-navig-regexp
      "^[ ]*#\\+begin_\\(?:\\(?:src\\)\\|\\(?:example\\)\\|\\(?:quote\\)\\)[[:space:]]*\\(?1:[[:alpha:]_-]*\\).*?$")

(defun ram-org-previous-block (arg)
  "Jump to previous code block without raising the error.

Leave a mark to return to."
  (interactive "p")
  (condition-case err
    (progn
      ;; Push mark for the first call so than you can return to the orig bloc
      (when (not (eq this-command last-command))
        (push-mark))
      ;; (re-search-backward "^#\\+begin_\\(?:src\\)\\|\\(?:example\\)[[:space:]]\\(?1:[[:alpha:]_-]+\\).*?$")
      (re-search-backward ram-org-block-navig-regexp)
      ;; consider moving to the same language blocks (captured by group 1)
      ;; (ram-push-mark-for-none-consecutive-cmd arg #'org-babel-previous-src-block)
      )
    (user-error (when (not (string= (error-message-string err)
                                    "No previous code blocks"))
                  (signal (car err) (cdr err))))
    ;; consider action if no more blocks above.
    (search-failed (message "No more blocks above."))
    (error (signal (car err) (cdr err)))
    ;; (:success
    ;;  nil)
    ))

(defun ram-org-next-block (arg)
  "Jump to next code block without raising the error.

Leave a mark to return to."
  (interactive "p")
  (condition-case err
      (let ((p (point)))
        ;; Push mark for the first call so than you can return to the orig bloc
        (when (not (eq this-command last-command))
          (push-mark))
        ;; (ram-push-mark-for-none-consecutive-cmd arg #'org-next-block)
        ;; (ram-push-mark-for-none-consecutive-cmd arg #'org-babel-next-src-block)
        (when (eq this-command last-command)
          ;; otherwise it would match the same thing
          (forward-char 1))
        (re-search-forward ram-org-block-navig-regexp)
        (if (= p (match-beginning 0))
            ;; we started at the block beginning
            ;; and ended at the same place, repeat.
            (ram-org-next-block arg)
          (goto-char (match-beginning 0))))
    (user-error (when (not (string= (error-message-string err)
                                    "No next code blocks"))
                  (signal (car err) (cdr err))))
    (search-failed (message "No more blocks above."))
    (error (signal (car err) (cdr err)))
    ;; (:success
    ;;  nil)
    ))

;;*** org-mode/functions: navigate #+name

(defun ram-org-next-name (arg)
  "Jump to next #+NAME: element.

Leave a mark to return to."
  (interactive "p")
  ;; regexp is defined in #'org-link-search
  (let ((re "^[ \t]*#\\+NAME: +.+[ \t]*$")
        (point (point)))
    (condition-case err
        (progn (when (looking-at-p re)
                 (end-of-line))
               (re-search-forward re))
      (search-failed (goto-char point))
      (:success
       (progn
         (unless (eq this-command last-command)
           (push-mark point))
         (beginning-of-line))))))

(defun ram-org-previous-name (arg)
  "Jump to previous #+NAME: element.

Leave a mark to return to."
  (interactive "p")
  ;; regexp is defined in #'org-link-search
  (let ((re "^[ \t]*#\\+NAME: +.+[ \t]*$")
        (point (point)))
    (condition-case err
        (re-search-backward re)
      (search-failed (goto-char point))
      (:success
       (progn (unless (eq this-command last-command)
                (push-mark point))
              (beginning-of-line))))))

(defun ram-scroll-up-command ()
  "Scroll up one line."
  (interactive)
  (scroll-up-command 1))

(defun ram-scroll-down-command ()
  "Scroll down one line."
  (interactive)
  (scroll-up-command -1))


;;** org-mode: bindings

(with-eval-after-load "org"
  (define-key org-mode-map (kbd "<C-tab>") #'ram-hide-block-toggle)
  ;; originally, C-' runs the command org-cycle-agenda-files
  (define-key org-mode-map (kbd "C-'") nil)

  (define-key org-mode-map (kbd "M-S-<f5>") #'ram-org-jump-to-name)

  (define-key org-mode-map (kbd "M-<f19>") #'ram-org-next-block)
  (define-key org-mode-map (kbd "C-c M-f") #'ram-org-next-block)
  (define-key org-mode-map (kbd "M-<f20>") #'ram-org-previous-block)
  (define-key org-mode-map (kbd "C-c M-b") #'ram-org-previous-block)

  (define-key org-mode-map (kbd "C-M-<f19>") #'ram-org-next-name)
  (define-key org-mode-map (kbd "C-M-<f20>") #'ram-org-previous-name)

  (define-key org-mode-map (kbd "C-c C-n") #'org-next-link)
  (define-key org-mode-map (kbd "C-c C-p") #'org-previous-link)

  (define-key org-mode-map (kbd "C-c z") 'ram-org-hide-block-toggle-all)
  ;; originally bound to 'org-table-copy-down
  (define-key org-mode-map (kbd "<S-return>") 'newline-and-indent)

  (defun ram-org-mark-element ()
    "Better mark Handling when calling `org-mark-element'.

If inside the element, `push-mark' to return to. Remote marks
left by `org-mark-element`."
    (interactive)
    (push-mark)
    (org-mark-element))

  ;; (define-key org-mode-map (kbd "M-h") #'ram-org-mark-element)
  (define-key org-mode-map (kbd "C-~") #'ram-wrap-in-~)
  (define-key org-mode-map (kbd "C-=") #'ram-wrap-in-=)

  (define-key org-mode-map (kbd "s-R") #'ram-avy-goto-org-heading)
  (define-key org-mode-map (kbd "s-l") #'ram-avy-goto-org-link)

  (define-key global-map (kbd "C-c C-S-L") #'org-store-link)
  (define-key org-mode-map (kbd "C-c C-l") #'org-insert-link)

  (define-key org-mode-map (kbd "M-p") #'ram-scroll-up-command)
  (define-key org-mode-map (kbd "M-n") #'ram-scroll-down-command))

;;** org-mode: cache

;; do not cache the parsed elements
(setq org-element-use-cache t)
;; keep the setting if it does not slow you down
;; rendering daily notes into weekly and monthly is too slow
;; without caching


;;** org-mode: settings

(setq org-blank-before-new-entry
      '((heading . always) (plain-list-item . nil)))

;; toggle it with "C-c C-x \" or "M-x org-toggle-pretty-entities"
(setq org-pretty-entities t)

(with-eval-after-load "org"
  (add-to-list 'org-tags-exclude-from-inheritance "project"))

;;** org-mode: tables

(setq org-startup-shrink-all-tables t)


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

;;*** org-mode/settings: tables

(setq org-startup-align-all-tables t)

;; enable org-indent-mode for every file
;; per file sittings: #+STARTUP: indent, #+STARTUP: noindent
(setq org-startup-indented t)
(setq org-indent-mode-turns-on-hiding-stars t)
(setq org-hide-leading-stars t)
(setq org-indent-mode-turns-off-org-adapt-indentation t)
(setq org-link-file-path-type 'absolute)
(setq org-fontify-quote-and-verse-blocks t)
(setq org-list-allow-alphabetical t)
(setq org-use-sub-superscripts '{})

;;** org-mode: syntax-table

(defun ram-org-mode-syntax-table-update ()
  "Update `org-mode-syntax-table'."
  ;; the default for ?' is word constituent, breaks expansion
  (modify-syntax-entry ?\' "'" org-mode-syntax-table)
  ;; the default for ?~ is symbol constituent, breaks expansion
  (modify-syntax-entry ?\~ "'" org-mode-syntax-table)
  ;; make ?: a word constituent
  ;; (modify-syntax-entry ?\: "w" org-mode-syntax-table)
  ;; the default for ?< is open delimiter constituent, fails (check-parens)
  ;; make it a symbol constituent, like "+", "-" etc
  (modify-syntax-entry ?\< "_" org-mode-syntax-table)
  (modify-syntax-entry ?\> "_" org-mode-syntax-table)
  )

;;** org-mode: faces, fonts

;; (setq org-src-block-faces
;;       '(("clojure" (:family "Operator Mono Medium" :weight 'normal :background "#EEFFEE"))))

;; (setq org-src-block-faces
;;         '(("emacs-lisp" (:background "#EEE2FF"))
;;           ("python" (:background "#e5ffb8"))))

;; !!! Including :weight property would reset
;; font-lock-string-face and font-lock-doc-face that are set to light.
;; (with-eval-after-load "org"
;;   (set-face-attribute
;;    'org-block nil
;;    :family "Operator Mono Medium"))


;; (custom-theme-set-faces
;;    'user
;;    ;; '(variable-pitch ((t (:family "Verdana" :height 180 :weight light))))
;;    ;; '(variable-pitch ((t (:family "LucidaGrande" :height 190 :weight light))))
;;    ;; '(variable-pitch ((t (:family "SourceSansPro" :height 220 :weight normal))))
;;    '(variable-pitch ((t (:family "Bembo" :height 260 :weight normal))))
;;    ;; '(variable-pitch ((t (:family "BemboStd" :height 260 :weight normal :style regular))))
;;    ;; '(variable-pitch ((t (:family "ETbb" :height 240 :weight normal))))
;;    '(fixed-pitch ((t (:family "Operator Mono" :height 190 :weight semi-light)))))

(defun ram-set-org-faces (&optional frame)
  "Modify Org faces.

Use it from `exwm-manage-finish-hook' because when `org-mode-hook' is
run, the window for the buffer is not ready and the target frame could
not be determined. Hence, incorrect faces (from different monitor) could
be used."
  ;; (custom-theme-set-faces
  ;;  'user
  ;;  ;; '(variable-pitch ((t (:family "Verdana" :height 180 :weight light))))
  ;;  ;; '(variable-pitch ((t (:family "LucidaGrande" :height 190 :weight light))))
  ;;  ;; '(variable-pitch ((t (:family "SourceSansPro" :height 220 :weight normal))))
  ;;  '(variable-pitch ((t (:family "Bembo" :height 260 :weight normal))))
  ;;  ;; '(variable-pitch ((t (:family "BemboStd" :height 260 :weight normal :style regular))))
  ;;  ;; '(variable-pitch ((t (:family "ETbb" :height 240 :weight normal))))
  ;;  '(fixed-pitch ((t (:family "Operator Mono Medium-19" :height 190 :weight light)))))
  (let* ((buffer (current-buffer))
         (mode (buffer-local-value 'major-mode buffer))
         (org-buffer-p (eq 'org-mode mode))
         (frame (window-frame (get-buffer-window buffer))))
    (when org-buffer-p
      (variable-pitch-mode)
      (set-face-attribute 'org-code frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-drawer frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-date frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-document-info-keyword frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-indent frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-meta-line frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-table frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-formula frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-verbatim frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-block-begin-line frame :inherit 'fixed-pitch)
      (set-face-attribute 'org-block frame :inherit 'fixed-pitch))))



;; (with-eval-after-load "org"
;;   (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-code nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-date nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-document-info-keyword nil :inherit 'fixed-pitch)
;;   (with-eval-after-load 'org-indent-mode
;;     (set-face-attribute 'org-indent nil :inherit 'fixed-pitch))
;;   (set-face-attribute 'org-meta-line nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-table nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-verbatim nil :inherit 'fixed-pitch)
;;   (set-face-attribute 'org-block-begin-line nil :inherit 'fixed-pitch))

;;** org-mode: emphasis

;; modify org-emphasis-regexp-components, 3rd entry, to include char to emphasis markup
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
(with-eval-after-load "org"
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\""))

(defface ram-org-emphasis-bold
  '((default (:inherit bold))
    (((class color) (min-colors 88)) (:foreground "grey40")))
  "Custom emphasis face for Org bold.")

(defface ram-org-code-face
  '((default (:inherit org-code))
    (((class color) (min-colors 88))
     (:foreground "#8f0075" :background "#f0f0f0")))
  "My custom emphasis face for org-code.")

(defface ram-org-verbatim-face
  '((default (:inherit org-code))
    (((class color) (min-colors 88))
     (:foreground "#005a5f" :background "#f0f0f0")))
  "My custom emphasis face for org-verbatim.")

(defface ram-org-emphasis-italic
  '((default (:inherit italic))
    (((class color) (min-colors 88)) (:weight light)))
  "My custom emphasis face for Org italic.")

;; (with-eval-after-load
;;     (add-to-list 'org-emphasis-alist '("/" (:background "green"))))
(setq org-emphasis-alist
  '(("*" ram-org-emphasis-bold)
    ;; ("/" (:family "Operator Mono Light" :slant italic))
    ("/" ram-org-emphasis-italic)
    ("_" underline)
    ("=" ram-org-verbatim-face)
    ("~" ram-org-code-face)
    ("+" (:strike-through t))))

(setq org-hide-emphasis-markers t)
;; setting this to nil "unhides" the emphasis markers
;; (setq org-descriptive-links nil)
(setq org-src-window-setup 'current-window)
(setq org-hide-leading-stars nil)
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-return-follows-link t)

;;** org-mode: code blocks

;;** org-mode/code blocks: indent

;; do not indent code in code-blocks
(setq org-src-preserve-indentation nil)

(setq org-edit-src-content-indentation 0)

;; non-nil, use language major-mode for indentation paredit calls
;; #'check-parens. Apparently, Org attempts to open temp buffer to
;; indent the code. if check-parens fails, the buffer is left hanging.
;; The source block is impossible to edit after that.
(setq org-src-tab-acts-natively nil)


(setq org-imenu-depth 7)

;; credit to https://stackoverflow.com/a/7165419/9913235
(font-lock-add-keywords 'org-mode '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face)))

;; this will affect derived modes too.
;; (defun add-quotes-to-font-lock-keywords ()
;;   (font-lock-add-keywords nil '(("\"\\(\\(?:.\\|\n\\)*?[^\\]\\)\"" 0 font-lock-string-face))))

;; (add-hook 'text-mode-hook 'add-quotes-to-font-lock-keywords)

;;** org-mode: links

(defcustom org-hidden-links-additional-re "<<[<]?[[:print:]]+>>[>]?"
  "Regular expression that matches strings where the invisible-property is set to org-link."
  :type '(choice (const :tag "Off" nil) regexp)
  :group 'org-link)
(make-variable-buffer-local 'org-hidden-links-additional-re)

(defun org-activate-hidden-links-additional (limit)
  "Put invisible-property org-link on strings matching `org-hide-links-additional-re'."
  (if org-hidden-links-additional-re
      (re-search-forward org-hidden-links-additional-re limit t)
    (goto-char limit)
    nil))

(add-hook 'org-font-lock-set-keywords-hook (lambda ()
                         (add-to-list 'org-font-lock-extra-keywords
                              '(org-activate-hidden-links-additional
                                (0 '(face org-target invisible org-link))))))

;;** org-mode: LaTeX

(setq org-format-latex-options
      '(
        :foreground default
        :background default
        :scale 2.5
        :html-foreground "Black"
        :html-background "Transparent"
        :html-scale 1.0
        :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))


;;** org-mode: images, img

;; org-toggle-inline-images (C-c C-x C-v)

(setq org-startup-with-inline-images t)
(setq org-image-actual-width nil)

;;** org-mode: structure-templates, snippets

;; https://orgmode.org/manual/Structure-Templates.html

;; For example, type "<el" and press TAB key to expand
(with-eval-after-load "org"
  (require 'org-tempo)
  (add-to-list 'org-structure-template-alist
               '("el" . "src emacs-lisp"))
  (add-to-list 'org-structure-template-alist
               '("cl" . "src clojure :results silent :noweb yes"))
  (add-to-list 'org-structure-template-alist
               '("cls" . "src clojurescrsipt"))
  (add-to-list 'org-structure-template-alist
               '("rac" . "src racket :lang racket/base :results output"))
  (add-to-list 'org-structure-template-alist
               '("pl" . "src prolog :results silent"))
  (add-to-list 'org-structure-template-alist
               '("r" . "src R :results output"))
  (add-to-list 'org-structure-template-alist
               '("rs" . "src R :results value :session my-R-session"))
  (add-to-list 'org-structure-template-alist
               '("rgr" . "src R :file /tmp/R-img.png :results output graphics file :session my-R-session"))
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

(with-eval-after-load 'cider
  (setq org-babel-clojure-backend 'cider))

(setq python-shell-interpreter "/usr/bin/python3")
(setq org-babel-python-command "python3")

(with-eval-after-load "org"
  (autoload 'ob-racket "ob-racket")
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((shell . t)
     (js . t)
     (emacs-lisp . t)
     (clojure . t)
     ;; for dot to work install:
     ;; sudo apt-get install graphviz to use dot
     (dot . t)
     (lisp . t)
     (latex . t)
     (prolog . t)
     (python . t)
     (R . t)
     ;; (racket . t)
     ;; (sparql . t)
     ;; (scribble . t)
     (css . t)
     (haskell .t))))

;;*** org-mode/org-babel: prolog

;; (setq org-babel-prolog-backend 'ediprolog)

(straight-use-package
 '(ob-prolog :type git :flavor melpa :host github :repo "ljos/ob-prolog"))
(setq org-babel-prolog-command "swipl")
;;*** org-mode/org-babel: R

;;**** org-mode/org-babel/R: ESS Emacs Speaks Statistics

(straight-use-package
 '(ess :type git
       :flavor melpa
       :files ("lisp/*.el" "doc/ess.texi"
               ("etc" "etc/*")
               ("obsolete" "lisp/obsolete/*")
               (:exclude "etc/other")
               "ess-pkg.el")
       :host github :repo "emacs-ess/ESS"))



;; (eval-after-load 'ess
;;   ;; <f2> <f2> call ram-prolog-dwim
;;   ;; create a custom fn
;;   ;; that would recognize the source blocks
;;   (define-key org-mode-map (kbd "<f2> <f2>") #'ess-switch-to-end-of-ESS))

;;*** org-mode/org-babel: org-babel-eval-in-repl

(with-eval-after-load "ob"
  ;; (require 'eval-in-repl-racket)
  (require 'org-babel-eval-in-repl)
  (setq org-babel-prompt-command "PROMPT_COMMAND=;PS1=\"%s\";PS2=")
  ;; always wrap results output in #+begin_example block
  ;; works only for :results output
  (setq org-babel-min-lines-for-block-output 0)
  ;; this is relevant when you run ober-eval-block-in-repl
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

;;*** org-mode/org-babel: hooks, advice, timers

(add-hook 'org-babel-after-execute-hook #'org-display-inline-images)
;; when another image (graph) is generate by an Org code block
;; the old cached image purist, clear the image cache.
(add-hook 'org-babel-after-execute-hook #'clear-image-cache)
;; (remove-hook 'org-babel-after-execute-hook 'clear-image-cache)

(defvar ram-org-results-table-max-width nil
    "Max table width that would fit on the screen.

If the result table width exceeds that value, shrink columns.")

(defun ram-org-babel-shrink-results-table ()
  "Call `ram-org-table-shrink' on the table result."
  (save-excursion
    (when (org-babel-where-is-src-block-result)
      (goto-char (org-babel-where-is-src-block-result))
      (forward-line 1)
      (when (org-at-table-p)
        (ram-org-table-shrink ram-org-results-table-max-width)))
    ))

(add-hook 'org-babel-after-execute-hook #'ram-org-babel-shrink-results-table)

;;**** org-mode/org-babel/hooks, advice, timers: apply ansi colors to output

;; credit to https://emacs.stackexchange.com/a/63562
;; this function will apply ansi color code in the output of
;; shell code blocks
(defun ek/babel-ansi ()
  (when-let ((beg (org-babel-where-is-src-block-result nil nil)))
    (save-excursion
      (goto-char beg)
      (when (looking-at org-babel-result-regexp)
        (let ((end (org-babel-result-end))
              (ansi-color-context-region nil))
          (ansi-color-apply-on-region beg end))))
    (delete-trailing-whitespace)))

(add-hook 'org-babel-after-execute-hook 'ek/babel-ansi)

;;** org-mode: orgit

;; Link to Magit buffers from Org documents
;; https://github.com/magit/orgit
(straight-use-package
 '(orgit :type git :flavor melpa :host github :repo "magit/orgit"))

;;** org-mode: hooks, advice, timers

;; (add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook #'org-fold-hide-block-all)
(add-hook 'org-mode-hook #'org-display-inline-images)
(add-hook 'org-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'ram-org-mode-syntax-table-update)

(add-hook 'org-src-mode-hook #'ram-assoc-clojure-org-code-block-buffer-with-file)
(add-hook 'org-src-mode-hook #'ram-assoc-prolog-org-code-block-buffer-with-file)
(add-hook 'org-src-mode-hook #'display-line-numbers-mode)

;; (add-hook 'org-mode-hook
;;           (lambda () (add-hook 'post-command-hook
;;                                #'ram-toggle-lisp-editing-modes-in-org-code-block 0 'local)))

;; (remove-hook 'org-mode-hook
;;           (lambda () (add-hook 'post-command-hook
;;                                #'ram-toggle-lisp-editing-modes-in-org-code-block 0 'local)))

(defun ram-switch-on-org-mode-syntax-table ()
  "Ensure that `org-mode-syntax-table' is on for org commands.
When editing Org code block, they may use their own syntax
tables. Reset the table for org commands"
  ;; (message "------- command: %S" this-command)
  ;; (message "-------   table: %S"
  ;;          (if (= 32 (char-syntax (string-to-char "\n")))
  ;;              (format "org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))
  ;;            (format "NOT org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))
  ;;            ))
  (condition-case err
      (when (and ram-edit-org-block-in-lisp-for-languages
                 (and (symbolp this-command)
                      (not (eq 'org-self-insert-command this-command) )
                      (eq 'ram-copy-line this-command)
                      (or (eq 'org-babel-execute-src-block this-command)
                          (string-prefix-p "org" (symbol-name this-command))
                          (string-prefix-p "ram-org" (symbol-name this-command)))))
        ;; (message "?????? switching syntax table:")
        (set-syntax-table org-mode-syntax-table)
        ;; (ignore-errors (let ((el (org-element-at-point)))
        ;;                  (when (eq 'src-block (org-element-type el))
        ;;                    (org-element--cache-remove el))))
        (ignore-errors (org-element-cache-reset))
        ;; (setq org-element-use-cache t)
        ;; (message "?????? table:  %S"
        ;;          (if (= 32 (char-syntax (string-to-char "\n")))
        ;;              (format "org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))
        ;;            (format "NOT org-mode-syntax-table %S" (char-syntax (string-to-char "\n")))
        ;;            ))
        ;; reset the var to indicate that things changed
        ;; in case you remain in the same code block after the command
        ;; However, if you leave the block, the nil value would not
        ;; keep the block related minor-modes on
        ;; (setq ram-edit-org-block-in-lisp-for-languages nil)
        )
    ((debug error user-error) (signal (car err) (cdr err)))
    (:success nil)))

(add-hook 'org-mode-hook
          (lambda () (add-hook 'pre-command-hook
                               #'ram-switch-on-org-mode-syntax-table 0 'local)))

;; (remove-hook 'org-mode-hook
;;           (lambda () (add-hook 'pre-command-hook
;;                                #'ram-switch-on-org-mode-syntax-table 0 'local)))

(run-with-idle-timer 1 nil #'require 'org)

;;* org-agenda

;;** org-agenda: bindings

;; (define-key global-map (kbd "s-a") #'org-agenda)

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

;;** org-roam: init, initialize

(setq org-roam-v2-ack t)
(setq org-roam-directory (file-truename "~/backup/org/org-roam/"))
(defvar ram-org-roam-notes-directory "./notes"
  "A subdirectory of `org-roam-directory' to store notes.

Keep all subdirectories neat and tidy, store `org-roam' notes to
this subdirectory.")

;;** org-roam: install

(straight-use-package
 '(org-roam :type git :flavor melpa
            :files (:defaults "extensions/*" "org-roam-pkg.el")
            :host github :repo "org-roam/org-roam"))
;(require 'org-roam)

;;** org-roam: bindings

(define-key global-map (kbd "C-c n f") #'ram-org-roam-node-find)
(define-key global-map (kbd "C-c n i") #'org-roam-node-insert)
(define-key global-map (kbd "C-c n l") #'org-roam-buffer-toggle)
(define-key global-map (kbd "C-c n g") #'org-roam-graph)
(define-key global-map (kbd "s-t") #'ram-org-roam-node-find)
(define-key global-map (kbd "s-T") #'org-roam-node-insert)

(define-key global-map (kbd "s-c u") #'ram-org-roam-update-org-id-locations)

(define-key ram-leader-map-tap-global (kbd "/") #'ram-org-roam-node-find)
(define-key ram-leader-map-tap-global (kbd "'") #'org-roam-node-insert)

;;** org-roam: buffer

;;*** org-roam/buffer: functions

(cl-defun ram-org-roam-backlinks-section (node &key (unique nil) (show-backlink-p nil))
  "Use custom SQL collecting backlinks with `ram-org-roam-backlinks-get'.

UNIQUE is not used."
  (when-let ((backlinks (seq-sort #'org-roam-backlinks-sort (ram-org-roam-backlinks-get node))))
    (magit-insert-section (org-roam-backlinks)
      (magit-insert-heading "Backlinks:")
      (dolist (backlink backlinks)
        (when (or (null show-backlink-p)
                  (and (not (null show-backlink-p))
                       (funcall show-backlink-p backlink)))
          (org-roam-node-insert-section
           :source-node (org-roam-backlink-source-node backlink)
           :point (org-roam-backlink-point backlink)
           :properties (org-roam-backlink-properties backlink))))
      (insert ?\n))))

(cl-defun ram-org-roam-backlinks-get (node)
  "Same as `org-roam-backlinks-get'.

Use custom SQL to:
- include
  - \"id\"
  - \"ram-org-roam-id-el\"
 type links.
- exclude daily, weekly, monthly backlinks.

Return the list of backlink objects.
"
  (let* ((backlinks (org-roam-db-query
                     (format
                      "SELECT source, dest, links.pos, links.properties
            FROM links INNER JOIN nodes ON links.source = nodes.id
            WHERE
            dest LIKE '\"%s%%%%\"'
            AND (type = '\"id\"' OR type = '\"ram-org-roam-id-el\"')
            AND nodes.file NOT LIKE '\"%%%%/daily/%%%%'
            AND nodes.file NOT LIKE '\"%%%%/weekly/%%%%'
            AND nodes.file NOT LIKE '\"%%%%/monthly/%%%%'
                        "
                      (org-roam-node-id node))) ))
    (cl-loop for backlink in backlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) backlink))
                       (org-roam-populate
                        (org-roam-backlink-create
                         :source-node (org-roam-node-create :id source-id)
                         :target-node (org-roam-node-create :id dest-id)
                         :point pos
                         :properties properties))))))

;; *** org-roam/buffer: backlinks

;; **** org-roam/buffer/backlinks: functions

(defun ram-org-roam-preview-function ()
  "Return the preview content at point.

This function returns the current paragraph."
  (let ((beg (max (save-excursion
                    (org-roam-end-of-meta-data t)
                    (point))
                  (save-excursion
                    (org-backward-paragraph)
                    (point))))
        (end (min (save-excursion
                    (org-next-visible-heading 1)
                    (point))
                  (save-excursion
                    (org-forward-paragraph)
                    (point)))))
    (string-trim (buffer-substring-no-properties beg end))))

;; **** org-roam/buffer/backlinks: settings

(eval-after-load 'org-roam
  '(setq org-roam-preview-function #'ram-org-roam-preview-function))

;;** org-roam: functions

(defun ram-org-roam-node-find ()
  "Store a new note in a subdirectory of `org-roam-directory'."
  (interactive current-prefix-arg)
    (let ((org-roam-directory (expand-file-name ram-org-roam-notes-directory org-roam-directory)))
   (call-interactively #'org-roam-node-find 'RECORD-FLAG)))

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
               (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory)
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
                               ;; (.#) excludes lock files (info "(emacs)Interlocking")
                               ;; dailies
                               (directory-files
                                (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory)
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

(defun ram-org-roam-update-org-id-locations ()
  "Call `org-roam-update-org-id-locations' with set DIRECTORY."
  (interactive)
  (org-roam-update-org-id-locations "~/backup/books"))

;;** org-roam: capture-templates

(with-eval-after-load "org-roam"
  (setq org-roam-capture-templates
        '(("d" "default"
           entry "* ${title}\n\n%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+TITLE: ${title}\n#+CREATED: %U\n#+LAST_MODIFIED: %U\n#+DATE: %<%Y-%m-%d %a>")
           :unnarrowed t))))

;;** org-roam: deft

;; https://github.com/jrblevin/deft
;; deft is for quickly browsing, filtering, and editing directories of notes.

(straight-use-package
 '(deft :type git :flavor melpa :host github :repo "jrblevin/deft"))

;;*** org-roam/deft: settings

(setq deft-extensions '("org"))
(with-eval-after-load "deft"
  (setq deft-directory org-roam-directory))
;; (setq deft-ignore-file-regexp "your regex here")
;; (setq deft-strip-title-regexp "your regex here")

;; search subdirectories
(setq deft-recursive t)
(setq deft-recursive-ignore-dir-regexp
      (concat "\\(?:"
              "\\."
              "\\|\\.\\."
              "\\|"
              "\\(?:^\\.[:print:]*$\\)"
              "\\|"
              "\\(?:^ltximg$\\)"
              "\\)$"))

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

(setq deft-file-limit 65)

;;*** org-roam/deft: functions

;; (when (not (boundp 'org-list-full-item-re))
;; (require 'org))

(defvar org-list-full-item-re)

(defun ram-get-deft-strip-summary-regexp-for-notes ()
  "Return a regexp to exclude from note content."
  (require 'org)
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
        ;; (deft-current-sort-method 'title)
        (deft-current-sort-method 'mtime)
        (deft-recursive t)
        (deft-recursive-ignore-dir-regexp
         (concat deft-recursive-ignore-dir-regexp
                 "\\|"
                 "\\(?:/dailies/\\)"))
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
                      (setq title-match (replace-regexp-in-string (deft-base-filename file) "^[0-9]\\{14\\}-" ""))))))

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
  "Invoke `deft' after setting `deft-directory' to `ram-org-roam-daily-notes-directory'"
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
  (cl-letf ((deft-directory (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory))
            (deft-strip-summary-regexp (ram-get-deft-strip-summary-regexp-for-notes))
            (deft-recursive nil)
            ;; an ugly hack: if deft-parse-summary produces an empty string,
            ;; return a message saying so, the deft-chomp is the smallest fn
            ;; to redefine, this is way, redefine deft-parse-summary later
            ((symbol-function 'deft-chomp)
             (lambda (str)
               "Return a message there is the result is an empty string."
               (let ((parsed-str (replace-regexp-in-string "\\(^[[:space:]\n]*\\|[[:space:]\n]*$\\)" "" str)))
                 (if (string= parsed-str "")
                     "no content"
                   parsed-str))))
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

(setq org-roam-mode-sections (list #'ram-org-roam-backlinks-section
                                   #'org-roam-reflinks-section))

;; (setq org-roam-database-connector 'sqlite-builtin)
(setq org-roam-database-connector 'sqlite)
(with-eval-after-load "org-roam"
  (org-roam-db-autosync-mode))

;;*** org-roam/settings: symlinks

;; see system/general settings: file, symlink
;; where (setq find-file-visit-truename t) is set

;;** org-roam: links

;;** org-roam/links: custom

(with-eval-after-load 'org-roam
  (org-link-set-parameters
   "ram-org-roam-id-el"
   :follow #'ram-org-roam-id-and-element-open
   :store #'ram-org-roam-id-and-element-store-link
   ;; :export #'ram-org-roam-id-and-element-export-link
   )
  (defun ram-org-roam-id-and-element-open (path &optional _)
    "Follow link by using id, then by what comes after \"::\"."
    (let* ((id-desc (split-string path "::" 'OMIT-NULLS "[ ]+"))
           (id (car id-desc))
           (desc (cadr id-desc)))
      (org-roam-id-open id _)
      (condition-case err
	  (org-link-search desc)
        ;; Save position before error-ing out so user
        ;; can easily move back to the original buffer.
        (error (funcall save-position-maybe)
	       (error (nth 1 err))))))
  (defun ram-org-roam-id-and-element-store-link ()
    "Store `ram-org-roam-id-el'."
    (when-let ((link-plist (and
                            ;; symlinks to files in repositories do not work correctly
                            ;; either org-roam-buffer-p fails or magit-status.
                            ;; see a note one "... debug symlink in org-roam-directory ..."
                            ;;(org-roam-buffer-p)
                            (derived-mode-p 'org-mode)
                            (let* ((el (org-element-at-point))
                                   (link-vals (list
                                               ;; get heading
                                               (ignore-error (wrong-type-argument stringp nil)
                                                 (org-get-heading 'NO-TAGS 'NO-TODO 'NO-PRIORITY 'NO-COMMENT))
                                               ;; get closest to the point ID
                                               (org-with-wide-buffer
                                                (org-back-to-heading-or-point-min t)
                                                (while (and (not (org-id-get))
                                                            (not (bobp)))
                                                  (org-roam-up-heading-or-point-min))
                                                (org-id-get))
                                               ;; get element :name
                                               (org-element-property :name el))))
                              (pcase link-vals
                                ;; 1. has a :name property
                                ((and `(,_ ,id ,name)  (guard (and id name)))
                                 (list :id id :search-str name :description name))
                                ;; 2. has a headline
                                ((and `(,heading ,id ,_) (guard (and heading id)))
                                 (list :id id :search-str heading :description heading))
                                ;; 3. has a title
                                ((and `(,_ ,id ,_) (guard id))
                                 (let ((title (cadr (car (org-collect-keywords '("title"))))))
                                   (if title
                                       (list :id id :description title)
                                     (list :id id))))
                                ;; default: explicitly return nil
                                (_ nil))
                              ))))
      (if (and (plist-get link-plist :search-str)
               (plist-get link-plist :description))
          (org-link-store-props
           :type "ram-org-roam-id-el"
           :link (concat "ram-org-roam-id-el"
                         ":"
                         (plist-get link-plist :id)
                         "::"
                         (plist-get link-plist :search-str))
           :description (plist-get link-plist :description))
        (if (plist-get link-plist :description)
            (org-link-store-props
                :type "id"
                :link (concat "id"
                              ":"
                              (plist-get link-plist :id))
                :description (plist-get link-plist :description))
            (org-link-store-props
             :type "id"
             :link (concat "id"
                           ":"
                           (plist-get link-plist :id))))))))

;;** org-roam: dailies

;;*** org-roam/dailies: functions

(defun ram-buffer-is-from-roam-dailies-directory-p ()
  "Return non-nil if the currently visited buffer is from `ram-org-roam-daily-notes-directory'."
  (and buffer-file-name
       (string-prefix-p
        (expand-file-name (file-name-as-directory ram-org-roam-daily-notes-directory))
        (file-name-directory buffer-file-name))))

;;*** org-roam/dailies: settings

(defvar ram-org-roam-daily-notes-directory "./dailies/daily/"
  "A subdirectory of `org-roam-directory' for daily notes.")

;; reset the original value "./daily" to the value of
;; ram-org-roam-daily-notes-directory
(setq org-roam-dailies-directory ram-org-roam-daily-notes-directory)

;;*** org-roam/dailies: capture templates

(defun ram-org-parse-heading-element (el filename &optional include-backlink-p)
  "Transform Org element EL into headline element if possible.

Include a backlink to source if INCLUDE-BACKLINK-P is true."
  (let* ((file filename)
         (headline-props (cl-case (org-element-type el)
                           (headline `(
                                       :title ,(plist-get (cadr el) :title)
                                       :level 1
                                       :pre-blank 0
                                       :raw-value ,(plist-get (cadr el) :raw-value)))
                           (src-block `(
                                        :title ,(plist-get (cadr el) :name)
                                        :level 1
                                        :pre-blank 0
                                        :raw-value ,(plist-get (cadr el) :name)
                                        :tags ,(list (plist-get (cadr el) :language))))
                           (t `(
                                :title ,(format
                                         "parsing of the element \"%S\" is not supported"
                                         (org-element-type el))
                                :level 1
                                :pre-blank 0
                                :tags ("unsupported-element")))))
         (backlink (when include-backlink-p
                     (if-let* ((link (ram-org-roam-id-and-element-store-link)))
                         (cons 'link
                               (cons `(
                                       :type "ram-org-roam-id-el"
                                       :path ,(progn (string-match "^[^:]+:\\([[:print:]]+\\)$"
                                                                   (plist-get link :link))
                                                     (match-string 1 (plist-get link :link)))
                                       :format bracket
                                       :raw-link ,(plist-get link :link))
                                     '("backlink"))))
                     (cl-case (org-element-type el)
                       (headline (cons 'link
                                       (cons `(
                                               :type "file"
                                               :path ,file
                                               :format bracket
                                               :raw-link ,(format "file:%s::*%s" file
                                                                  (plist-get headline-props :raw-value))
                                               :search-option ,(concat "*" (plist-get headline-props :raw-value)))
                                             '("backlink"))))
                       (t (user-error "Creating the backlink to element \"%S\" is not supported" (org-element-type)))))))
    ;; heading
    ;; get just headline, no section etc
    (list 'headline headline-props
          (list 'section '(:post-blank 0 :pre-plank 0)
                (when backlink
                  backlink)))))

;; EXAMPLE: this fn is no longer used, it is left here for now as an
;; example of org-element-map and org-element-parse-buffer examples.
;; (defun ram-org-get-heading-for-capturing ()
;;   "Return the current heading and links in its section element."
;;   (let* ((file-name (file-truename (buffer-file-name)))
;;          (current-headline (save-excursion
;;                              (condition-case nil
;;                                  (outline-back-to-heading)
;;                                (error
;;                                 (user-error "Before first headline at position %d in buffer %s"
;; 		                            (point) (current-buffer)))
;;                                (:success (org-element-at-point)))))
;;          ;; seem like org-element-at-point does not return all needed heading data
;;          ;; hence, need to extract it with org-element-parse-buffer
;;          (headline-element (car (org-element-map
;;                                     (org-element-parse-buffer)
;;                                     'headline
;;                                   (lambda (headline)
;;                                     (when (string= (org-element-property :raw-value headline)
;;                                                    (org-element-property :raw-value current-headline))
;;                                       headline))))))
;;     (org-element-interpret-data (ram-org-parse-heading-element headline-element file-name 'INCLUDE-BACKLINK-P))))


(defun ram-org-capture-element-to-dailies (&optional arg)
  "Capture the current headline into a `org-roam' daily note.
Insert into daily note for ARG days from now. Or use calendar if
ARG value is 4."
  (interactive "P")
  (require 'org-roam-dailies)
  (let* ((backlink (or (ram-org-roam-id-and-element-store-link)
                       (let ((heading (when-let* ((heading
                                                   (org-get-heading 'no-tags 'no-togos 'no-priority 'no-comment)))
                                        (substring-no-properties (org-link-escape heading))))
                             (title (cadr (car (org-collect-keywords '("title"))))))
                         (cond
                          (heading (list :type "file"
                                         :link (format "file:%s::*%s"
                                                       ;; (file-truename (buffer-file-name))
                                                       (buffer-file-name (buffer-base-buffer))
                                                       heading)
                                         :description heading))
                          (title (list :type "file"
                                       :link (format "file:%s::/%s/"
                                                     ;; (file-truename (buffer-file-name))
                                                     (buffer-file-name (buffer-base-buffer))
                                                     title)
                                       :description title))
                          (t (signal 'user-error '("Could not get Org heading")))))))
         (backlink-str (org-link-make-string (plist-get backlink :link)
                                             "backlink"))
         (templates
          `(("d" "capture org element as a heading an backlink"
             ;; entry ,(concat (ram-org-get-heading-for-capturing) " %(org-set-tags \":write:\")  " "\n" backlink "\n\n%?")
             ;; entry ,(ram-org-get-heading-for-capturing)
             entry ,(concat "* "
                            (or (plist-get backlink :description)
                                "link to this entry has no description")
                            "\n" backlink-str "\n\n%?")
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d %a>")
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
    (let ((org-roam-directory (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory)))
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
Insert into daily note for ARG days from today. Or use calendar if
ARG value is 4."
  (interactive "P")
  (save-excursion
    (beginning-of-buffer)
    (ram-org-capture-element-to-dailies arg)))

(defun ram-org-capture-defun-to-dailies (&optional arg)
  "Capture the defun into a `org-roam' daily note.
Insert into daily note for ARG days from now. Or use calendar if
ARG value is 4."
  (interactive "P")
  (require 'org-roam-dailies)
  (let* ((defun-regex (save-excursion
                        (when (not (or (bobp)
                                       (and (= (char-before) ?\n)
                                            (= (char-after) ?\())))
                          (beginning-of-defun))
                        (let ((beg (point))
                              (end (progn (down-list)
                                          (forward-symbol 2))))
                          (buffer-substring-no-properties beg end))))
         (backlink (format "[[file:%s::%s][source]]" (buffer-file-name) defun-regex))s
         (defun-name (car (last (split-string defun-regex))))
         (templates
          `(("d" "capture document title"
             entry ,(concat "* " defun-name  " %(org-set-tags \":"
                            (car (split-string (symbol-name major-mode) "-"))
                            ":\")\n" backlink "\n\n%?")
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d %a>")
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
    (let ((org-roam-directory (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory)))
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
  (require 'org-roam-dailies)
  (let* ((repo (abbreviate-file-name default-directory))
         (rev (if (eq major-mode 'magit-status-mode)
                  ;; this form is straight from
                  ;; #'magit-copy-section-value
                  (oref (magit-current-section) value)
                ;; this stopped working after recompiling Emacs
                ;; (magit-copy-section-value nil)
                (magit-git-string "rev-parse" "HEAD")))
         (backlink (format "[[orgit-rev:%s::%s][%s]]" repo rev (substring rev 0 7)))
         (summary (substring-no-properties (magit-format-rev-summary rev)))
         (templates
          `(("d" "capture document title"
             entry ,(format "* %s %%(org-set-tags \":git:\")\n%s\n\n%%?"
                            summary backlink)
             :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+DATE: %<%Y-%m-%d %a>")
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
    (let ((org-roam-directory (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory)))
      (org-roam-capture- :goto nil
                         :node (org-roam-node-create)
                         :props (list :override-default-time time)
                         :templates templates))
    (org-align-tags)))

(with-eval-after-load "org-roam-dailies"
  ;; (setq time-stamp-format "[%Y-%02m-%02d %3a %02H:%02M]")
  (setq org-roam-dailies-capture-templates
        '(("d" "default" plain ""
           :target (file+head "%<%Y-%m-%d>.org" "#+TITLE: %<%Y-%m-%d>\n#+LAST_MODIFIED: %U\n#+DATE: %<%Y-%m-%d %a>")
           :empty-lines-before 1
           :empty-lines-after 1
           :unnarrowed t
           :kill-buffer nil
           :no-save nil))))

;;*** org-roam/dailies: navigate notes

(defun ram-org-roam-prev-note-dwim (&optional n)
  "Goto the previous daily, weekly or monthly note."
  (interactive "p")
  (require 'org-roam-dailies)
  (cond
   ((and (buffer-file-name)
         (org-roam-dailies--daily-note-p))
    (let ((current-note-time
           (time-convert (date-to-time (file-name-base (buffer-file-name))) 'integer)))
      (org-roam-dailies--capture (time-add (* (- n) 86400) current-note-time) t)))
   ((ram-org-roam-weekly-note-p)
    (ram-org-roam-weeklies-next -1))
   ((ram-org-roam-monthly-note-p)
    (ram-org-roam-monthly-note-next -1))
   (t (org-roam-dailies-goto-today)
      (save-buffer)
      (org-roam-dailies-goto-previous-note)
      (delete-other-windows))))

(defun ram-org-roam-next-note-dwim (&optional n)
  "Goto the next note of the same type as the current one."
  (interactive "p")
  (require 'org-roam-dailies)
  (cond
   ((and (buffer-file-name)
         (org-roam-dailies--daily-note-p))
    (let ((current-note-time
           (time-convert (date-to-time (file-name-base (buffer-file-name))) 'integer)))
      (org-roam-dailies--capture (time-add (* n 86400) current-note-time) t)))
   ((ram-org-roam-weekly-note-p)
    (ram-org-roam-weeklies-next 1))
   ((ram-org-roam-monthly-note-p)
    (ram-org-roam-monthly-note-next 1))
   (t (org-roam-dailies-goto-today)
      (save-buffer)
      ;; (org-roam-dailies-goto-next-note)
      (org-roam-dailies-goto-tomorrow 1)
      (delete-other-windows))))

;;*** org-roam/dailies: bindings

;; these command are ###autoload and 'org-roam-dailies-map is not
;; (with-eval-after-load "org-roam-dailies"
;;   (define-key global-map (kbd "s-c") org-roam-dailies-map))
(with-eval-after-load "org"
  (define-key org-mode-map (kbd "s-c a") #'ram-org-capture-element-to-dailies)
  (define-key org-mode-map (kbd "s-c A") #'ram-org-capture-title-to-dailies))

(define-key emacs-lisp-mode-map (kbd "s-c a") #'ram-org-capture-defun-to-dailies)

(with-eval-after-load "magit"
  (define-key magit-status-mode-map (kbd "s-c a") #'ram-org-capture-magit-commit-to-dailies)
  (define-key magit-revision-mode-map (kbd "s-c a") #'ram-org-capture-magit-commit-to-dailies))

(define-key global-map (kbd "s-c w") #'ram-org-capture-weekly-note)

(define-key global-map (kbd "s-c m") #'ram-org-capture-monthly-note)

(define-key global-map (kbd "s-c n") #'org-roam-dailies-capture-today)
(define-key global-map (kbd "s-c d") #'org-roam-dailies-goto-today)
(define-key global-map (kbd "s-c f") #'ram-org-roam-next-note-dwim)
(define-key global-map (kbd "s-c b") #'ram-org-roam-prev-note-dwim)
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

;;** org-roam: monthly

;;*** org-roam/monthly: functions

(defun ram-org-roam-monthly-note-p (&optional file)
  "Return t if FILE is a monthly note.
Use the current buffer file-path if FILE is nil."
  (when-let ((buffer-name
              (or file
                  (buffer-file-name (buffer-base-buffer))))
             (path (expand-file-name
                    buffer-name))
             (directory (expand-file-name ram-org-roam-monthly-notes-directory org-roam-directory)))
    (setq path (expand-file-name path))
    (save-match-data
      (and (org-roam-file-p path)
           (f-descendant-of-p path directory)))))

(defun ram-org-roam-monthly-note-next (&optional n)
  "Goto or create next Nth monthly note."
  (pcase-let* ((file-name (file-name-base (buffer-file-name (buffer-base-buffer))))
               (month-from-buffer-name (string-to-number (cadr (split-string file-name "-"))))
               (year-from-buffer-name (string-to-number (car (split-string file-name "-"))))
               (`(,target-month ,target-year) (let ((val (+ month-from-buffer-name n)))
                                                (cond
                                                 ((< val 1) (list (+ 12 val) (1- year-from-buffer-name)))
                                                 ((> val 12) (list (- val 12) (1+ year-from-buffer-name)))
                                                 (t (list val year-from-buffer-name))))))
    (ram-org-capture-monthly-note nil (encode-time 1 1 0 1 target-month target-year))))

(defun ram-org-capture-monthly-element (month-1st-day)
  "Return an org-element for a month built from weekly headings."
  (let ((day-of-week (let ((dow (nth 6 (decode-time month-1st-day))))
                       (if (= dow 0) 6 (1- dow)))))
    (cl-labels
        ((get-week (week-day)
           (if (not (= (nth 4 (decode-time week-day)) (nth 4 (decode-time month-1st-day)))) ; different month
               '()
             (cons
              (cons 'headline
                    (cons (let ((week-day-heading (format (format-time-string "%b w %%s" week-day)
                                                          ;; week number in the month
                                                          ;; (1+ (- (/ (time-to-day-in-year week-day) 7)
                                                          ;;        (/ (time-to-day-in-year month-1st-day) 7)))
                                                          (1+ (- (string-to-number (format-time-string "%W" week-day))
                                                                 (string-to-number (format-time-string "%W" month-1st-day)))))))
                            `(:raw-value ,week-day-heading
                                         :pre-blank 0
                                         :post-blank 2
                                         :level 1
                                         :title ,(list week-day-heading)))
                          (cl-labels ((demote-headings (hs)
                                        (cond
                                         ((null hs) '())
                                         ((eq (org-element-type (car hs)) 'headline)
                                          (cons
                                           (cons
                                            (caar hs) (cons (plist-put (cadar hs)
                                                                       :level
                                                                       (1+ (plist-get (cadar hs) :level)))
                                                            (demote-headings (cddar hs))))
                                           (demote-headings (cdr hs))))
                                         (t (cons (car hs) (demote-headings (cdr hs)))))))
                            (demote-headings (ram-org-get-daily-headings-from-week week-day nil t)))))
              (get-week
               ;; recurs only Mondays
               (if (= 1 (nth 6 (decode-time week-day))) ; Monday?
                   ;; yes, go to the next Monday
                   (time-add (* 7 86400) week-day)
                 ;; go from this day-of-week to the following Monday
                 (time-add (* (- 7 day-of-week) 86400) week-day)))))))
      (get-week month-1st-day))))

;; FIXME: same as for capturing (creating) weekly notes
;; 1. headlines  for a day should preserve their level
;;    fold headlines beyond top level.
;; 2. rename "create" to "capture" for consistency
;;    (could not find this fn with "capture weekly" search)
;; 3. May be add links to the actual notes.
(defun ram-org-capture-monthly-note (&optional arg time)
  "Create a note of all weeks in an ARG month from now.
Use calendar if ARG value is '(4).
When ARG is 1, update the current note."
  (interactive "P")
  (require 'org)
  (let* ((point (point))
         (screen-line (- (line-number-at-pos (point)) (line-number-at-pos (window-start))))
         (time (or
                time
                ;; if arg is 1, update the current note
                ;; use the time of the current monthly note
                (if (and arg (numberp arg) (= arg 1)
                         (ram-org-roam-monthly-note-p))
                    (let ((year-month (split-string (file-name-base (buffer-file-name)) "-")))
                      (encode-time 1 1 0 1 (string-to-number (cadr year-month))
                                   (string-to-number (car year-month)))))
                (let ((time (if (equal arg '(4))
                                (let ((org-read-date-prefer-future t))
                                  (org-read-date nil 'TO-TIME nil "Capture to monthly note: " ))
                              (current-time))))
                  (let* ((time-decoded (decode-time time))
                         (month (nth 4 time-decoded))
                         (year  (nth 5 time-decoded))
                         (month-1st-day (encode-time 1 1 0 1 month year)))
                    month-1st-day
                    ;; (time-add (* (or arg 0) 7 86400) (current-time))
                    ))))
         (doc-title (format-time-string "%Y-%m" time))
         (file-name (file-name-concat (expand-file-name ram-org-roam-monthly-notes-directory
                                                        org-roam-directory)
                                      (file-name-with-extension doc-title "org")))
         (note-exists-p (or (get-buffer (file-name-nondirectory file-name))
                            (file-readable-p file-name)))
         (doc-id (if note-exists-p
                     (ram-org-get-fist-id-property file-name)
                   (org-id-new)))
         (doc-text (concat (concat ":PROPERTIES:\n"
                                   (format ":ID:       %s\n" doc-id)
                                   ":END:\n"
                                   (format "#+TITLE: %s\n" doc-title)
                                   (format "#+CREATED: [%s]\n"
                                           (format-time-string (org-time-stamp-format t t) time))
                                   (format "#+DATE: %s\n\n"
                                           (format-time-string "%Y %b" time)))
                           (org-element-interpret-data (ram-org-capture-monthly-element time)))))
    (find-file file-name)
    (erase-buffer)
    (insert doc-text)
    (save-buffer)
    ;; move point:
    ;; if arg is 1, update the current note and remain at the same point
    ;; else, go to the 1st heading
    (if (and arg (numberp arg) (= arg 1)
             (ram-org-roam-monthly-note-p))
        (progn (goto-char (min point (point-max)))
               (recenter screen-line))
      (goto-char (point-min))
      (org-next-visible-heading 1))))


;;*** org-roam/monthly: settings

(defvar ram-org-roam-monthly-notes-directory "./dailies/monthly/"
  "A subdirectory of `org-roam-directory' for monthly notes.")

;;** org-roam: weeklies

;;*** org-roam/weeklies: functions

(defun ram-org-roam-weekly-note-p (&optional file)
  "Return t if FILE is a weekly note.
Use the current buffer file-path if FILE is nil."
  (when-let ((buffer-name
              (or file
                  (buffer-file-name (buffer-base-buffer))))
             (path (expand-file-name
                    buffer-name))
             (directory (expand-file-name ram-org-roam-weekly-notes-directory org-roam-directory)))
    (setq path (expand-file-name path))
    (save-match-data
      (and (org-roam-file-p path)
           (f-descendant-of-p path directory)))))

(defun ram-org-roam-weeklies-next (&optional n)
  "Goto or create next Nth weekly note."
  (let* ((file-name (file-name-base (buffer-file-name (buffer-base-buffer))))
         (week-from-buffer-name (string-to-number (car (last (split-string file-name "-w")))))
         (year-from-buffer-name (string-to-number (car (split-string file-name "-"))))
         ;; (encode-time '(SECOND MINUTE HOUR DAY MONTH YEAR IGNORED DST ZONE))
         ;; (decode-time) into
         ;; (SEC MINUTE HOUR DAY MONTH YEAR DOW DST UTCOFF)
         (1st-of-jan (encode-time (list 1 1 0 1 1 year-from-buffer-name nil nil t)))
         ;; 1. from file-name, e.g., "2024-01-w04.org"
         ;;    we can only get the week number and the year
         ;; 2. We need time on Monday for this week
         ;;    - we add week-from-buffer-name 7 day periods (weeks)
         ;;      to the 1st day of the year-from-buffer-name
         ;;    - however, 1st day of the year may not start on Monday
         ;;      hence, we need to subtract the number of weekdays that
         ;;      1st of Jan happens to land on.
         (dow-1st-of-jan (let (
                               ;; start week from Mon rather than Sun
                               (wd (nth 6 (decode-time 1st-of-jan))))
                           (if (= wd 0) 6 (1- wd))))
         (time-from-buffer-name (time-add 1st-of-jan
                                          ;; subtract weekdays before 1st of Jan
                                          (- (* 7 week-from-buffer-name (* 24 3600))
                                             (*
                                              ;; if Jan 1st happens to Monday, then
                                              ;; strangely, we need to subtract the whole week
                                              ;; because 7 day periods from 1st of Jan
                                              ;; will carry as to the Monday one week too far
                                              ;; from the week-from-buffer-name
                                              (if (zerop dow-1st-of-jan)
                                                  7
                                                dow-1st-of-jan)
                                              (* 24 3600)))))
         ;; add (or subtract) n weeks
         (target-time (time-add time-from-buffer-name (* 7 (or n 1) 86400))))
    (ram-org-capture-weekly-note nil target-time)))

(defun ram-org-get-daily-headings-from-week (time &optional include-backlink-p same-month-only-p)
  "Return an org-element made `org-roam' daily note headings for a week."
  (let* ((dailies-dir (expand-file-name ram-org-roam-daily-notes-directory org-roam-directory))
         (current-month (nth 4 (decode-time time)))
         (weekday (let ((wd (nth 6 (decode-time time)))) ; start week from Mon rather than Sun
                    (if (= wd 0) 6 (1- wd))))
         (mon (time-subtract time (* weekday 86400)))
         (day-times (cl-labels ((get-week-days (time)
                                  (let ((same-month-p (= (nth 4 (decode-time time)) current-month) ))
                                    (cond
                                     ;; reached Sunday
                                     ((= 0 (nth 6 (decode-time time))) (if (and same-month-only-p
                                                                                (not same-month-p))
                                                                           '()
                                                                         (list time)))
                                     ;; same-month-only-p
                                     ((and same-month-only-p (not same-month-p))
                                      (get-week-days (time-add 86400 time)))
                                     (t (cons time (get-week-days (time-add 86400 time))))))))
                      (get-week-days mon)))
         (daily-notes
          (cl-labels
              ((get-headings (days)
                 (if (null days)
                     '()
                   (cons
                    (let* ((file-name (file-name-with-extension
                                       (format-time-string "%Y-%m-%d" (car days))
                                       "org"))
                           (file-path (file-name-concat dailies-dir file-name)))
                      (if (not (file-exists-p file-path))
                          ;; create daily note, return backlink
                          (let* ((id (org-id-new))
                                 (doc-title (file-name-base file-name))
                                 (doc-header (concat
                                              ":PROPERTIES:\n"
                                              (format ":ID:       %s\n" id)
                                              ":END:\n"
                                              (format "#+TITLE: %s\n" doc-title)
                                              (format "#+DATE: %s\n"
                                                      (format-time-string
                                                       "%Y-%m-%d %a" (car days)))))
                                 (week-day-heading
                                  (format "[[id:%s][%s]]"
                                          id
                                          (upcase (format-time-string "%a %-d" (car days)))))
                                 buffer)
                            (let ((inhibit-message t)
                                  (message-log-max nil))
                              (setq buffer (find-file-noselect file-path))
                              (set-buffer buffer)
                              (insert doc-header)
                              (save-buffer)
                              (kill-buffer))
                            ;; (format "* [[id:%s][%s]]\n"
                            ;;         id
                            ;;         (upcase (format-time-string "%a %-d" (car days))))
                            (list 'headline
                                  `(:raw-value ,week-day-heading
                                               :post-blank 1
                                               :level 1
                                               :title ,(list week-day-heading))))
                        ;; return backlink and headings in daily note
                        (with-temp-buffer
                          (insert-file-contents file-path)
                          (org-mode)
                          (let* ((parsed-buffer (org-element-parse-buffer))
                                 (id (org-element-map
                                         parsed-buffer 'node-property
                                       (lambda (prop)
                                         (when (string= (org-element-property :key prop) "ID")
                                           (org-element-property :value prop)))
                                       'first-match t))
                                 (week-day-heading
                                  (format "[[id:%s][%s]]"
                                          id
                                          (upcase (format-time-string "%a %-d" (car days))))))
                            (cons 'headline
                                  (cons `(:raw-value ,week-day-heading
                                                     :post-blank 1
                                                     :level 1
                                                     :title ,(list week-day-heading))
                                        (org-element-map parsed-buffer 'headline
                                          (lambda (h)
                                            (let ((h (ram-org-parse-heading-element h file-path)))
                                              (org-element-put-property
                                               h :level (1+ (org-element-property :level h))))))))))))
                    (get-headings (cdr days))))))
            (get-headings day-times))))
    daily-notes))

;; FIXME:
;; 1. headlines  for a day should preserve their level
;;    fold headlines beyond top level.
;; 2. rename "create" to "capture" for consistency
;;    (could not find this fn with "capture weekly" search)
;; 3. May be include links to the actual notes.
(defun ram-org-capture-weekly-note (&optional arg time)
  "Create a weekly note from daily notes in an ARG week from now.
Use calendar if ARG value is '(4)."
  (interactive "P")
  (require 'org)
  (let* ((time (or time
                   (if (equal arg '(4))
                       (let ((org-read-date-prefer-future t))
                         (org-read-date nil 'TO-TIME nil "Capture to daily-note: " ))
                     (time-add (* (or arg 0) 7 86400) (current-time)))))
         ;; day of the week, where 0 is monday and 6 is sunday
         (dow (let ((dow (nth 6 (decode-time time))))
                (if (= dow 0) 6 (1- dow))))
         (date-str (let* (
                          ;; get time on Monday
                          (monday (time-subtract time (* dow 86400)))
                          (month-1st-day-mon (encode-time 1 1 0 1 (nth 4 (decode-time monday))
                                                          (nth 5 (decode-time monday))))
                          (date-str-mon (format "%s w%s"
                                                (format-time-string "%b" monday)
                                                (1+ (- (string-to-number (format-time-string "%W" monday))
                                                       (string-to-number (format-time-string "%W" month-1st-day-mon))))))
                          (sunday (time-add time (* (- 6 dow) 86400)))
                          (month-1st-day-sun (encode-time 1 1 0 1 (nth 4 (decode-time sunday))
                                                          (nth 5 (decode-time sunday))))
                          (date-str-sun (format "%s w%s"
                                                (format-time-string "%b" sunday)
                                                (1+ (- (string-to-number (format-time-string "%W" sunday))
                                                       (string-to-number (format-time-string "%W" month-1st-day-sun)))))))
                     (if (string= date-str-mon date-str-sun)
                         date-str-mon
                       (format "%s, %s" date-str-mon date-str-sun))))
         (doc-title (format-time-string "%Y-%m-w%W" time))
         (file-name (file-name-concat (expand-file-name ram-org-roam-weekly-notes-directory
                                                        org-roam-directory)
                                      (file-name-with-extension doc-title "org")))
         (note-exists-p (or (get-buffer (file-name-nondirectory file-name))
                            (file-readable-p file-name)))
         (doc-id (if note-exists-p
                     (ram-org-get-fist-id-property file-name)
                   (org-id-new)))
         (doc-text (concat (concat ":PROPERTIES:\n"
                                   (format ":ID:       %s\n" doc-id)
                                   ":END:\n"
                                   (format "#+TITLE: %s\n" doc-title)
                                   ;; (format "#+CREATED: %s\n"
                                   ;;         (format-time-string (org-time-stamp-format t t) time))
                                   ;; DATE includes the month adn the week number
                                   (format "#+DATE: %s\n\n"  date-str))
                           (org-element-interpret-data (ram-org-get-daily-headings-from-week time)))))
    (find-file file-name)
    (if (not note-exists-p)
        (progn
          (erase-buffer)
          ;; when new daily notes are created (they were automatically
          ;; created because none existed)
          ;; the id link does not work because id locations were not updates.
          ;; call (org-roam-update-org-id-locations)
          ;; TODO: may too much overhead involved?
          ;; update only the relevant directory dailies
          (org-roam-update-org-id-locations))
      (goto-char (point-min))
      ;; (org-next-visible-heading 1)
      (delete-region (point) (point-max)))
    (insert doc-text)
    (save-buffer)
    (re-search-backward (format "^\\*[[:blank:]]+.+%s [[:digit:]]\\{1,2\\}.+$" (format-time-string "%a" time)))))


;;*** org-roam/weeklies: settings

(defvar ram-org-roam-weekly-notes-directory "./dailies/weekly/"
  "A subdirectory of `org-roam-directory' for weekly notes.")


;;** org-roam: hooks, advice, timers

;; !!! 'org-capture-before-finalize-hook is set to nil in org-capture-kill
;; (add-hook 'org-capture-before-finalize-hook #'ram-remove-face-remapping)
(add-hook 'org-capture-prepare-finalize-hook #'ram-remove-face-remapping)

;; !!! why not use hooks specific to org or org-roam buffers
(add-hook 'find-file-hook #'ram-update-org-roam-tag-if-contains-todos)
(add-hook 'before-save-hook #'ram-update-org-roam-tag-if-contains-todos)

;; it fires even when you open a daily note (or yesterday)
;; (add-hook 'org-roam-capture-new-node-hook #'ram-set-face-remapping-alist-capture-new-node)

(add-hook 'org-roam-find-file-hook #'ram-remap-hl-line-face-in-find-file-hook 0 t)

(add-hook 'org-capture-mode-hook #'ram-add-remap-face-to-hl-line-in-capture-hook 0 t)

;; (run-with-idle-timer 1.5 nil #'require 'org-roam)
(eval-after-load 'org
  '(require 'org-roam))

;;* outline:

;; ** outline: headings, headlines


;;** outline: setup

(outline-minor-mode t)

(with-eval-after-load "outline"
  (require 'foldout))

;;** outline: bicycle

(straight-use-package
 '(bicycle :type git :flavor melpa :host github :repo "tarsius/bicycle"))

;;** outline: functions

(defun ram-outline-hide-all ()
  "Hide all `outline-mode' subtrees."
  (interactive)
  (let ((p (point)))
    (outline-map-region 'outline-hide-subtree (point-min) (point-max))
    ;; (outline-map-region (lambda () (outline-hide-sublevels 2)) (point-min) (point-max))
    (goto-char p)
    ;; (move-beginning-of-line 1)
    (recenter)))


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

(defun ram-move-to-heading-visible-char ()
  "Move to first visible char assuming starting at `point-at-bol'."
  (re-search-forward "[^[:space:]]" (point-at-eol) 'NOERROR 1)
  (backward-char))

(defun ram-outline-down-heading ()
  "Move to the next `outline-mode' subtree."
  (interactive)
  (condition-case err
      (progn
        (deactivate-mark)
        (ram-push-mark)
        (outline-up-heading 1 t)
        (outline-forward-same-level 1))
    (error (cond ((string= (error-message-string err)
                           "No following same-level heading")
                  (ram-outline-down-heading))
                 ((string= (error-message-string err)
                           "Already at top level of the outline")
                  (outline-forward-same-level 1))
                 (signal (car err) (cdr err)))))
  (ram-move-to-heading-visible-char))

(defun ram-outline-up-heading (arg &optional invisible-ok)
  "Call `outline-up-heading' and ignore the error.

Ignore \"Already at top level of the outline\" error, call
`outline-backward-same-level' instead."
  (interactive "p")
  (condition-case err
      (progn (deactivate-mark)
             (ram-push-mark)
             (outline-up-heading 1 t))
    (error (if (string= (error-message-string err)
                        "Already at top level of the outline")
               (outline-backward-same-level arg)
             (signal (car err) (cdr err)))))
  (ram-move-to-heading-visible-char))

(defun ram-outline-next-same-level (arg &optional invisible-ok)
  "Call `outline-forward-same-level' and ignore the error.

Ignore \"No following same-level heading\" error, call
`ram-outline-down-heading' instead."
  (interactive "p")
  (condition-case err
      (outline-forward-same-level arg)
    (error (if (string= (error-message-string err)
                        "No following same-level heading")
               (if (buffer-narrowed-p)
                   (progn
                     (widen)
                     (ram-outline-next-same-level arg invisible-ok))
                 (ram-outline-down-heading))
             (ram-outline-down-heading)
             (signal (car err) (cdr err)))))
  (ram-move-to-heading-visible-char))

(defun ram-outline-next-visible-heading (arg)
  "Move point after jumping to a heading."
  (interactive "p")
  (call-interactively #'outline-next-visible-heading 'RECORD-FLAG)
  (ram-move-to-heading-visible-char))

(defun ram-outline-previous-visible-heading (arg)
  "Move point after jumping to a heading."
  (interactive "p")
  (call-interactively #'outline-previous-visible-heading 'RECORD-FLAG)
  (ram-move-to-heading-visible-char))

(defun ram-outline-previous-same-level (arg)
  "Move point after jumping to a heading."
  (interactive "p")
  (call-interactively #'outline-backward-same-level 'RECORD-FLAG)
  (ram-move-to-heading-visible-char))

;;** outline/functions: toggle

(defvar-local ram-outline-toggle-plist nil
  "A data store for `ram-outline-toggle'.

Properties:
  - :counter \"number of time the function was called\"
  - :orig-point \"point at the first function call\"
  - :beg \"beginning of the region for the function\"
  - :end \"end of the region for the function\")"
  )

(defun ram-outline-toggle (&optional beg end)
  "Toggle `outline-mode' outlines delimited by BEG and END.

Preserve the point position."
  (interactive)
  (let* (;; either get BEG, or :beg or (point-min)
         (beg (or (plist-get (plist-put ram-outline-toggle-plist :beg beg) :beg)
                  (plist-get ram-outline-toggle-plist :beg)
                  (point-min)))
         ;; either get END, or :end or (point-max)
         (end (or (plist-get (plist-put ram-outline-toggle-plist :end end) :end)
                  (plist-get ram-outline-toggle-plist :end)
                  (point-max)))
         (p (or (plist-get ram-outline-toggle-plist :orig-point)
                (point)))
         (c (or (plist-get ram-outline-toggle-plist :counter)
                0))
         )
    (cond
     ;; 1st new iteration
     ;;   - 1st call to the function ram-outline-toggle
     ;; hide all
     ((not (eq last-command this-command))
      ;; reset :orig-point unless the last command was also
      ;; toggling outlines
      ;; let other "ram-outline-toggle-*" commands reuse the same
      ;; :orig-point
      ;; reset it for other last-command
      (when (not (and (symbolp last-command)
                      (string-prefix-p "ram-outline-toggle" (symbol-name last-command))))
        (setq p (point)))
      (outline-map-region (lambda () (when
                                         (= 1 (funcall outline-level))
                                       (outline-hide-subtree)))
                          beg end)
      (move-beginning-of-line 1)
      (setq ram-outline-toggle-plist (plist-put
                                      (plist-put ram-outline-toggle-plist :counter 2)
                                      :orig-point p)))
     ;; 1st iteration again:
     ;;   - repeated cycle of iterations
     ;; hide all
     ((and (eq last-command this-command)
           (or (= 1 c)
               (< 3 c)))
      (outline-map-region (lambda () (when
                                         (= 1 (funcall outline-level))
                                       (outline-hide-subtree)))
                          beg end)
      (move-beginning-of-line 1)
      (setq ram-outline-toggle-plist (plist-put ram-outline-toggle-plist :counter 2))
      )
     ;; last iteration
     ((and (eq last-command this-command)
           (= 4 c))
      (outline-map-region (lambda () (when
                                         (= 1 (funcall outline-level))
                                       (outline-hide-subtree)))
                          beg end)
      (goto-char p)
      (setq ram-outline-toggle-plist (plist-put ram-outline-toggle-plist :counter (1+ c))))
     ;; 2nd iteration:
     ;;   - show sublevels 2
     ((and (eq last-command this-command)
           (= 2 c))
      (outline-show-all)
      (outline-map-region (lambda ()
                            (cond
                             ((= 1 (funcall outline-level)) (outline-hide-subtree))
                             ((= 2 (funcall outline-level)) (outline-show-heading))
                             (t nil)))
                          beg end)
      (goto-char p)
      (move-beginning-of-line 1)
      (setq ram-outline-toggle-plist (plist-put ram-outline-toggle-plist :counter (1+ c))))
     ;; 3nd iteration:
     ;;   - outline-show-all
     ((and (eq last-command this-command)
           (= 3 c))
      (outline-show-all)
      (setq ram-outline-toggle-plist (plist-put ram-outline-toggle-plist :counter (1+ c)))
      (goto-char p))
     ;; should not happend, but just in case
     (t (setq ram-outline-toggle-plist nil)))
    (recenter))
  )

(defvar-local ram-outline-toggle-current-subtree-plist nil
  "A data store for `ram-outline-toggle-currect-subtree'.

Although you could reuse `ram-outline-toggle-plist' for the same
purpose, We define a separate data store to reduce the interdependence
between the functions. This could be reconsidered if the addition of
extra variable does not reduce the complexity.

Properties:
  - :beg \"beginning of the region for the function\"
  - :end \"end of the region for the function\")"
  )

(defun ram-outline-toggle-current-subtree ()
  "Toggle `outline-mode' outlines for the current subtree.

Preserve the point position.
The \"ram-outline-toggle-*\" name part is recognized by `ram-outline-toggle'."
  (interactive)
  ;; for the first call to the command
  ;; reset data in ram-outline-toggle-current-subtree-plist
  (when (not (eq last-command this-command))
    (setq-local ram-outline-toggle-current-subtree-plist nil))
  (let* ((beg (or (plist-get ram-outline-toggle-current-subtree-plist :beg)
                  (plist-get (setq ram-outline-toggle-current-subtree-plist
                                   (plist-put ram-outline-toggle-current-subtree-plist :beg
                                              (save-excursion
                                                (outline-back-to-heading t)
                                                (if (not (= 1 (outline-level)))
                                                    (progn (condition-case err
                                                               (outline-up-heading (outline-level) t)
                                                             (error (if (string= (error-message-string err)
                                                                                 "Already at top level of the outline")
                                                                        (point)
                                                                      (signal (car err) (cdr err))))
                                                             (:success (point))))
                                                  (point))))) :beg)))
         (end (or (plist-get ram-outline-toggle-current-subtree-plist :end)
                  (plist-get (setq ram-outline-toggle-current-subtree-plist
                                   (plist-put ram-outline-toggle-current-subtree-plist :end
                                              (save-excursion (goto-char beg)
                                                              (condition-case err
                                                                  (outline-forward-same-level 1)
                                                                (error (if (string= (error-message-string err)
                                                                                    "No following same-level heading")
                                                                           (point-max)
                                                                         (signal (car err) (cdr err))))
                                                                (quit (setq ram-outline-toggle-current-subtree-plist nil))
                                                                (:success (point))))))
                             :end))))
    (funcall-interactively #'ram-outline-toggle beg end)))

;;** outline: bindings

(with-eval-after-load "outline"
  (define-key outline-minor-mode-map (kbd "<tab>") #'bicycle-cycle))

(define-key ram-leader-map-tap-global (kbd "n") #'ram-outline-next-visible-heading)
(define-key global-map (kbd "<f10>") #'ram-outline-next-visible-heading)

(define-key ram-leader-map-tap-global (kbd "p") #'ram-outline-previous-visible-heading)
(define-key global-map (kbd "<f6>") #'ram-outline-previous-visible-heading)

(define-key ram-leader-map-tap-global (kbd "f") #'ram-outline-next-same-level)
(define-key global-map (kbd "<f20>") #'ram-outline-next-same-level)

;; bad idea: accidentally pressing the key can mess up a lot
;; (define-key global-map (kbd "<f19>") #'org-move-subtree-up)

(define-key ram-leader-map-tap-global (kbd "b") #'ram-outline-previous-same-level)
(define-key global-map (kbd "<f8>") #'ram-outline-previous-same-level)
;; bad idea: accidentally pressing the key can mess up a lot
;; (define-key global-map (kbd "<f16>") #'org-move-subtree-down)

(define-key ram-leader-map-tap-global (kbd "o") #'outline-show-all)
(define-key ram-leader-map-tap-global (kbd "q") #'ram-outline-hide-all)

(define-key ram-leader-map-tap-global (kbd "u") #'ram-outline-up-heading)
(define-key global-map (kbd "<f23>") #'ram-outline-up-heading)

(define-key ram-leader-map-tap-global (kbd "d") #'ram-outline-down-heading)
(define-key global-map (kbd "<f7>") #'ram-outline-down-heading)

(define-key ram-leader-map-tap-global (kbd "z") #'ram-toggle-narrow-outline-heading)
(define-key global-map (kbd "<f22>") #'ram-toggle-narrow-outline-heading)

(define-key global-map (kbd "<f15>") #'ram-outline-toggle)

;;** outline/bindings: toggle

(define-key global-map (kbd "S-<f15>") #'ram-outline-toggle)
(define-key global-map (kbd "<f15>") #'ram-outline-toggle-current-subtree)

;; ** outline: outline-regexp

;; sadly, some headings may not start an the beginning of the line
;; (e.g. ram-abbrev.el). Try to keep heading starting on the point in
;; line.
;; (defvar ram-outline-regxp-for-lisp "[[:space:]]*;;[[:space:]]?\\(?:;+[^#]\\|\\*+\\)"
;;   "`outline-regexp' for languages with comments defined by \";\".")

;; drop ";;;;" style, only ";;*" or ";; *"
(defvar ram-outline-regxp-for-lisp "[[:space:]]*;;[[:space:]]?\\(\\*+\\)"
  "`outline-regexp' for languages with comments defined by \";\".")

;; ** outline: outline-level

(defun ram-outline-get-level-for-lisps ()
  "Custom `outline-mode' outline level.

Use regexp match group 1. Default to group 0."
  (if (match-string 1)
      (- (match-end 1) (match-beginning 1))
    (- (match-end 0) (match-beginning 0))))

(defun ram-outline-setup-for-lisps ()
            (setq-local outline-regexp ram-outline-regxp-for-lisp)
            (setq-local outline-level #'ram-outline-get-level-for-lisps)
            (outline-minor-mode 1))

(add-hook 'emacs-lisp-mode-hook #'ram-outline-setup-for-lisps)
(add-hook 'clojure-mode-hook #'ram-outline-setup-for-lisps)

;;* hideshow

(add-hook 'prog-mode-hook #'hs-minor-mode)

(with-eval-after-load "hideshow"
  (define-key hs-minor-mode-map (kbd "<C-tab>") #'hs-toggle-hiding))

;;* git-gutter-fringe

(straight-use-package
 '(git-gutter-fringe :type git :flavor melpa :host github :repo "syohex/emacs-git-gutter-fringe"))

;; update gutter when switching exwm workspaces
(with-eval-after-load 'git-gutter
  (add-to-list 'exwm-workspace-switch-hook
               #'git-gutter:update-all-windows))

(with-eval-after-load 'magit
  (add-to-list 'magit-post-stage-hook
               #'git-gutter:update-all-windows))

(with-eval-after-load 'magit
  (add-to-list 'magit-post-unstage-hook
               #'git-gutter:update-all-windows))

(setq git-gutter:update-interval 0.3)
;; don't ask y/n before staging/reverting
(setq git-gutter:ask-p nil)
;; don't log/message
(setq git-gutter:verbosity 0)
;; update gutter after these commands
(setq git-gutter:update-commands
  '(ido-switch-buffer helm-buffers-list))
;; update gutter info in other windows after these commands
(setq git-gutter:update-windows-commands
      '(kill-buffer ido-kill-buffer ram-other-workspace
                    other-window
                    magit-stage))
(global-git-gutter-mode -1)

(add-to-list 'window-buffer-change-functions
             (lambda (frame)
               "Call `git-gutter' when buffer changes."
               (let ((buffer
                      (window-buffer (frame-selected-window frame))))
                 (with-current-buffer buffer
                   ;; check if the buffer file is under version control
                   (if-let* ((file-name (buffer-file-name buffer))
                             (vc-type (vc-backend (file-truename file-name))))
                       (if (local-variable-p 'git-gutter-mode)
                           (git-gutter)
                         (setq-local git-gutter-mode t)
                         (git-gutter-mode 1)))))))

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

(defun ram-get-git-gutter-diff-window ()
  "Return window displaying '*git-gutter:diff*' buffer, if any."
  (car (seq-filter
        (lambda (w) (equal "*git-gutter:diff*" (buffer-name (window-buffer w))))
        (window-list-1 nil 'nomini))))

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
           (let ((diff-win (ram-get-git-gutter-diff-window)))
             (when diff-win
               (quit-window nil diff-win)))
           (magit-status)))
   "commit" :exit t)
  ("N" git-gutter:end-of-hunk "end")
  ("m" git-gutter:mark-hunk "mark")
  ("d" (lambda ()
         (interactive)
         (if-let ((diff-win (ram-get-git-gutter-diff-window)))
             (quit-window nil diff-win)
           (git-gutter:popup-hunk)))
   "diff")
  ("r" git-gutter:revert-hunk "revert")
  ("s" git-gutter:stage-hunk "stage")
  ("SPC" nil "quit" :exit t)
  ("q" nil "quit" :exit t))

(defun ram-start-git-gutter-hydra ()
  "Enable git-gutter-mode and run `hydra-git-gutter/body."
  (interactive)
  ;; (git-gutter-mode 1)
  ;; (git-gutter:live-update)
  (hydra-git-gutter/body))

(define-key ram-leader-map-tap-global (kbd "h") #'ram-start-git-gutter-hydra)
;; (define-key global-map (kbd "<f5>") 'hydra-git-gutter/body)
(define-key global-map (kbd "<f5>") #'ram-start-git-gutter-hydra)

(define-key global-map (kbd "<f5>") #'hydra-git-gutter/body)

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


;;* racket

(straight-use-package
 '(racket-mode :type git :flavor melpa :files
               (:defaults "*.rkt"
                          ("racket" "racket/*")
                          (:exclude "racket/example/*" "racket/test/*")
                          "racket-mode-pkg.el")
               :host github :repo "greghendershott/racket-mode"))

;; an error in :around advice: ‘racket--indent-sexp-advice’.
;; for indent-sexp
;; this messes up my Org code block editing with syntax-table changed
;; (require 'racket-mode)

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)
  (define-key racket-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  (define-key racket-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun))

;; breaks lispy-mode key bindings
;; (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  (define-key racket-mode-map (kbd "<return>") #'ram-newline-and-indent)
  (define-key racket-mode-map (kbd "<S-return>") 'newline-and-indent))

;;** racket: repl

(with-eval-after-load "racket-repl"
  (define-key racket-repl-mode-map (kbd "<f2>") #'racket-repl-submit))

;;** racket: hooks, advice, timers

(with-eval-after-load 'racket-mode
  ;; an error in :around advice: ‘racket--indent-sexp-advice’.
  ;; for indent-sexp
  ;; this messes up my Org code block editing with syntax-table changed
  ;; (require 'racket-mode)
(advice-remove 'lisp-indent-line #'racket--lisp-indent-line-advice)
(advice-remove 'indent-sexp #'racket--indent-sexp-advice))

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
(setq avy-keys '(?a ?s ?r ?e ?t ?i ?u ?n ?o ?m ?p ?l ?f ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d))

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
    ;; when in org-mode, if 1st thing in a heading is a link and
    ;; org-toggle-link-display is on, the following regex does not work
    ;; (avy-jump "^[[:blank:]]*\\*+ [^[:space:]]" :window-flip nil :beg nil :end nil)
    (avy-jump "^[[:blank:]]*\\*+[[:space:]]+" :window-flip nil :beg nil :end nil))
  ;; (re-search-forward "[^*[:space:]]" (window-end) t 1)
  ;; (forward-char -1)
  )

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

(defun ram-get-open-parens ()
  "Get all open parens in defun, drop adjacent."
  (let* ((ppss (syntax-ppss))
         (beg (if (= 0 (car ppss))
                  (if (memq (char-before) ram-close-delimiters)
                      (save-excursion (backward-list))
                    (point))
                (car (nth 9 ppss))))
         (end (min (window-end)
                   (save-excursion
                     (goto-char beg)
                     (forward-list))))
         (window-start (window-start))
         (window (get-buffer-window)))
    (cl-labels ((get-open-parens ()
                  (let ((delimiter (ram-forward-to-delim)))
                    (if (and delimiter
                             (not (> delimiter end)))
                        (cons delimiter (get-open-parens))
                      '())))
                (remove-adjacent (delims)
                  (let ((1st (car delims))
                        (2nd (cadr delims))
                        (rest (cddr delims)))
                    (cond
                     ((not 1st) '())
                     ((not 2nd) (list 1st))
                     (t (if (= 1st (1- 2nd))
                            (cons 1st (remove-adjacent rest))
                          (cons 1st (cons 2nd (remove-adjacent rest)))))))))
      (mapcar (lambda (p) (cons (cons (1- p) p) window))
              (remove-adjacent (seq-filter (lambda (p) (> p window-start))
                                           (save-excursion (goto-char beg) (get-open-parens))))))))

(defun ram-avy-goto-ace-paren ()
  "Call `lispy-ace-paren'."
  (interactive)
  (require 'lispy)
  (let* ((avy-command 'ram-avy-goto-ace-paren)
         (avy-style 'at-full)
         (avy-orders-alist (list (cons avy-command 'ram-avy-order-from-beg-of-defun)))
         (cands (ram-get-open-parens)))
    (setq avy-action nil)
    (avy-process cands)))

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

;;** multiple-cursors: mc/list-file

;; mc/list-file is a variable defined in ‘multiple-cursors-core.el’.
;; Its value is "~/.local/share/emacs/my.emacs.d/.mc-lists.el"

;;** multiple-cursors: mc/cmds-to-run-once

;; commands to run only for one cursor
;; mc/cmds-to-run-once


;;** multiple-cursors: mc/cmds-to-run-once

;; mc/cmds-to-run-for-all
;; commands to run for all cursors

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

 ;;* multiple-cursors: one line, arbitrary places

;; 1. place cursor where to insert
;;    press "M-<f9>" to place the 1st cursor
;;     1.1 Move cursor, press "M-<f9>" again  place 2nd cursor
;;     1.2 REPEAT for new locations.
(define-key global-map (kbd "M-<f9>") #'mc/toggle-cursor-at-point)
;; 2. with the multiple cursors selected, press "M-S-<f9> and
;;     start typing what you need
(define-key global-map (kbd "M-S-<f9>") 'multiple-cursors-mode)
;; 3. when finished typing, press "M-S-<f9> to exit multiple cursors.

;;* ram-manage-sexps-mode

;;** ram-manage-sexps-mode: multi-line, one-line, flatten

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

;;** ram-manage-sexps-mode: navigation

(defun ram-push-mark ()
  "Push mark only if there is no mark for the point."
  (if (mark)
      (when (not (= (mark) (point)))
             (push-mark))
    (push-mark)))

(defun ram-forward-list (&optional arg)
  "Move to the next thing.
When at the list limit, move a level up and continue.
If ARG is `nil', do not `push-mark'."
  (interactive "p")
  (deactivate-mark)
  (when arg (ram-push-mark))
  (cond
   ;; move out of empty space
   ((save-excursion (and (or (= 1 (line-number-at-pos))
                             (looking-back "[[:space:]\n]+"
                                           (save-excursion (previous-logical-line)
                                                           (point-at-eol))))
                         (looking-at-p "[[:space:]\n]+")))
    (re-search-forward "[^[:space:]\n]\\{1\\}")
    (if (save-excursion (backward-char)
                        (ram-at-delimited-beg-p))
        (progn (backward-char)
               (ram-sexp-bounds))
      (ram-forward-list)))
   (t (let* ((bounds (ram-sexp-bounds))
             (next-bounds (save-excursion (goto-char (cdr bounds))
                                          (ram-next-delimited-sexp-bounds)))
             (at-end-p (= (point) (cdr bounds))))
        (when next-bounds
          (if at-end-p
              (progn
                (goto-char (cdr next-bounds))
                next-bounds)
            (goto-char (car next-bounds))
            (if (> (car bounds) (car next-bounds))
                (ram-forward-list)
              next-bounds)))))))

(defun ram-backward-list (&optional arg)
  "Move to the previous thing.
When at the list limit, move a level up and continue.
If ARG is `nil', do not `push-mark'."
  (interactive "p")
  (deactivate-mark)
  (when arg (ram-push-mark))
  (cond
   ;; move out of empty space
   ((and (and (or (= 1 (line-number-at-pos))
                             (looking-back "[[:space:]\n]+"
                                           (save-excursion (previous-logical-line)
                                                           (point-at-eol))))
                         (looking-at-p "[[:space:]\n]+"))
         (looking-at-p "[[:space:]\n]+"))
    (when (re-search-backward "[^[:space:]\n]\\{1\\}")
      (if-let ((bounds (save-excursion (forward-char) (ram-thing-bounds))))
          (progn (goto-char  (car bounds))
                 (when (not (ram-at-delimited-beg-p))
                   (ram-backward-list))))))
   (t (let* ((point (point))
             (bounds (ram-sexp-bounds))
             (prev-bounds (save-excursion (goto-char (car bounds))
                                          (ram-prev-delimited-sexp-bounds)))
             (at-end-p (ram-at-thing-end-p)))
        (if at-end-p
            (progn
              (goto-char (cdr prev-bounds))
              (if (< (cdr bounds) (cdr prev-bounds))
                  (ram-backward-list)
                prev-bounds))
          (and prev-bounds
               (goto-char (car prev-bounds)))
          prev-bounds)))))

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

(defun ram-forward-to-delim ()
  "Jump forward to the open delimiter that is not in a string or comment."
  (if-let ((match-point (re-search-forward ram-open-delimiters-re (point-max) t 1)))
      ;; skip matches in strings and comments
      (let ((s (syntax-ppss)))
        (if (or (nth 3 s)
                (nth 4 s))
            (ram-forward-to-delim)
          match-point))))

(defun ram-jump-forward-to-open-delimiter ()
  "Jump forward to the open delimiter that is not in a string."
  (interactive)
  (forward-char 1)
  (ram-forward-to-delim)
  (forward-char -1))

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
      (forward-sexp (- arg))
      (point))))

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
  ;; (when (buffer-narrowed-p)
  ;;   (widen))
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
  "Jump to the previous top level thing."
  (interactive "p")
  (when (buffer-narrowed-p)
    (widen))
  (let ((point (point)))
    (when (= point (ram-beg-of-top-sexp 1))
      (ram-backward-list)))
  ;; (let ((at-end-p (ram-at-thing-end-p))
  ;;       prev-bounds)
  ;;   (if (= arg 4)
  ;;       (ram-beg-of-top-sexp 1)
  ;;     (ram-beg-of-top-sexp 1)
  ;;     (setq prev-bounds (ram-backward-list))
  ;;     (when (< arg 0)                   ; reverse at-end-p value
  ;;       (setq at-end-p (not at-end-p)))
  ;;     (if at-end-p
  ;;         (and prev-bounds
  ;;              (goto-char (cdr prev-bounds)))
  ;;       (and prev-bounds
  ;;            (goto-char (car prev-bounds)))) ))
  )

;;** ram-manage-sexps-mode: settings

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

;;** ram-manage-sexps-mode: ram-highlight-sexps

(autoload 'ram-highlight-sexps-mode "ram-highlight-sexps")

(setq hl-sexp-highlight-adjacent t)
(add-hook 'emacs-lisp-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'lisp-interactive-mode #'ram-highlight-sexps-mode)
(add-hook 'clojure-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'cider-repl-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'racket-mode-hook #'ram-highlight-sexps-mode)
(add-hook 'racket-repl-mode-hook #'ram-highlight-sexps-mode)

(with-eval-after-load "ram-highlight-sexps"
  (add-hook 'after-load-theme-hook #'hl-sexp-color-update))

;;** ram-manage-sexps-mode: highlight

(defvar ram-copied-region-overlay nil
  "Highlight the region copied with `ram-copy-sexp'.")

(setq ram-copied-region-overlay (make-overlay 0 0))
;; (overlay-put ram-copied-region-overlay 'face (list :background
;;                                                   (face-background 'modus-themes-intense-green)))
(overlay-put ram-copied-region-overlay 'face (list :background "yellow"))
(overlay-put ram-copied-region-overlay 'priority 1)

(add-hook 'pre-command-hook (lambda () (when (symbol-value 'ram-copied-region-overlay)
                                           (move-overlay ram-copied-region-overlay 1 1))))

;;** ram-manage-sexps-mode: comments

(defun ram-inline-comment-bounds ()
  "Return a pair of the inline comment beginning and end. "
  (if-let (beg (and (ram-in-comment-p)
                    (save-excursion
                      (ram-goto-beg-of-comment))))
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
            (when (and (ram-in-comment-p)
                       (not (minibufferp)))
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

;;** ram-manage-sexps-mode: at beg? at end?

(defun ram-at-delimited-beg-p ()
  "Return non `nil' if the point is before `ram-open-delimiters-re'."
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

;;** ram-manage-sexps-mode: kill-whole-line

(defun ram-kill-whole-line (&optional arg)
  "Invoke `kill-whole-line' followed by `back-to-indentation'."
  (interactive "p")
  (kill-whole-line arg)
  (back-to-indentation)
  (indent-according-to-mode))

;;** ram-manage-sexps-mode: <enter>, newline-and-indent

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
  (let ((bounds (ignore-errors (ram-thing-bounds))))
    ;; !!! newline-and-indent is called after 'cond actions
    (cond
     ;; if in minibuffer, exit
     ((> (minibuffer-depth) 0)
      (exit-minibuffer))
     ;; if in comment, do nothing
     ((ram-in-comment-p))
     ;; when in string, go to its end
     ((ram-in-string-p) (goto-char (cdr bounds)))
     ;; at delimited sexp beginning, go to its end
     ((ram-at-delimited-beg-p) (goto-char (cdr bounds)))
     ;; inside a list that spans a single line: go to before the closing delimiter
     ((let ((bounds (ram-delimited-sexp-bounds))
            (line-num (line-number-at-pos)))
        (when (and bounds
                   (= line-num (line-number-at-pos (car bounds)))
                   (= line-num (line-number-at-pos (cdr bounds)))
                   ;; not before open delim or after close delim
                   (not (or (ram-at-delimited-beg-p)
                            (ram-at-delimited-end-p))))
          (goto-char (cdr bounds))
          (backward-char))))
     ;; empty list: (|) -> ()|
     ((and (memq (char-after (point)) ram-close-delimiters)
           (memq (char-before (point)) ram-open-delimiters))
      (goto-char (cdr bounds)))
     ;; blank line with `ram-close-delimiters' at the end
     ((and (looking-back "^ +" (point-at-bol))
           (looking-at (concat "[[:space:]]*" ram-close-delimiters-re)))
      (ram-remove-whitespace-between-regexps "[^[:space:]\n]" ram-close-delimiters-re)
      (forward-char))
     ;; blank line
     ((and (looking-back "^ +" (point-at-bol))
           (looking-at "[[:space:]]*$"))
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
                        ( or (cdr (ram-delimited-sexp-bounds)) (point-max)))))
          (while (< (point) (1- end))
            (forward-sexp)))))
    (newline-and-indent)
    (indent-according-to-mode)))

(define-key emacs-lisp-mode-map (kbd "<return>") #'ram-newline-and-indent)
(define-key emacs-lisp-mode-map (kbd "S-<return>") #'newline-and-indent)

(define-key lisp-interaction-mode-map (kbd "<return>") #'ram-newline-and-indent)
(define-key lisp-interaction-mode-map (kbd "S-<return>") #'newline-and-indent)

;;** ram-manage-sexps-mode: bounds

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
                 (t (if (= (nth 0 ppss) 0) ; not in a list
                        (save-excursion (beginning-of-defun) (point))
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

(defun ram-prev-delimited-sexp-bounds ()
  "Return the bounds of the previous delimited sexp.
The search must start outside the current thing bounds."
  (when (re-search-backward (concat "[^"
                                    (string-join (mapcar #'char-to-string ram-open-delimiters))
                                    "#'[:space:]" "\n]") nil t 1)
    (forward-char)
    (cond
     ((ram-in-string-p)
      (goto-char (car (ram-string-bounds)))
      (ram-prev-delimited-sexp-bounds))
     ((ram-inline-comment-bounds)
      (goto-char (car (ram-inline-comment-bounds)))
      (ram-prev-delimited-sexp-bounds))
     ((ram-goto-comment-block-beg)
      (ram-prev-delimited-sexp-bounds))
     ((thing-at-point 'symbol)
      (progn (forward-symbol -1)
             (ram-prev-delimited-sexp-bounds)))
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

(defun ram-next-delimited-sexp-bounds ()
  "Return the bounds of the delimited sexp.
The search must start outside the current thing bounds."
  (when (re-search-forward (concat "[^"
                                   (string-join (mapcar #'char-to-string ram-close-delimiters))
                                   "@#`',[:space:]" "\n]") nil t 1)
    (backward-char)
    (cond
     ((memq (char-after) ram-open-delimiters)  ; edge case when comments follow a delimiter
      (ram-thing-bounds))
     ((thing-at-point 'symbol)
      (forward-symbol 1)
      (ram-next-delimited-sexp-bounds))
     ((ram-in-string-p)
      (goto-char (cdr (ram-string-bounds)))
      (ram-next-delimited-sexp-bounds))
     ((ram-inline-comment-bounds)
      (end-of-line)
      (ram-next-delimited-sexp-bounds))
     ((ram-goto-comment-block-end)
      (ram-next-delimited-sexp-bounds))
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

;;** ram-manage-sexps-mode: select, copy, clone, kill

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
      (move-overlay ram-copied-region-overlay (car bounds) (cdr bounds) (current-buffer))
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
                      (car new-bounds) (cdr new-bounds) (current-buffer))
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
                      (cdr new-bounds) (current-buffer))
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

;;** ram-manage-sexps-mode: delete

(defun ram-delete-char ()
  "Delete all whitespace from `point' to next non-whitespace char.

If looking at whitespace, delete it until next non-whitespace char.
Otherwise, call `paredit-delete-char'."
  (interactive)
  (if (and (let ((syntax (syntax-ppss)))
             ;; not in string or comment
             (and (not (nth 3 syntax))
                  (not (nth 4 syntax))))
           (or (eq (char-after) (string-to-char " "))
               (eq (char-after) (string-to-char "\n"))))
      (progn
        (while (or (eq (char-after) (string-to-char " "))
                   (eq (char-after) (string-to-char "\n")))
          (delete-char 1))
        (when (not (char-before (string-to-char " ")))
          (insert " "))
        (indent-according-to-mode))
    (paredit-delete-char)))

(defun ram-delete-whitespace-backward ()
  "Delete whitespace characters backward.

Either stop an correct indentation, or
at the next sexp."
  (interactive)
  (let ((p (point)))
    (indent-according-to-mode)
    (when (eq p (point))
      (while (or (eq (char-before) (string-to-char " "))
                 (eq (char-before) (string-to-char "\n"))
                 (eq (char-before) (string-to-char "\t")))
        (delete-char -1))
      (if (eq (char-after) (string-to-char " "))
          (forward-char)
        (unless (eq (char-after) (string-to-char "\n"))
          (insert " ")))))
  )

;;** ram-manage-sexps-mode: move up, down

;;** ram-manage-sexps-mode: bindings

;; ------
(define-key prog-mode-map (kbd "<end>" ) #'ram-mark-sexp)
(define-key prog-mode-map (kbd "<home>" ) #'ram-copy-sexp)

;; "M-<end>" default global-map binding end-of-buffer-other-window
;; "M-<home>" default global-map binding beginning-of-buffer-other-window
(define-key global-map (kbd "M-<end>" ) #'ram-mark-sexp)
(define-key global-map (kbd "M-<home>" ) #'ram-copy-sexp)

;; (define-key prog-mode-map (kbd "<end>" ) #'ram-mark-sexp)
;; (define-key prog-mode-map (kbd "<home>" ) #'ram-copy-sexp)

;; (define-key minibuffer-mode-map (kbd "<end>" ) #'ram-mark-sexp)
;; (define-key minibuffer-mode-map (kbd "<home>" ) #'ram-copy-sexp)

;; ------
(define-key prog-mode-map (kbd "<down>") #'ram-forward-list)
(define-key prog-mode-map (kbd "<up>") #'ram-backward-list)

;; (define-key prog-mode-map (kbd "<down>") #'ram-forward-list)
;; (define-key prog-mode-map (kbd "<up>") #'ram-backward-list)

;; (define-key minibuffer-mode-map (kbd "<down>") #'ram-forward-list)
;; (define-key minibuffer-mode-map (kbd "<up>") #'ram-backward-list)

;; ------
(define-key prog-mode-map (kbd "M-<down>" ) #'ram-move-sexp-down)
(define-key prog-mode-map (kbd "M-<up>" ) #'ram-move-sexp-up)

;; ------
(define-key prog-mode-map (kbd "S-<down>") #'ram-next-defun)
(define-key prog-mode-map (kbd "S-<up>") #'ram-prev-defun)

;; (define-key prog-mode-map (kbd "S-<down>") #'ram-next-defun)
;; (define-key prog-mode-map (kbd "S-<up>") #'ram-prev-defun)

;; ------
(define-key prog-mode-map (kbd "C-<down>" ) #'ram-clone-sexp-forward)
(define-key prog-mode-map (kbd "C-<up>" ) #'ram-clone-sexp-backward)

;; ------
(define-key prog-mode-map (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
(define-key prog-mode-map (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

;; (define-key prog-mode-map (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
;; (define-key prog-mode-map (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

;; (define-key minibuffer-mode-map (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
;; (define-key minibuffer-mode-map (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

;; ------
(define-key prog-mode-map (kbd "M-<left>") #'ram-jump-backward-to-close-delimiter)
(define-key prog-mode-map (kbd "M-<right>") #'ram-jump-forward-to-close-delimiter)

;; (define-key prog-mode-map (kbd "M-<left>") #'ram-jump-backward-to-close-delimiter)
;; (define-key prog-mode-map (kbd "M-<right>") #'ram-jump-forward-to-close-delimiter)

;; ------
(define-key prog-mode-map (kbd "S-<left>" ) #'ram-beg-of-top-sexp)
(define-key prog-mode-map (kbd "S-<right>" ) #'ram-end-of-top-sexp)

;; (define-key prog-mode-map (kbd "S-<left>" ) #'ram-beg-of-top-sexp)
;; (define-key prog-mode-map (kbd "S-<right>" ) #'ram-end-of-top-sexp)

;; ------
(define-key prog-mode-map (kbd "C-<left>" ) #'ram-up-list-backward)
(define-key prog-mode-map (kbd "C-<right>" ) #'ram-up-list-forward)

;; (define-key prog-mode-map (kbd "C-<left>" ) #'ram-up-list-backward)
;; (define-key prog-mode-map (kbd "C-<right>" ) #'ram-up-list-forward)

;; ------
;; (define-key prog-mode-map (kbd "C-," ) #'ram-kill-at-point)
(define-key prog-mode-map (kbd "C-," ) #'ram-kill-at-point)

;; flatten sexp
(define-key prog-mode-map (kbd "C-:" ) #'ram-toggle-multiline-delimited-sexp)

(with-eval-after-load 'clojure
  ;; originally, "C-:" is bound to #'clojure-toggle-keyword-string
  (define-key clojure-mode-map (kbd "C-:" ) #'ram-toggle-multiline-delimited-sexp))

;;** ram-manage-sexps-mode: keymap

(defvar ram-manage-sexps-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "<end>" ) #'ram-mark-sexp)

    (define-key keymap (kbd "<home>" ) #'ram-copy-sexp)

    (define-key keymap (kbd "<down>") #'ram-forward-list)
    (define-key keymap (kbd "<up>") #'ram-backward-list)

    (define-key keymap (kbd "M-<down>" ) #'ram-move-sexp-down)
    (define-key keymap (kbd "M-<up>" ) #'ram-move-sexp-up)

    (define-key keymap (kbd "S-<down>") #'ram-next-defun)
    (define-key keymap (kbd "S-<up>") #'ram-prev-defun)

    (define-key keymap (kbd "C-<down>" ) #'ram-clone-sexp-forward)
    (define-key keymap (kbd "C-<up>" ) #'ram-clone-sexp-backward)

    (define-key keymap (kbd "<left>") #'ram-jump-backward-to-open-delimiter)
    (define-key keymap (kbd "<right>") #'ram-jump-forward-to-open-delimiter)

    (define-key keymap (kbd "M-<left>") #'ram-jump-backward-to-close-delimiter)
    (define-key keymap (kbd "M-<right>") #'ram-jump-forward-to-close-delimiter)

    (define-key keymap (kbd "S-<left>" ) #'ram-beg-of-top-sexp)
    (define-key keymap (kbd "S-<right>" ) #'ram-end-of-top-sexp)

    (define-key keymap (kbd "C-<left>" ) #'ram-up-list-backward)
    (define-key keymap (kbd "C-<right>" ) #'ram-up-list-forward)

    (define-key keymap (kbd "C-," ) #'ram-kill-at-point)

    (define-key keymap (kbd "C-:" ) #'ram-toggle-multiline-delimited-sexp)

    (define-key keymap (kbd "<return>") #'ram-newline-and-indent)
    (define-key keymap (kbd "S-<return>") #'newline-and-indent)
    (define-key keymap (kbd "C-S-<backspace>") #'ram-kill-whole-line)

    (define-key keymap (kbd "s-k") #'ram-next-comment)
    (define-key keymap (kbd "s-K") #'ram-previous-comment)

    (define-key keymap (kbd "C-<backspace>") #'ram-delete-whitespace-backward)
    (define-key keymap (kbd "C-d") #'ram-delete-char)
    keymap)
  "Keymap for `ram-manage-sexps-mode'")

;;** ram-manage-sexps-mode: define-minor-mode

(define-minor-mode ram-manage-sexps-mode
  "Minor mode handling s-expressions."
  nil " ram-manage-sexps-mode" ram-manage-sexps-mode-map
  (if ram-manage-sexps-mode
      (progn
        ;; put setting up the minor-mode code here
        ;; (message "enable ram-manage-sexps-mode")
        ;;
        )
    ;; put setting up the minor-mode code here
    ;; (message "disable ram-manage-sexps-mode")
    ))

;;*** ram-manage-sexps-mode: hooks, advice, timers

(add-hook 'emacs-lisp-mode-hook #'ram-manage-sexps-mode)
(add-hook 'lisp-interactive-mode #'ram-manage-sexps-mode)
(add-hook 'clojure-mode-hook #'ram-manage-sexps-mode)
(add-hook 'cider-repl-mode-hook #'ram-manage-sexps-mode)
(add-hook 'racket-mode-hook #'ram-manage-sexps-mode)
(add-hook 'racket-repl-mode-hook #'ram-manage-sexps-mode)

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

;; (require 'flycheck-clj-kondo)

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

;;** clojure: bindings

(defun ram-clojure-add-bindings ()
  (define-key clojure-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)
  (define-key clojure-mode-map (kbd "<f2> <f2>") #'ram-clojure-eval-defun-at-point)
  (define-key clojure-mode-map (kbd "<f2> <f7>") #'ram-clojure-tap-defun-at-point)

  (define-key clojure-mode-map (kbd "<f2> <f14>") #'ram-clojure-eval-last-sexp)
  (define-key clojure-mode-map (kbd "<f2> e") #'ram-clojure-eval-last-sexp)
  (define-key clojure-mode-map (kbd "<f2> t") #'ram-clojure-tap-last-sexp)

  (define-key clojure-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  (define-key clojure-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)

  (define-key clojure-mode-map (kbd "s-E") #'ram-switch-to-clojure-repl)

  (define-key clojure-mode-map (kbd "<return>") #'ram-newline-and-indent)
  (define-key clojure-mode-map (kbd "S-<return>") #'newline-and-indent)
  (define-key clojure-mode-map (kbd ":") #'ram-insert-column)

  (define-key clojure-mode-map (kbd "M-S-<f5>") #'ram-lsp-jump-workspace-symbol))

(add-hook 'clojure-mode-hook #'ram-clojure-add-bindings)

;;* clojure: cider
;; (setq package-check-signature nil)

(straight-use-package
 '(cider :type git :flavor melpa
         :files ("*.el" (:exclude ".dir-locals.el") "cider-pkg.el")
         :host github :repo "clojure-emacs/cider"))

(with-eval-after-load 'cider
  (cider-auto-test-mode 1) ;; run test on buffer load
  ;; (cider-fringe-good-face ((t (:foreground ,green-l))))
  (setq cider-save-file-on-load t)
  (setq cider-repl-wrap-history t)
  (setq cider-repl-history-size 1000)
  (setq cider-test-show-report-on-success nil)
  (setq cider-repl-history-file "~/.cider-repl-history")
  (setq cider-repl-display-help-banner nil)
  (setq cider-show-eval-spinner nil)
  (setq cider-eval-result-duration 'change)
  (face-spec-set
   'cider-fringe-good-face
   '((t :foreground "SkyBlue2"
        :width ultra-expanded
        :weight bold))
   'face-defface-spec))

;;** clojure/cider: functions

(defun ram-cider-eval (form)
  "Evaluate FORM calling `cider-interactive-eval'."
  (cider-nrepl-sync-request:eval form)
  ;; (let ((cider-show-eval-spinner 'nil)
  ;;       (cider-use-overlays 'nil))
  ;;   (cider-interactive-eval form (lambda (response) 'nil) nil nil))
  )

;;** clojure/cider: repl

(with-eval-after-load "cider"
  ;; use "C-<return>" for cider-repl-closing-return
  ;; (define-key cider-repl-mode-map (kbd "<f2>") #'cider-repl-return)
  )

;; start repl from Emacs with #'cider-jack-in
(setq cider-inject-dependencies-at-jack-in nil)
;; (setq cider-clojure-cli-parameters "-A:test:dev:local-dev")
;; (setq cider-clojure-cli-parameters "-M:inspect/reveal-cider")

;;** clojure: functions

(defun ram-clojure-get-last-sexp ()
  "Return the sexp preceding the point."
  (save-excursion
    (clojure-backward-logical-sexp 1)
    (let ((beg (point)))
      (clojure-forward-logical-sexp 1)
      (buffer-substring-no-properties beg (point)))))

(defun ram-clojure-get-defun-at-point ()
  "Return the toplevel sexp at point."
  (save-excursion
    (save-match-data
      (end-of-defun)
      (let ((end (point)))
        (clojure-backward-logical-sexp 1)
        (buffer-substring-no-properties (point) end)))))

(defun ram-clojure-eval-defun-at-point (arg)
  "Evaluate top level sexp at point."
  (interactive "P")
  (let ((orginal-workspace (exwm-workspace--position exwm-workspace--current))
        ;; the workspace that shows reveal buffers as defined in
        ;; display-buffer-alist
        (reveal-exwm-workspace 4))
    (ram-cider-eval (ram-clojure-get-defun-at-point))
    ;; switch to workspace that displays Reveal window
    (ram-reveal-display-buffers orginal-workspace reveal-exwm-workspace)))

(defun ram-clojure-eval-last-sexp (arg)
  "Evaluate the sexp preceding the point."
  (interactive "P")
  (let ((orginal-workspace (exwm-workspace--position exwm-workspace--current))
        ;; the workspace that shows reveal buffers as defined in
        ;; display-buffer-alist
        (reveal-exwm-workspace 4))
    (ram-cider-eval (ram-clojure-get-last-sexp))
    ;; switch to workspace that displays Reveal window
    (ram-reveal-display-buffers orginal-workspace reveal-exwm-workspace)))

(defun ram-clojure-tap-defun-at-point (arg)
  "Evaluate top level sexp at point wrapped in \"tap>\"."
  (interactive "P")
  (let ((orginal-workspace (exwm-workspace--position exwm-workspace--current))
        ;; the workspace that shows reveal buffers as defined in
        ;; display-buffer-alist
        (target-exwm-workspace 4))
    (ram-cider-eval (format "(tap> %s)" (ram-clojure-get-defun-at-point)))
    ;; switch to workspace that displays Reveal window
    (ram-reveal-display-buffers orginal-workspace target-exwm-workspace)))

(defun ram-clojure-tap-last-sexp (arg)
  "Evaluate the sexp preceding the point wrapped in \"tap>\"."
  (interactive "P")
  (let ((orginal-workspace (exwm-workspace--position exwm-workspace--current))
        ;; the workspace that shows reveal buffers as defined in
        ;; display-buffer-alist
        (reveal-exwm-workspace 4))
    (ram-cider-eval (format "(tap> %s)" (ram-clojure-get-last-sexp)))
    ;; switch to workspace that displays Reveal window
    (ram-reveal-display-buffers orginal-workspace reveal-exwm-workspace)))

;;** clojure: hooks, advice, timers

(add-hook 'clojure-mode-hook #'cider-mode)

;;** clojure: portal

(defun ram-portal.api/open ()
  "Open Portal and display the its buffer."
  (interactive)
  (let ((orginal-workspace (exwm-workspace--position exwm-workspace--current))
        ;; the workspace that displays portal reveal buffers as defined in
        ;; display-buffer-alist
        (reveal-exwm-workspace 4))
    (ram-cider-eval (ram-clojure-get-defun-at-point))
    ;; switch to workspace that displays Reveal window
    (cider-nrepl-sync-request:eval
     "(do (ns user) (def portal ((requiring-resolve 'portal.api/open))) (add-tap (requiring-resolve 'portal.api/submit)))")
    (ram-reveal-display-buffers orginal-workspace reveal-exwm-workspace)))

(defun ram-portal.api/clear ()
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/clear)"))

(defun ram-portal.api/close ()
  "Close Portal."
  (interactive)
  (cider-nrepl-sync-request:eval "(portal.api/close"))

;;** clojure/portal: bindings

(define-key global-map (kbd "<f2> o") #'ram-portal.api/open)
(define-key global-map (kbd "<f2> c") #'ram-portal.api/clear)

;;** clojure: reveal

(defun ram-reveal-display-buffers (original-exwm-workspace target-exwm-workspace)
  "Display Clojure data browsing buffers in TARGET-EXWM-WORKSPACE."
  ;; switch to target-exwm-workspace
  (exwm-workspace-switch target-exwm-workspace)
  (let* ((acc '())
         ;; xwidget and chrome are portal buffers, java is reveal
         (possible-display-buffers '("xwidget-webkit" "Google-chrome" "java"))
         (buf-names-regexp (cl-reduce
                            (lambda (acc s)
                              (format (if (string= acc "")
                                          "%s\\(%s\\)"
                                        "%s\\|\\(%s\\)") acc s))
                            possible-display-buffers :initial-value "")))
    (dolist (b (buffer-list))
      (when (string-match-p buf-names-regexp (buffer-name b))
        (setq acc (cons (buffer-name b) acc))))
    ;; in case there are "*java<1>*", "*java<2>*" etc
    ;; sort them in reverse so that the "*java<1>*" is last to display
    ;; and possibly will get focus
    (dolist (b (sort acc #'string<))
      ;; (display-buffer b nil (exwm-workspace--workspace-from-frame-or-index target-exwm-workspace))
      (display-buffer b)
      ))
  ;; (dolist (w (window-list-1 nil 'NOMINI (exwm-workspace--workspace-from-frame-or-index target-exwm-workspace)))
  ;;   (when (window-live-p w)
  ;;     (select-window w)
  ;;     ;; give time to render reveal windows
  ;;     (sleep-for 0.1)))
  ;; switch to previous workspace if it is not in the same monitor.
  (when (not (string= (frame-parameter
                       (exwm-workspace--workspace-from-frame-or-index original-exwm-workspace)
                       'exwm-randr-monitor)
                      (frame-parameter
                       (exwm-workspace--workspace-from-frame-or-index target-exwm-workspace)
                       'exwm-randr-monitor)))
    (exwm-workspace-switch original-exwm-workspace)))

(defun ram-reveal-clear ()
  (interactive)
  (let ((form "{:vlaaad.reveal/command '(clear-output)}"))
    (ram-cider-eval form)))

(defun ram-reveal-as-table (form-str)
  (ram-cider-eval
   (format "{:vlaaad.reveal/command
 '(open-view
   {:fx/type action-view
    :action :vlaaad.reveal.action/view:table
    :value v})
 ;; environment passed from current ns
 :env {'v %s}}}" form-str)))


(defun ram-reveal-defun-as-table ()
  (interactive)
  (ram-reveal-as-table (cider-defun-at-point)))

(defun ram-reveal-send-to-output (form)
  (ram-cider-eval (format "{:vlaaad.reveal/command '(submit %s)}" form)))

;; (define-key global-map (kbd "<f2> c") #'ram-reveal-clear)

;;** clojure: lsp

;; (setq package-selected-packages '(clojure-mode lsp-mode cider lsp-treemacs flycheck company))

(straight-use-package
 '(lsp-mode :type git :flavor melpa :files (:defaults "clients/*.el" "lsp-mode-pkg.el")
            :host github :repo "emacs-lsp/lsp-mode"))

(straight-use-package
 '(lsp-treemacs :type git :flavor melpa :files (:defaults "icons" "lsp-treemacs-pkg.el")
                :host github :repo "emacs-lsp/lsp-treemacs"))

(straight-use-package
 '(lsp-ui :type git :flavor melpa :files (:defaults "lsp-ui-doc.html" "resources" "lsp-ui-pkg.el")
          :host github :repo "emacs-lsp/lsp-ui"))

;;*** clojure/lsp: functions

;;**** clojure/lsp/functions: symbols completion

(defun ram-lsp-icomplete--transform-candidate (project-root candidate)
  "Transform `lsp-mode' &SymbolInformation to build completion table from."
  (require 'lsp-ido)
  (if-let* ((kind (gethash "kind" candidate))
            (sanitized-kind (if (< kind (length lsp-ido-symbol-kind-to-string)) kind 0))
            (type (elt lsp-ido-symbol-kind-to-string sanitized-kind))
            (typestr (if lsp-ido-show-symbol-kind (format "[%s] " type) ""))
            (location (gethash "location" candidate))
            (uri (gethash "uri" location))
            (pathstr (if lsp-ido-show-symbol-filename
                         (propertize (format " %s" (file-relative-name (lsp--uri-to-path uri) project-root))
                                     'face 'font-lock-comment-face)
                       ""))
            (textual-representation
             (lsp-render-symbol-information candidate "."))
            (entry (concat typestr textual-representation
                           (make-string
                            (- (window-width) (length typestr) (length textual-representation) (length pathstr))
                            32)
                           pathstr)))
      (cons entry candidate)))

(defun ram-lsp-request-workspaces-symbols (query)
  "Get all WORKSPACES symbols that match QUERY"
  (let* ((workspace-root (lsp-workspace-root))
         (raw-choices (with-lsp-workspaces (lsp-workspaces)
                        (lsp-request "workspace/symbol"
                                     (lsp-make-workspace-symbol-params :query query)))))
    (mapcar (apply-partially #'ram-lsp-icomplete--transform-candidate
                             workspace-root)
            raw-choices)))

(defvar ram-lsp-workspace-symbol-history nil "History for `lsp-mode' symbols completions.")
(put 'ram-lsp-workspace-symbol-history 'history-length 20)

(defun ram-lsp-add-to-workspace-symbol-history ()
  "Add search string entered in minibuffer to `ram-lsp-workspace-symbol-history'."
  (interactive)
  (let ((search-str (buffer-substring
                     (line-beginning-position) (line-end-position 1))))
    ;; (message ">>> this-command: %S" this-command)
    (when (>=  (length (buffer-substring
                        (line-beginning-position) (line-end-position 1)))
               2)
      (add-to-history 'ram-lsp-workspace-symbol-history search-str)))
  (minibuffer-force-complete-and-exit))
(intern "ram-lsp-add-to-workspace-symbol-history")

(defun ram-lsp-jump-to-workspace-symbol-from-minibuffer ()
  "Call `ram-lsp-jump-workspace-symbol' and use previous minibuffer input.
Toggle `lsp-ido-show-symbol-filename'."
  (interactive)
  (let ((user-input  (buffer-substring (point-at-bol) (point-at-eol)))
        (lsp-ido-show-symbol-filename (not lsp-ido-show-symbol-filename)))
    (minibuffer-with-setup-hook
        (lambda () (insert user-input))
      (condition-case err
          (with-current-buffer (window-buffer (minibuffer-selected-window))
            (call-interactively #'ram-lsp-jump-workspace-symbol))
        (quit (abort-recursive-edit))
        (:success (abort-recursive-edit))))))
(intern "ram-lsp-jump-to-workspace-symbol-from-minibuffer")

(defun ram-lsp-jump-selected-candidate (candidate)
  "Jump to selected candidate."
  (require 'dash)
  (-let (((&SymbolInformation
           :location (&Location :uri
                                :range (&Range :start (&Position :line :character))))
          candidate))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(defun ram-lsp-jump-workspace-symbol (arg)
  "Search among `lsp-mode' workspace symbols and jump to its definition."
  (interactive "P")
  (let* ((symbl-at-point (symbol-at-point))
         ;; (query (if (and (equal arg '(4)) symbl-at-point) (symbol-name symbl-at-point) ""))
         ;; if query is provided, you can search only in the narrowed
         ;; providing the query limits the search candidates,
         ;; if you decide to search for a part of the symbol, it would not work.
         ;; for now, allow searching from the full set of symbols
         (query "")
         (candidates (ram-lsp-request-workspaces-symbols query))
         ;; do not use default writing to history
         (history-add-new-input nil)
         (hist-item (car ram-lsp-workspace-symbol-history))
         (selected-symbol nil)
         (old-binding-to-return (cdr (assoc 'return (cdr minibuffer-local-completion-map))))
         (this-cmd-key (aref (this-command-keys) 0))
         (minibuffer-cmd-bound-to-this-cmd-key (cdr (assoc this-cmd-key
                                                           (cdr minibuffer-local-completion-map))))
         (reset-keybindings (lambda ()
                             "Rebind modified keys to their previous states."
                             (if old-binding-to-return
                                 (setf (alist-get 'return (cdr minibuffer-local-completion-map))
                                       old-binding-to-return)
                               (assq-delete-all 'return (cdr minibuffer-local-completion-map)))
                             (if minibuffer-cmd-bound-to-this-cmd-key
                                 (setf (alist-get this-cmd-key (cdr minibuffer-local-completion-map))
                                       minibuffer-cmd-bound-to-this-cmd-key)
                               (assq-delete-all this-cmd-key (cdr minibuffer-local-completion-map))))))

    ;; bind to 'return key the fn that writes to history
    (setf (alist-get 'return (cdr minibuffer-local-completion-map))
          #'ram-lsp-add-to-workspace-symbol-history)
    (setf (alist-get this-cmd-key (cdr minibuffer-local-completion-map))
          #'ram-lsp-jump-to-workspace-symbol-from-minibuffer)
    ;; bind to this-command-keys fn that toggles showing files

    (condition-case err
        (minibuffer-with-setup-hook
            (if (and (equal arg '(4))
                     symbl-at-point)
                (lambda () (insert (symbol-name symbl-at-point)))
              (lambda ()))
          (setq selected-symbol
                (assoc (completing-read
                        (if symbl-at-point
                            (format-prompt "Workspace Symbol" (symbol-name symbl-at-point))
                          (format-prompt "Workspace Symbol" (car ram-lsp-workspace-symbol-history)))
                        candidates
                        nil t nil
                        'ram-lsp-workspace-symbol-history
                        (if symbl-at-point
                            (symbol-name symbl-at-point)
                          ram-lsp-workspace-symbol-history))
                       candidates)))
      (error (funcall reset-keybindings) (signal (car err) (cdr err)))
      (quit (funcall reset-keybindings) (signal 'quit nil))
      (:success (funcall reset-keybindings)))

    ;; append selected symbol to history
    (when selected-symbol
      (setq ram-lsp-workspace-symbol-history
            ;; get the symbol name only (disregard the type and filepath)
            (cons (let ((split (split-string (car selected-symbol) nil 'omit-nulls)))
                    (if lsp-ido-show-symbol-kind
                        (cadr split)
                      (car-split)))
                  ram-lsp-workspace-symbol-history)))

    ;; reorder history so that the search string is fist and the
    ;; selected item is second.

    ;; if user input was inserted, we added two items in total
    (if (string= hist-item (caddr ram-lsp-workspace-symbol-history))
        (delete-dups
         (setq ram-lsp-workspace-symbol-history
               (cons (cadr ram-lsp-workspace-symbol-history)
                     (cons (car ram-lsp-workspace-symbol-history)
                           (cddr ram-lsp-workspace-symbol-history))))))

    (when selected-symbol
      (setq my-selected-symbol (cdr selected-symbol))
      (when (minibufferp)
        (let ((pre-minibuffer-buffer (with-minibuffer-selected-window
                                       (current-buffer))))
          (switch-to-buffer pre-minibuffer-buffer)
          (push-mark)))

      (ram-lsp-jump-selected-candidate (cdr selected-symbol))

      (let ((default (if (boundp pulse-flag)
                         pulse-flag
                       nil)))
        ;; pulse-iteration pulse-delay
        (setq pulse-flag nil)
        (pulse-momentary-highlight-one-line (point) 'isearch)
        (setq pulse-flag default)))))

;;*** clojure/lsp: settings

(setq lsp-clojure-custom-server-command '("bash" "-c" "$HOME/.local/bin/clojure-lsp"))
(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-lens-enable nil
      ; lsp-enable-indentation nil ; uncomment to use r indentation instead of lsp
      ; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp
      )
(setq lsp-lens-place-position 'above-line)
(setq lsp-enable-symbol-highlighting nil)

;;**** clojure/lsp/settings: docs
;; increase the text size
(setq lsp-ui-doc-use-webkit nil)
(setq lsp-ui-doc-text-scale-level 3.0)
;; set the doc popup overlay hight
(setq lsp-ui-doc-max-height 55)
(setq lsp-ui-doc-enable t)
;; this seem to cause jumping between the frames
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-mouse nil)
(setq lsp-headerline-breadcrumb-enable t)

;; this shows info in the right side of the code
(setq lsp-ui-sideline-enable t)
;; actions that can be applied to the code
;; could be useful when you learn it
(setq lsp-ui-sideline-show-code-actions nil)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-diagnostics nil)

(setq lsp-modeline-code-actions-enable t) ; do not see anything in modeline
(setq lsp-modeline-diagnostics-enable t)  ; same, does not work

(setq lsp-diagnostics-provider :auto)
(setq lsp-eldoc-enable-hover t) ; supposed to show docs in minibuffer

(setq lsp-signature-auto-activate nil)
(setq lsp-signature-render-documentation nil)

(setq lsp-completion-provider :capf)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)

;;*** clojure/lsp: hooks, advice, timers

(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
(add-hook 'clojurec-mode-hook 'lsp)

;;* python

;;** python: ide

(straight-use-package
 '(elpy :type git :flavor melpa
        :files ("*.el" "NEWS.rst" "snippets" "elpy" "elpy-pkg.el")
        :host github :repo "jorgenschaefer/elpy"))

;;** python: outlines

(defun python-mode-outline-hook ()
  (setq-local outline-level 'python-outline-level)

  (setq-local outline-regexp
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

(setq elpy-rpc-virtualenv-path "~/.virtualenvs/elpy-enc/")
(setenv "PYTHONIOENCODING" "utf-8")
(with-eval-after-load 'elpy
  (add-to-list 'process-coding-system-alist '("python" . (utf-8 . utf-8)))
  (add-to-list 'process-coding-system-alist '("elpy" . (utf-8 . utf-8)))
  (add-to-list 'process-coding-system-alist '("flake8" . (utf-8 . utf-8))))

;; (add-hook 'python-mode-hook #'elpy-enable)
;; (remove-hook 'python-mode-hook #'elpy-enable)

(add-hook 'python-mode-hook #'outline-minor-mode)

(add-hook 'python-mode-hook 'python-mode-outline-hook)

(with-eval-after-load 'python
  (define-key python-mode-map (kbd "<M-f5>") 'ram-jump-to-outline))
  ;; (define-key python-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  ;; (define-key python-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  ;; (require 'flycheck-clj-kondo)


;;* prolog

;;** prolog: prolog-mode

;; !!! run prolog-consult-buffer if you need to open console

(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
;; (autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)  ; optional, the system you are using;
                           ; see `prolog-system' below for possible values

;; (setq prolog-electric-if-then-else-flag t)

(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

;; (setq prolog-electric-newline-flag nil)
(add-hook 'prolog-mode #'display-line-numbers-mode)

;;** prolog: ediprolog

(straight-use-package
 '(ediprolog :type git :host github :repo "emacs-straight/ediprolog" :files ("*" (:exclude ".git"))))

(with-eval-after-load "ediprolog"
  (custom-set-variables
   '(ediprolog-system 'swi)
   '(ediprolog-program "/usr/bin/swipl")))

;;** prolog: bindings

(with-eval-after-load "prolog"
  (define-key prolog-mode-map (kbd "C-c l") (lambda ()
                                              (interactive)
                                              (insert ":- use_module(library(()).")
                                              (forward-char -3))))
;; refer to section
;; org-mode/org-babel: org-babel-eval-in-repl
;; for commented binding original setting
;; (define-key ram-leader-map-tap-org (kbd "e") #'ober-eval-block-in-repl)

(define-key global-map (kbd "<f2> <f2>") #'ram-prolog-dwim)

(with-eval-after-load "ediprolog"
  (define-key prolog-mode-map (kbd "C-x C-e") #'ediprolog-dwim)
  (define-key prolog-mode-map (kbd "C-M-x") #'ediprolog-dwim))

;;** prolog: functions

;; Motivation: when #'ediprolog-dwim command is envoked
;; and the cursor is not on a query, it attempts to consult the
;; whole buffer. If you are in a Org source code block, then
;; the whole org buffer is consulted. This usually makes the
;; prolog process hang.
;; This is why, depending on whether the point is on the query or not
;; you either call
;; - ober-eval-block-in-repl
;; - ediprolog-dwim
(defun ram-prolog-dwim ()
  "Either call `ediprolog-dwim' or `ober-eval-block-in-repl'"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^%\\?-" 'inhibit-modify)
        (ediprolog-dwim)
      (ober-eval-block-in-repl))))

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

  (add-to-list 'super-save-triggers #'ram-org-roam-node-find)

  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-today)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-tomorrow)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-yesterday)
  (add-to-list 'super-save-triggers #'org-roam-dailies-goto-date)
  (add-to-list 'super-save-triggers #'ram-org-roam-next-note-dwim)
  (add-to-list 'super-save-triggers #'ram-org-roam-prev-note-dwim)
  (add-to-list 'super-save-triggers #'ram-choose-from-recentf)

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

(defface ram-mode-line-cpu-temp-cool-face
  '((((class color) (min-colors 88))
     ;; (:foreground "pink2")
     (:foreground "green4" :weight light)))
  "Face for mode-line text showing cpu temp considered to be cool."
  :group 'ram-mode-line)

(defface ram-mode-line-cpu-temp-hot-face
  '((((class color) (min-colors 88))
     (:foreground "brown2" :weight bold)))
  "Face for mode-line text showing cpu temp considered to be hot."
  :group 'ram-mode-line)

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
    ;; get sensors output as json:
    (process-send-string ram-shell-sensors-cmd-name "sensors -j\n")
    (when output
      (if-let* ((parsed-output (condition-case err
                                   (json-parse-string output :object-type 'alist :array-type 'list)
                                 ;; error: (json-trailing-content 121 1 2973)
                                 ;; (error "???")
                                 (error nil)))
                (temp-inputs (seq-filter (lambda (res) (string-prefix-p "Core" (symbol-name (car res))))
                                         (cdr (assq 'coretemp-isa-0000 parsed-output))))
                (temps (mapcar (lambda (l) (cdadr l)) temp-inputs))
                (cpu-temp (if (not (= 0 (length temps)))
                              (floor (/ (cl-reduce #'+ temps) (length temps)))
                            0))
                (cpu-temp-str (if (< cpu-temp 60)
                                  (propertize (number-to-string cpu-temp)
                                              'face 'ram-mode-line-cpu-temp-cool-face)
                                (propertize (number-to-string cpu-temp)
                                            'face 'ram-mode-line-cpu-temp-hot-face)))
                (ssd-temp (floor (cdar (cdaddr (assq 'nvme-pci-0200 parsed-output)))))
                (ssd-temp-str (if (< ssd-temp 60)
                                  (propertize (number-to-string ssd-temp)
                                              'face 'ram-mode-line-cpu-temp-cool-face)
                                (propertize (number-to-string ssd-temp)
                                            'face 'ram-mode-line-cpu-temp-hot-face))))
          (setq ram-cpu-temp-mode-line-str (concat  cpu-temp-str " " ssd-temp-str))))))

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

;;** mode-line: timer

;; (cancel-timer mode-line-timer)
;; (run-at-time 1 nil #'(lambda () (force-mode-line-update t)))
(defvar mode-line-timer
  ;; (run-with-timer 1 1 #'(lambda () (force-mode-line-update t)))
  (run-with-idle-timer 0.5 t #'(lambda () (force-mode-line-update t))))
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
                ;; show workspace number
                (:eval (if (and (window-at-side-p (get-buffer-window) 'bottom)
                                (window-at-side-p (get-buffer-window) 'left))
                           (propertize (format " %s " (exwm-workspace--position
                                                        (window-frame (get-buffer-window))))
                                       'face (if (eq ram-selwin (get-buffer-window))
                                                 '((:foreground "#00ff00") (:weight semi-light))
                                               '((:foreground "#008800") (:weight semi-light))))
                         "   "))
                (:eval (when (buffer-narrowed-p)
                         (progn (propertize " %n  " 'face
                                            (if (eq ram-selwin (get-buffer-window))
                                                '((:background "#32cd32") (:weight semi-light)) ; LimeGreen
                                              '((:background "#32cd32") (:weight semi-light)))
                                            ))))

                ;; on remote machine?
                "[" "%@" "]"
                ;; mode-line-modified
                ;; %, * or - for read only, changed, saved
                " %*%*%* "
                ;; mode-line-buffer-identification

                ;; (:eval (propertize "%b " 'face font-lock-keyword-face
                ;;                    'help-echo (buffer-file-name)))
                ;; buffer-file-name
                (:eval (propertize (format "%s " (if (> (length (buffer-name)) 33)
                                                     (format "%s...%s" (substring (buffer-name) 0 13)
                                                             (substring (buffer-name) (- (length (buffer-name))
                                                                                         20)))
                                                   (buffer-name)))
                                   'face (if (eq ram-selwin (get-buffer-window))
                                             '((:foreground "aquamarine") (:weight semi-light)) ; RosyBrown1
                                           '((:foreground "Blue3") (:weight semi-light)))
                                   'help-echo (buffer-file-name)))
                ;; git-gutter:statistic
                ;;
                (:eval (when (and vc-mode
                                  ;; otherwise (file-truename (git-gutter:vcs-root git-gutter:vcs-type))
                                  ;; would raise --Lisp error: (wrong-type-argument arrayp nil)
                                  (and (local-variable-p 'git-gutter:vcs-type) (not (null git-gutter:vcs-type))))
                         (propertize (format "+%s " (car (git-gutter:statistic)))
                                     'face (if (eq ram-selwin (get-buffer-window))
                                               '((:foreground "#00ff00") (:weight semi-light))
                                             '((:foreground "#008800") (:weight semi-light)))
                                     )))
                (:eval (when (and (vc-mode)
                                  ;; otherwise (file-truename (git-gutter:vcs-root git-gutter:vcs-type))
                                  ;; would raise --Lisp error: (wrong-type-argument arrayp nil)
                                  (and (local-variable-p 'git-gutter:vcs-type) (not (null git-gutter:vcs-type))))
                         (propertize (format "-%s " (cdr (git-gutter:statistic)))
                                     'face (if (eq ram-selwin (get-buffer-window))
                                               '((:foreground "#ffc1c1") (:weight semi-light)) ; RosyBrown1
                                             '((:foreground "#d2691e") (:weight semi-light)))))) ; chocolate
                (:eval (my-mode-line-vc-info))

                ;; (:eval (propertize vc-mode 'face '((:foreground "DarkGoldenrod2"))))
                ;; line and column
                " ("
                "%02l" "," "%01c" "," "%01P%"
                ") "
                ;; value of `mode-name'
                "%["
                (:eval (propertize " %m " 'face (if (eq ram-selwin (get-buffer-window))
                                                    '((:foreground "#ffc1c1") (:weight semi-light)) ; RosyBrown1
                                                  '((:foreground "#d2691e") (:weight semi-light))) ; chocolate
                                   ))
                "%]"
                mode-line-process
                ;; mode-line-modes
                ;; "-- user: "
                ;; value of user
                ;; (getenv "USER")

                ;; this line is used as a marker where to insert the
                ;; keycast package content
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
                                                     '((:foreground "grey40")))))
                            (cpu-temp (or ram-cpu-temp-mode-line-str ""))
                            (bat (ram-get-battery-status))
                            (time (propertize display-time-string
                                              'face (if (eq ram-selwin (get-buffer-window))
                                                        '((:foreground "grey90"))
                                                      '((:foreground "grey40")))))
                            (right-align-str (*-mode-line-fill (+ (+ (length mem)
                                                                     (length cpu-temp)
                                                                     1
                                                                     (length bat)
                                                                     (length time))
                                                                  ;; use this value adjust aligning
                                                                  3))))
                       (concat right-align-str
                               mem
                               cpu-temp
                               (if bat
                                   (concat " " bat " ")
                                 "")
                               " "
                               time)))))))

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
;; default depends on isearch-lax-whitespace
;; (setq search-whitespace-regexp ".*?")
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

;;** isearch: bindings

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

;;*** search char, next, prev char

(straight-use-package
 '(iy-go-to-char :type git :flavor melpa :host github :repo "doitian/iy-go-to-char"))

(define-key global-map (kbd "s-d") #'iy-go-up-to-char)
(define-key global-map (kbd "s-D") #'iy-go-to-char-backward)
(define-key global-map (kbd "C-s-d") #'iy-go-to-or-up-to-continue)
(define-key global-map (kbd "C-S-s-d") #'iy-go-to-or-up-to-continue-backward)


;;** packages: haskell-mode

;; (straight-use-package
;;  '(haskell-mode :type git :flavor melpa
;;                 :files (:defaults "NEWS" "logo.svg" "haskell-mode-pkg.el")
;;                 :host github :repo "haskell/haskell-mode"))
;; (require 'haskell)
;; (setq haskell-interactive-mode-eval-mode 'haskell-mode)

;; add capability to submit code to interpreter and mark errors
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; add missing keybindings for navigating errors
;; (define-key interactive-haskell-mode-map (kbd "M-n") 'haskell-goto-next-error)
;; (define-key interactive-haskell-mode-map (kbd "M-p") 'haskell-goto-prev-error)
;; (define-key interactive-haskell-mode-map (kbd "C-c M-p")
;;  'haskell-goto-first-error)

;; merge this with your existing custom-set-variables
;; (custom-set-variables

;;  ;; NOTE: include following line to work around haskell-mode
;;  ;; bug if using GHC >= 8.2.1.
;;  ;; See: https://github.com/haskell/haskell-mode/issues/1553
;;  '(haskell-process-args-stack-ghci
;;    '("--ghci-options=-ferror-spans -fshow-loaded-modules"
;;      "--no-build" "--no-load"))

;;  ;; some options suggested in the haskell-mode documentation
;;  '(haskell-process-auto-import-loaded-modules t)
;;  '(haskell-process-log t)
;;  '(haskell-process-suggest-remove-import-lines t))

;;** packages: keycast

(straight-use-package
 '(keycast :type git :flavor melpa :host github :repo "tarsius/keycast"))

(setq keycast-mode-line-insert-after 'mode-line-process)
(setq keycast-mode-line-remove-tail-elements nil)

(setq keycast-remove-tail-elements nil)
(setq keycast-separator-width 2)
(defun ram-keycast-left-frame-bottom-window-p ()
  "Return non-nil for specific temporarily selected window location.

It is in the left-most frame. It is at the bottom."
  ;; frame X coord is not 0, thus, it is not the top left frame
  (and (= 0 (car (frame-position (window-frame (get-buffer-window)))))
       (window-at-side-p nil 'bottom)))
;; choose left frame for keycast because the right frame displays
;; system info and date
(setq keycast-mode-line-window-predicate #'ram-keycast-left-frame-bottom-window-p)
(keycast-mode-line-mode 1)

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

(define-key paredit-mode-map (kbd "M-[") #'paredit-wrap-square)
(define-key paredit-mode-map (kbd "M-{") #'paredit-wrap-curly)

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
;; paredit redefines <return> key, define a custom minor-mode
;; that binds <return> to the correct command.
(define-minor-mode ram-eval-expression-minibuffer-local-mode
    "Minor mode only to simulate buffer local keybindings."
    :init-value nil
    :keymap (make-sparse-keymap))
(define-key ram-eval-expression-minibuffer-local-mode-map (kbd "<return>") #'read--expression-try-read)
(add-hook 'eval-expression-minibuffer-setup-hook (lambda () (enable-paredit-mode)
                                                      (setq-local ram-eval-expression-minibuffer-local-mode t)))
(add-hook 'racket-mode-hook #'enable-paredit-mode)
(add-hook 'racket-repl-mode-hook #'enable-paredit-mode)

;; (define-key lisp-interaction-mode-map (kbd "RET" ) #'electrify-return-if-match)

(defun ram-expand-abbrev-before-paredit-open-round ()
  "Call `expand-abbrev' before invoking `paredit-open-round'."
  (interactive)
  (condition-case err
      (progn (expand-abbrev)
             (paredit-open-round))
    ((debug error) (signal (car err) (cdr err)))))

(with-eval-after-load 'paredit
  (define-key paredit-mode-map (kbd "(") #'ram-expand-abbrev-before-paredit-open-round)
  ;; change order of paredit-mode and ram-manage-sexps-mode entries in minor-mode-map-alist
  ;; to prioritize ram-manage-sexps-mode keybindings:
  (if-let* ((paredit (assq 'paredit-mode minor-mode-map-alist))
            (manage-sexps (assq 'ram-manage-sexps-mode minor-mode-map-alist))
            (alist (assq-delete-all 'paredit-mode (assq-delete-all 'ram-manage-sexps-mode minor-mode-map-alist)))
            )
      (setq minor-mode-map-alist (cons manage-sexps (cons paredit alist)))))

;;** packages: recentf

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-saved-items 4000)
(run-at-time nil (* 1 60) 'recentf-save-list)
(run-at-time nil (* 3.3 60) #'ram-recentf-remove-non-existent-files)
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

(defun ram-recentf-keep-predicate (file)
  "Custom cases for `recentf-keep' list."
  (cond
   ;; exclude files that end with "~"
   ((string-match-p "^.*~$" f) nil)
   ;; keep files opened with "sudo" even if they are closed
   ;; (not readable)
   ((string-match-p "^/sudo:.*$" f))))

(defun ram-recentf-remove-non-existent-files ()
  "Remove filenames from `recentf-list' if conditions are not met."
  (setq recentf-list
        (seq-filter (lambda (f)
                      (cond
                       ;; exclude files that end with "~"
                       ((string-match-p "^.*~$" f) nil)
                       ;; keep files opened with "sudo"
                       ((string-match-p "^/sudo:.*$" f))
                       ((file-readable-p f))))
                    recentf-list)))

(add-to-list 'recentf-keep #'ram-recentf-keep-predicate)

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
    ;; --follow links, --no-follow
    ;; --no-ignore-vcs do not respect vc ignore files (.gitignore etc)
    ;; --glob=!pattern exclude glob match, --glob=pattern include match
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
(setq which-key-popup-type 'minibuffer)
;; (setq which-key-popup-type 'side-window)
;; use "C-h" key to access which-key bindings
;; in help-map, "\C-h" is originally bound to help-for-help
(define-key help-map "\C-h" 'which-key-C-h-dispatch)
(run-with-idle-timer 1 nil (lambda () (which-key-mode t)))

;;* prog-mode

;;** prog-mode: hook

;;** prog-mode: binding

;;* sparql

(straight-use-package
 '(sparql-mode :type git :flavor melpa :host github :repo "ljos/sparql-mode"))

(defun ram-org-babel-execute:sparql-remove-prefixes-in-results (body params)
  "Test `org-babel-execute:sparql'."
  (let ((url (cdr (assoc :url params)))
        (format (cdr (assoc :format params)))
        (query (org-babel-expand-body:sparql body params))
        (org-babel-sparql--current-curies (append org-link-abbrev-alist-local org-link-abbrev-alist))
        (prefixes (let ((start 0)
                        (prefs))
                    (while (string-match "^PREFIX[[:space:]]+\\(?1:[[:alpha:]]*?\\:\\)[[:space:]]+<\\(?2:.+?\\)>$"
                                         body start)
                      (push (cons (match-string 1 body) (match-string 2 body)) prefs)
                      (setq start (match-end 0)))
                    prefs)))
    (with-temp-buffer
      (sparql-execute-query query url format t)
      (dolist (pref prefixes)
        (goto-char (point-min))
        (while (re-search-forward (cdr pref) nil t)
          (replace-match (car pref))))
      (org-babel-result-cond
          (cdr (assoc :result-params params))
        (buffer-string)
        (if (string-equal "text/csv" format)
            (org-babel-sparql-convert-to-table)
          (buffer-string))))))

(with-eval-after-load "ob-sparql"
  (fset 'org-babel-execute:sparql #'ram-org-babel-execute:sparql-remove-prefixes-in-results))

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

;;** spelling: flycheck, flyspell

(straight-use-package
 '(flycheck :type git :flavor melpa :host github :repo "flycheck/flycheck"))
(add-hook 'clojure-mode-hook #'flycheck-mode)

;;*** spelling/flycheck, flyspell: settings

;; credit to https://joelkuiper.eu/spellcheck_emacs

(dolist (hook '(text-mode-hook
                org-mode))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(emacs-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook))
  (add-hook hook
            (lambda ()
              (flyspell-prog-mode))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;*** spelling/flycheck, flyspell: binding

(with-eval-after-load "flyspell"
  ;; disable binding for
  ;; #'flyspell-auto-correct-word
  ;; is bound to C-. and C-M-i.
  (define-key flyspell-mode-map (kbd "C-.") nil)
  (define-key flyspell-mode-map (kbd "C-M-i") nil)

  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key global-map (kbd "s-M-c") 'ispell-word)

  (define-key global-map (kbd "s-M-C") 'flyspell-check-next-highlighted-word))

  ;; hyper key is disables in favor of f1, ... keys
  ;; (define-key global-map (kbd "C-H-e") 'flycheck-next-error)
  ;; (define-key global-map (kbd "C-H-E") 'flycheck-previous-error)
(define-key global-map (kbd "<M-f11>") 'flyspell-goto-previous-error)

;;*** spelling/flycheck, flyspell: functions

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

;;*** system/comments: comment-or-uncomment-sexp

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
(defun ram-fontify-frame (frame)
  (interactive)
  (if window-system
      (if (eq (window-system) 'x)
          (let* (
                 ;; !!! in MONITOR-WIDTH calculation, (nth 3 ...) is
                 ;; when the monitors are rotated vertically in xrandr,
                 ;; for example: xrandr --output HDMI-1 --rotate left
                 (monitor-attr (display-monitor-attributes-list frame))
                 (frame-monitor (car (seq-filter (lambda (m) (member frame (assq 'frames m))) monitor-attr)))
                 (monitor-height-mm (nth 2 (assq 'mm-size frame-monitor)))
                 ;; (nth 3 ...) is for rotated screen
                 ;; (nth 4 ...) is for normal position
                 (monitor-height-px
                  (nth 3 (assq 'geometry frame-monitor))))
            (cond
             ;; 27 inch 4K (UDH)
             ((and (= monitor-height-mm 340)
                   (= monitor-height-px 2160))
              ;; (set-face-attribute 'variable-pitch frame :family "Verdana-19")
              (set-face-attribute 'variable-pitch frame :family "Bembo" :height 270 :weight 'normal)
              (set-face-attribute 'fixed-pitch frame :family "Operator Mono Medium-20" :height 200 :weight 'light)
              (set-frame-parameter frame 'font "Operator Mono Medium-20")
              (set-face-attribute
               'font-lock-comment-face frame
               :family "Operator Mono Medium-19"
               :height 190
               :weight 'light
               :slant 'italic
               :foreground "grey60"
               )
              (set-face-attribute
               'font-lock-doc-face frame
               :family "Operator Mono Light-19"
               :foreground "grey60"
               :weight 'light
               :slant 'italic)
              (set-face-attribute
               'font-lock-string-face frame
               ;; :foreground "#145c33"    ; green
               ;; :foreground "#a0132f"    ; red
               :foreground "#2544bb"    ; blue
               :weight 'light
               :slant 'italic
               :height 190
               )
              (set-face-attribute
               'mode-line frame
               :box '(:line-width 6 :style flat-button)
               :foreground "black" :background "grey55"
               :weight 'light
               :family "Operator Mono Medium-19"
               :height 230)
              (set-face-attribute
               'mode-line-inactive frame
               :box '(:line-width 6 :style flat-button)
               :weight 'light
               :foreground "grey20" :background "grey90"
               :height 230))
             ;; UPERFECT 18 inch 4K (UDH)
             (;;  for some reason, my UPERFECT 18 inch, 4K monitor
              ;;  sets incorrect height in mm of 355mm whereas the true
              ;;  height is around 233mm
              (and (= monitor-height-mm 355)
                   (= monitor-height-px 2160))
              ;; (set-face-attribute 'variable-pitch frame :family "Verdana-19")
              (set-face-attribute 'variable-pitch frame :family "Bembo" :height 390 :weight 'medium)
              (set-face-attribute 'fixed-pitch frame :family "Operator Mono Medium" :height 280 :weight 'light)
              ;; defaults to :weight semi-light
              (set-face-attribute 'default frame :family "Operator Mono Medium" :height 280 :weight 'light)
              (set-frame-font
               ;; :size is in points, not in 1/10 points as in :height
               (font-spec :family "Operator Mono" :size 28.0 :weight 'light :slant 'normal)
               nil (list frame))
              (set-face-attribute
               'font-lock-keyword-face frame
               :family "Operator Mono"
               :weight 'semi-light
               :slant 'normal
               )
              (set-face-attribute
               'font-lock-comment-face frame
               :family "Operator Mono"
               :foreground "grey40"
               :height 260
               :weight 'light
               :slant 'italic
               )
              (set-face-attribute
               'font-lock-doc-face frame
               :family "Operator Mono"
               :foreground "grey40"
               :height 260
               :weight 'light
               :slant 'italic)
              (set-face-attribute
               'font-lock-string-face frame
               :family "Operator Mono Light"
               ;; :foreground "#145c33"    ; green
               ;; :foreground "#a0132f"    ; red
               :foreground "#2544bb"    ; blue
               :height 270
               :weight 'light
               :slant 'italic
               )
              ;; mode-line
              (set-face-attribute
               'mode-line frame
               :foreground "black" :background "grey55"
               :box '(:line-width 6 :style flat-button)
               :height 290
               :weight 'light
               )
              (set-face-attribute
               'mode-line-inactive frame
               :foreground "black" :background "grey90"
               :box '(:line-width 6 :style flat-button)
               :height 290
               :weight 'light
               ))
             (t nil)
             )))))

;; Fontify all frames

;; this one works but unfocused screen remains blank until receives focus.
;; (add-hook 'exwm-randr-refresh-hook (lambda () (mapcar #'ram-fontify-frame (frame-list))))

(mapcar #'ram-fontify-frame (frame-list))
(set-face-documentation 'font-lock-comment-face
                        "I CUSTOMIZED font-lock-comment-face to make it less prominent.")
(set-face-documentation 'font-lock-string-face
                        "I CUSTOMIZED font-lock-string-face to make it less prominent.")
(set-face-documentation 'font-lock-doc-face
                        "I CUSTOMIZED font-lock-doc-face to make it less prominent.")

(run-with-idle-timer 1 nil (lambda () (mapcar #'ram-fontify-frame (frame-list))))

;; Fontify any future frames (new frame as given as argument)
;; all frames have the SAME WIDTH (for the same monitor)
;; (push 'ram-fontify-frame after-make-frame-functions)

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
;; (set-face-attribute 'fixed-pitch nil :family "Operator Mono Medium-19")
;; (set-face-attribute 'fixed-pitch nil :font "-misc-operator mono medium-medium-r-normal--0-90-0-0-m-0-iso10646-1")
;; (set-face-attribute 'fixed-pitch nil :font "-adobe-courier-medium-r-normal--25-180-100-100-m-150-iso10646-1")




;; (set-face-attribute 'variable-pitch nil :font "FiraGo-18")

;; (set-face-attribute 'variable-pitch nil :font "Calibri-20")
(set-face-attribute 'variable-pitch nil :family "Verdana-19")

;;** system: fringe, margin

;; (run-with-idle-timer 2 nil (lambda () (fringe-mode '(20 . 20))))
(run-with-idle-timer 2 nil (lambda () (fringe-mode '(0 . 3))))

;;** system: general settings

(setq delete-by-moving-to-trash t)

(setq confirm-nonexistent-file-or-buffer t)

(setq confirm-kill-emacs 'y-or-n-p)

(setq large-file-warning-threshold nil)

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

;; nil: highlight only selected windows
(setq hl-line-sticky-flag nil)
(setq global-hl-line-sticky-flag nil)

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
;;(when window-system (global-hl-line-mode 1))
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

;;*** system/general settings/language: input method

;;**** system/general settings/language/input method: french

;; consult my note on "input method languages"
(setq default-input-method "french-postfix")

;;*** system/general settings: lossage

;; set lossage-size
;; keep the track of the pressed you have made
;; you may need it trace the keybindings that were messed up
;; by enabling/disabling a minor mode

(lossage-size 3000)

;;*** system/general settings: *scratch*, lisp-interaction-mode

(defun ram-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(define-key lisp-interaction-mode-map (kbd "<S-return>") 'newline-and-indent)

;; (define-key ram-leader-map-tap-global (kbd "'") 'ram-switch-to-scratch)

;;*** system/general settings: eval

(setq eval-expression-print-length 100)

(defun ram-eval-expression-to-kill-ring ()
  (interactive)
  (call-interactively 'eval-expression)
  ;; have to store only strings because:
  ;; Debugger entered--Lisp error: (wrong-type-argument sequencep t)
  ;; menu-bar-update-yank-menu(t nil)
  ;; kill-new(t)

  ;; (let ((old-fn (symbol-function 'menu-bar-update-yank-menu)))
  ;;   (fset 'menu-bar-update-yank-menu nil)
  ;;   (kill-new (car values))
  ;;   (fset 'menu-bar-update-yank-menu old-fn))

  ;; The above does not work either
  ;; (wrong-type-argument sequencep t)
  ;; #<subr F616e6f6e796d6f75732d6c616d626461_anonymous_lambda_125>(t)
  ;; read-from-kill-ring("Yank from kill-ring: ")
  ;; yank-pop(1)
  ;; funcall-interactively(yank-pop 1)
  ;; command-execute(yank-pop)
  (kill-new (if (stringp (car values))
                (car values)
              (format "%S" (car values)))))

(define-key global-map (kbd "M-:" ) #'ram-eval-expression-to-kill-ring)

;;*** system/general settings: file, symlink

;; (describe-variable 'find-file-visit-truename)
;; if nil, buffer-file-name will show the symlink path
;; if t, it will show the original file
;; Implications:
;; file may not be considered version controlled
;; org-roam may not consider it as org-roam-buffer-p
(setq find-file-visit-truename t)

;;*** system/general settings: info/manual

(define-key Info-mode-map (kbd "s-l") #'ace-link)


;;*** system/general settings: errors, warnings

(setq warning-minimum-level :error)
;; (setq native-comp-async-report-warnings-errors 'silent)

;;*** system/general settings: vc

;; a value of t/ask has an effect only when
;; vc-find-file-hook is a member of find-file-hook
;; if you wish, set it with
;; (add-hook 'find-file-hook #'vc-find-file-hook)
;; 'Git must be in vc-handled-backends
(setq vc-follow-symlinks t)
(add-hook 'find-file-hook #'vc-find-file-hook)

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

;;** system: unix signals, SIGHUP, SIGINT, SIGTERM, SIGTSTP, SIGUSR, USR

;; credit to
;; https://emacs.stackexchange.com/questions/44085/how-does-emacs-respond-to-unix-signals

;;*** system/unix-signals: locked out, suspend frame

;; HOW TO:
;; change TTY by pressing "C-M-<f2>",
;; find the pid of emacs that is running in the other TTY:
;; ps aux | -i grep emacs
;; check the signal types on your system with
;; kill -l
;; send signal to that pid (USR1 or SIGUSR1, USR2 or SIGUSR2 etc)
;; kill -USR1 $you_emacs_pid_from_the_above_command
;; kill -USR2 $you_emacs_pid_from_the_above_command

;;*** system/unix-signals: SIGUSR1 (USR1), SIGUSR2 (USR2)

;; WHAT IS IT?
;; "C-h i g" (elisp)Misc Events
;;
;; ‘sigusr1’
;; ‘sigusr2’
;;      These events are generated when the Emacs process receives the
;;      signals ‘SIGUSR1’ and ‘SIGUSR2’.  They contain no additional data
;;      because signals do not carry additional information.  They can be
;;      useful for debugging (*note Error Debugging::).
;;
;;      To catch a user signal, bind the corresponding event to an
;;      interactive command in the ‘special-event-map’
;; (*note Controlling Active Maps::).  The command is called with no arguments, and the
;;      specific signal event is available in ‘last-input-event’
;; (*noteEvent Input Misc::).  For example:

;; To test the signal handler, you can make Emacs send a signal to
;; itself:
;; (signal-process (emacs-pid) 'sigusr1)


;; "C-h i g" (elisp) Event Examples

;;**** system/unix-signals/SIGUSR1 (USR1)

;;    To handle a SIGUSR1 signal, define an interactive function, and bind
;; it to the ‘signal usr1’ event sequence:

(defun sigusr-handler ()
  (interactive)
  ;; (keyboard-quit)
  (message ">>>>>>START SIGUSR1 handling ...")
  (backtrace)
  (message ">>>>>>END SIGUSR1 handling.")
  ;; (message "Caught signal %S" last-input-event)
  ;; (view-lossage)
  )

(keymap-set special-event-map "<sigusr1>" 'sigusr-handler)

;;**** system/unix-signals/SIGUSR2 (USR2)

;; USR2 signal by default causes to enter the debugger
;; (keymap-set special-event-map "<sigusr2>" 'sigusr-handler)



(defun usr1-handler ()
  (interactive)
  (message "Got USR1 signal"))

(defun usr2-handler ()
  (interactive)
  (message "Got USR2 signal"))
;; USR2 signal by default causes to enter the debugger
;; (keymap-global-set "<signal> <usr2>" 'usr2-handler)

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

;;** system: undo

;; Once I lost the content of a big file
;; the undo did not have enough size to keep it.

;; default is 160 KB
;; 25 MB
(setq undo-limit 25000000)

;; default is 240 KB
;; 25 MB
(setq undo-strong-limit 25000000)

;; undo limit for one command
;; default is 24 MB
(setq undo-outer-limit 24000000)

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

;;* templates, snippets

;;** templates, snippets: tempo

(require 'tempo)

(defvar ram-tempo-org-template-tags nil
  "A template tag list for `tempo-use-tag-list' for `org-mode'.")

(defun ram-tempo-init-org-template-tags ()
  "Use specific to Org settings.

Collect tags in `ram-tempo-org-template-tags'.
Modify `tempo-match-finder'."
  ;; use tags for templates that start with "<"
  (setq-local tempo-match-finder "\\(<[[:word:]]+\\)\\=")
  (tempo-use-tag-list 'ram-tempo-org-template-tags))

(defvar ram-tempo-elisp-template-tags nil
  "A template tag list for `tempo-use-tag-list' for `emacs-lisp-mode'.")

(defun ram-tempo-init-elisp-template-tags ()
  "Use specific to emacs-lisp settings.

Collect tags in `ram-tempo-elisp-template-tags'.
Modify `tempo-match-finder'."
  ;; use tags for templates that start with "<"
  (setq-local tempo-match-finder "\\(<[[:word:]]+\\)\\=")
  (tempo-use-tag-list 'ram-tempo-elisp-template-tags))

(add-hook 'org-mode-hook #'ram-tempo-init-org-template-tags)

(add-hook 'emacs-lisp-mode-hook #'ram-tempo-init-elisp-template-tags)

;;*** templates, snippets/tempo: functions

(defun ram-get-prev-org-code-block-value (language)
  "Return the previous code block content written in LANGUAGE."
  (save-excursion
    (let ((block-content nil))
      (condition-case err
          ;; loop through previous code blocks
          (while (not block-content)
            ;; when called from a template when
            ;; the #+begin_src was already inserted
            ;; it will consider that it is inside
            ;; source block. We do not want that.
            (if (org-in-src-block-p 'INSIDE nil)
                (org-previous-block 2)
              (org-previous-block 1))
            (let ((el (org-element-at-point)))
              ;; search for code block in LANGUAGE
              (when (string-equal language (org-element-property :language el))
                ;; setting block-content and break the loop
                (setq block-content (org-element-property :value el)))))
        (user-error (if (string= (error-message-string err)
                                 "No previous code blocks")
                        (setq block-content "\n")
                      (signal (car err) (cdr err))))
        (error (signal (car err) (cdr err)))
        ;; (:success
        ;;  nil)
        )
      (or block-content "\n"))))

(defun ram-get-org-code-block-img-count ()
  "Get greatest number used as an image counter for Org code blocks.

Motivation:
:output graphics uses :file arg to store the image file.
Use this counter to distinguish each image output.

Map through all 'src-block Org Mode elements and find the
greatest number in there :file header arguments where :results
includes \"graphics\""
  (let* ((img-count 0)
         (parsed-buffer (copy-tree (let ((old-buffer (current-buffer)))
                                     (with-temp-buffer (insert-buffer-substring old-buffer)
                                                       (org-element-parse-buffer 'element nil nil)))))
         (org-file-args (when (eq 'org-mode major-mode)
                          (org-element-map
                              parsed-buffer
                              'src-block
                            (lambda (sb)
                              (let ((header-args (append (org-babel-parse-header-arguments
                                                          (org-element-property :parameters sb))
                                                         (org-babel-parse-header-arguments
                                                          (string-join (org-element-property :header sb) " ")))))
                                (and (assq :results header-args)
                                     (member "graphics" (string-split (cdr (assq :results header-args)) " "))
                                     (cdr (assq :file header-args))))
                              )
                            nil nil 'no-recursion nil))))
    (dolist (file org-file-args)
      (let* ((f (file-name-base file))
             (curr-img-count (string-to-number
                              (or (if (string-match "^[[:print:]]+?\\([0-9]+\\)$" f)
                                      (match-string 1 f))
                                  ""))))
        (when (>= curr-img-count img-count)
          (setq img-count curr-img-count))))
    img-count))

(defun ram-make-code-block-name ()
  "Make a block name.

Derive it from either:
  - org heading
  - org title
  - buffer-name"
  (let ((raw-name (or (when-let* ((heading
                                   (org-get-heading 'no-tags 'no-togos 'no-priority 'no-comment)))
                        (substring-no-properties (org-link-escape heading)))
                      (org-get-title)
                      (replace-regexp-in-string "[^[:alpha:]_-]" "" (file-name-base (buffer-name))))))

    (string-join (string-split (downcase raw-name) " " 'omit_nuls "[ \f\t\r\v]+") "_")))

(defun ram-org-prev-code-block-copy ()
  "Return a copy of the previous Org code block."
  (save-excursion
    (ram-org-previous-block 1)
    (let ((element (org-element-at-point)))
      (if (eq 'src-block (org-element-type element))
          (buffer-substring-no-properties (org-element-begin element)
                                          (org-element-end element))
        (ram-org-prev-code-block-copy)))))

;;*** templates, snippets/tempo: abbrevs

;; enable expansion of templates identified by their tags

;;*** templates, snippets/tempo: templates

;; load tempo templates
(when (file-readable-p "~/.emacs.d/lisp/ram-tempo-templates.el")
    (load-file (expand-file-name "~/.emacs.d/lisp/ram-tempo-templates.el")))

;;* sayid
;; (straight-use-package
;;  '(sayid :type git :flavor melpa :files ("src/el/*.el" "sayid-pkg.el") :host github :repo "clojure-emacs/sayid"))

;; (eval-after-load 'clojure
;;   '(sayid-setup-package))

;;* dired

;; (require 'dired)

(autoload 'dired-mode "dired")

;;** dired: settings

;; use the other dired window at the paste/move target
(setq dired-dwim-target t)

;; do not ask y/n
;; “always” means no asking
(setq dired-recursive-copies 'always)
;; “top” means ask once
(setq dired-recursive-deletes 'top)

;; search only file names
(setq dired-isearch-filenames 't)

;; search only file names when the cursor is on a file
;; (setq dired-isearch-filenames 'dwim)

;; update buffer when visiting again
(setq dired-auto-revert-buffer t)

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

;;** dired: functions

(defun ram-dired-find-dir ()
  "Find directory recursively."
  (interactive)
  (let ((start-dir (read-directory-name "Directory:"))
        (find-cmd-opts (format "-type d -name '%s'" (read-string "Search string: "))))
    (message "Ran (find-dired  %s %s)" start-dir find-cmd-opts)
    (find-dired start-dir find-cmd-opts)))

;;** dired: bindings

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

;; give make-hippie-expand-function returned fn a name
(defalias 'ram-hippie-expand-function (make-hippie-expand-function
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
(define-key global-map (kbd "M-/") #'ram-hippie-expand-function)

(defalias 'ram-hippie-expand-function-org (make-hippie-expand-function
                                           '(
                                             ram-try-expand-diacritical-marked
                                             try-expand-all-abbrevs
                                             try-expand-dabbrev-visible
                                             try-expand-dabbrev-all-buffers
                                             try-expand-dabbrev
                                             try-complete-file-name-partially
                                             try-complete-file-name
                                             try-expand-line) t))
(eval-after-load "org"
  '(define-key org-mode-map (kbd "M-/") #'ram-hippie-expand-function-org))

(defalias 'ram-hippie-expand-line (make-hippie-expand-function
                                   '(try-expand-line
                                     try-expand-line-all-buffers) t))
;; <f23> key is where 'c' key is
(define-key global-map (kbd "M-<f23> l") #'ram-hippie-expand-line)


;; (
;;  try-expand-list
;;  ;;  try-expand-dabbrev-from-kill
;;* custom

;;** custom: copy

;;*** custom/copy: line

;; credit to https://www.emacswiki.org/emacs/CopyingWholeLines
;; see the link for more #'ram-copy-line commands

(defun ram-copy-line (arg)
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
      (if (eq last-command 'ram-copy-line)
          (kill-append (buffer-substring beg end) (< end beg))
        (kill-ring-save beg end)))
    (kill-append "\n" nil)
    (beginning-of-line (or (and arg (1+ arg)) 2))
    (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))

(define-key global-map (kbd "<M-f16>") #'ram-copy-line)

;;** custom: insert

;;*** custom/insert: ram-up-list-insert-space

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

;;*** custom/insert: ram-insert-column

(defun ram-insert-column ()
  "Insert space followed by ':'."
  (interactive)
  (cond
   ((ram-in-string-p) (insert ":"))
   ((ram-in-comment-p) (insert ":"))
   ((memq (char-before) ram-open-delimiters) (insert ":"))
   ((memq (char-before) ram-open-delimiters) (insert ":"))
   ((= (char-before) ?:) (insert ":"))
   ((= (char-before) ?^) (insert ":"))
   ((or (= (char-before) 10)            ; beginning of line
        (= (char-before) 32))           ; white space
    (insert ":"))
   (t (insert " :"))))

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
