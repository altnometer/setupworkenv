;; packages.el --- Package configuration -*- lexical-binding: t -*-

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

(define-key global-map (kbd "s-b") #'switch-to-buffer)
(define-key global-map (kbd "s-B") #'switch-to-buffer-other-window)
(define-key global-map (kbd "C-s-b") 'display-buffer)
(define-key global-map (kbd "<M-f3>") #'ibuffer)

;;** bindings: file

(define-key global-map (kbd "C-s-f") #'find-file)
(define-key global-map (kbd "C-S-s-f") #'find-file-other-window)

(define-key ram-leader-map-tap-global (kbd "n") 'comint-dynamic-complete-filename)

(defun ram-edit-abbrev-file ()
  "Open ram-abbrev.el file. If it is current, close it."
  (interactive)
  (if (string= "ram-abbrev.el" (buffer-name (window-buffer (selected-window))))
      (if (= 1 (count-windows nil))
          (switch-to-prev-buffer)
        (delete-window))
    (find-file "~/.emacs.d/lisp/ram-abbrev.el")))

(define-key ram-leader-map-tap-global (kbd "a") #'ram-edit-abbrev-file)
(define-key ram-leader-map-tap-global (kbd "i") 'completion-at-point)
(define-key ram-leader-map-tap-global (kbd "w") #'widen)

(define-key global-map (kbd "C-x D") 'dired-other-window)

(define-key global-map (kbd "C-h C-k") #'describe-keymap)

;;** bindings: general

;; default "M-a" #'backward-sentence, only with #'push-mark
(define-key global-map (kbd "M-a") (lambda () (interactive) (push-mark) (backward-sentence)))
;; "M-(" was originally bound to #'insert-parentheses
(global-unset-key (kbd "M-("))
(define-key global-map (kbd "C-%") #'repeat)

;;** bindings: up-list

(defun ram-up-list (universal-arg)
  "Ignore strings when calling `up-list'."
  (interactive "p")
  (condition-case nil
      (let ((inside-str (nth 3 (syntax-ppss))))
        (if inside-str
            (up-list (+ 1 universal-arg) t t)
          (let ((space-between-closing-parens?
                 (save-excursion
                   (and (looking-at-p ")")
                        (progn
                          (backward-char)
                          (looking-at-p " "))
                        (progn
                          (backward-char)
                          (looking-at-p ")"))))))
            (if space-between-closing-parens?
                (progn (delete-char -1)
                       (message "feature working?")))
            (up-list universal-arg t t))
          (let ((between-closing-parens?
                 (save-excursion
                   (and (looking-at-p ")")
                        (progn
                          (backward-char)
                          (looking-at-p ")"))))))
            (if between-closing-parens?
                (insert ? )))))
    (error nil)))

(defun ram-up-list-backward (universal-arg)
  "Backward `up-list' that ignores strings."
  (interactive "p")
  (condition-case nil
      (let ((inside-str (nth 3 (syntax-ppss))))
        (if inside-str
            (up-list (- (+ 1 universal-arg)) t t)
          (up-list (- universal-arg) t t))
        ;; insert space
        (insert ? ))

    (error nil)))

(define-key global-map (kbd "s-t") 'ram-up-list)
(define-key global-map (kbd "s-c") 'ram-up-list-backward)

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
                 '((company-elisp company-dabbrev-code company-files)))))

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

(add-hook 'emacs-lisp-mode-hook (lambda () (outline-hide-sublevels 1)))
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

;;* shell

;; shell settings are based on
;; https://www.masteringemacs.org/article/running-shells-in-emacs-overview
(setq explicit-shell-file-name "/usr/bin/zsh")
(setq shell-file-name "zsh")
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(straight-use-package
 '(vterm :type git :flavor melpa :files
         ("*" (:exclude ".dir-locals.el" ".gitignore" ".clang-format" ".travis.yml") "vterm-pkg.el")
         :host github :repo "akermu/emacs-libvterm"))
(define-key vterm-mode-map (kbd "M-<f9>") #'vterm-copy-mode)
(define-key vterm-copy-mode-map (kbd "M-<f9>") #'vterm-copy-mode-done)
(require 'vterm)

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
        (,(kbd "C-g") . keyboard-quit)
        ([?\s-q] . kill-buffer-and-window)
        ;; ([?\s-Q] . save-buffers-kill-emacs)

        ([?\s-n] . switch-to-next-buffer)
        ([?\s-p] . switch-to-prev-buffer)
        ([?\s-f] . ram-choose-from-recentf)
        ([?\s-b] . switch-to-buffer)
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

        ([?\s-z] . exwm-layout-toggle-fullscreen)
        ([?\s-/] . exwm-layout-toggle-mode-line)
        ([?\s-y] . exwm-workspace-toggle-minibuffer)
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
        (,(kbd "<f2>") . (lambda () (interactive) (exwm-workspace-switch-create 2)))
        (,(kbd "<f10>") . (lambda () (interactive) (exwm-workspace-switch-create 0)))
        (,(kbd "<f6>") . (lambda () (interactive) (exwm-workspace-switch-create 6)))
        (,(kbd "<f7>") . (lambda () (interactive) (exwm-workspace-switch-create 7)))
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
              (exwm-layout-toggle-fullscreen)
              ;; (setq mode-line-format nil)
              ))))

(setq exwm-manage-configurations
      '(((member exwm-instance-name '("qutebrowser"))
         workspace 1)))

;; Line-editing shortcuts

(setq exwm-input-simulation-keys
      '(([?\C-b] . [left])
        ([?\C-f] . [right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])))

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
(define-key global-map (kbd "<f1>") (lambda () (interactive (exwm-workspace-switch-create 1))))
;; <f2> is a prefix to some '2C-mode commands
(define-key global-map (kbd "<f2>") nil)
(define-key global-map (kbd "<f2>") (lambda () (interactive (exwm-workspace-switch-create 2))))
(define-key global-map (kbd "<f10>") (lambda () (interactive (exwm-workspace-switch-create 0))))
(define-key global-map (kbd "<f6>") (lambda () (interactive (exwm-workspace-switch-create 6))))
(define-key global-map (kbd "<f7>") (lambda () (interactive (exwm-workspace-switch-create 7))))
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

(define-key global-map (kbd "s-m") 'magit-status)
(define-key global-map (kbd "s-M") 'magit-file-dispatch)
(define-key global-map (kbd "M-s-m") 'magit-dispatch)

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

(defun ram-describe-function (function &optional swap-history-p)
  "Describe function and store the search string and input to history."
  (interactive
   (let ((fn (function-called-at-point))
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
    (describe-function function)
    (setq history-add-new-input default)))

;; (define-key ram-leader-map-tap-global "v" #'ram-describe-function)
(define-key global-map (kbd "C-h f") #'ram-describe-function)

;;*** minibuffer/functions: ram-jump-to-outline

(defvar ram-jump-to-outline-history nil "History for outlines to jump to.")
(put 'ram-jump-to-outline-history 'history-length 20)

(defun ram-jump-to-outline (outline &optional swap-history-p)
  "Jump to outline."
  (interactive
   (let ((headlines '())
         (headline-regex
          ;; TODO: use 'outline-regexp and copy the line
          (cond
           ((eq major-mode 'org-mode)
            "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$")
           ((eq major-mode 'python-mode)
            ;; (group
            ;;    (* space)                 ; 0 or more spaces
            ;;    (one-or-more (syntax comment-start))
            ;;    (one-or-more space)
            ;;    ;; Heading level
            ;;    (group (repeat 1 8 "\*")) ; Outline stars
            ;;    (one-or-more space)
            ;;    )
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
            "^;;\\(?:;\\(;[^#]\\)\\|\\(\\*+\\)\\)\\(?: +\\(.*?\\)\\)?[ ]*$")))
         (old-binding (cdr (assoc 'return minibuffer-local-completion-map)))
         (hist-item (car ram-jump-to-outline-history)))
     (define-key minibuffer-local-completion-map (kbd "<return>")
       (ram-add-to-history-cmd ram-add-to-jump-to-outline-history 'ram-jump-to-outline-history))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward headline-regex nil t)
         (cl-pushnew (match-string 3) headlines)))
     (setq val (completing-read
                (format-prompt
                 "Find heading" (car ram-jump-to-outline-history))
                headlines
                nil t nil
                'ram-jump-to-outline-history
                (car ram-jump-to-outline-history)))
     (define-key minibuffer-local-completion-map (kbd "<return>") old-binding)
     (list val
           ;; if two items are inserted, swap them so that the search str is first
           (let ((third-element (caddr ram-jump-to-outline-history))
                 (second-element (cadr ram-jump-to-outline-history)))
             (and second-element (equal hist-item third-element))))))

  ;; reorder history so that the search string is fist and the input is second.
  (when swap-history-p
    (setq ram-jump-to-outline-history
          (cons (cadr ram-jump-to-outline-history)
                (cons (car ram-jump-to-outline-history)
                      (cddr ram-jump-to-outline-history)))))
  (let ((default case-fold-search)
        (case-fold-search nil))
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward (format "^[^\r\n[:alpha:]]+%s$" outline) nil t))
      (push-mark)
      (goto-char (match-beginning 0))
      (outline-show-entry)
      (outline-show-branches))
    (setq case-fold-search default)))

(eval-after-load "org"
  '(define-key org-mode-map (kbd "<M-f5>") 'ram-jump-to-outline))
(define-key emacs-lisp-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)

;;*** minibuffer/functions: ram-jump-to-def

(defvar ram-jump-to-def-history nil "History for definitions to jump to.")
(put 'ram-jump-to-def-history 'history-length 10)

(defun ram-jump-to-def-get-regexs (major-mode name-regex)
  "Return regex to capture definitions for MAJOR-MODE."
  (cond
   ((eq major-mode 'emacs-lisp-mode)
    (format "^(\\(def\\(?:un\\|var\\|ine-minor-mode\\) +%s\\)" name-regex))
   (t (message (format "%s is not supported, add regex to `ram-jump-to-def'") major-mode))))

(defun ram-jump-to-def (outline &optional swap-history-p)
  "Jump to def."
  (interactive
   (let ((defs '())
         (def-regex (ram-jump-to-def-get-regexs major-mode "\\([-[:alnum:]]+\\)"))
         (old-binding (cdr (assoc 'return minibuffer-local-completion-map)))
         (hist-item (car ram-jump-to-def-history))
         (s (thing-at-point 'symbol)))
     (define-key minibuffer-local-completion-map (kbd "<return>")
       (ram-add-to-history-cmd ram-add-to-jump-to-outline-history 'ram-jump-to-def-history))
     (save-excursion
       (goto-char (point-min))
       (while (re-search-forward def-regex nil t)
         (cl-pushnew (match-string 1) defs)))
     (setq val (icomplete-vertical-do ()
                 (completing-read
                  (if s
                      (format-prompt "Jump to def" s)
                    (format-prompt "Jump to def" nil))
                 defs
                 nil t nil
                 'ram-jump-to-def-history
                 s
                 ;; (car ram-jump-to-def-history)
                 )))
     (define-key minibuffer-local-completion-map (kbd "<return>") old-binding)
     (list val
           ;; if two items are inserted, swap them so that the search str is first
           (let ((third-element (caddr ram-jump-to-def-history))
                 (second-element (cadr ram-jump-to-def-history)))
             (and second-element (equal hist-item third-element))))))

  ;; reorder history so that the search string is fist and the input is second.
  (when swap-history-p
    (setq ram-jump-to-def-history
          (cons (cadr ram-jump-to-def-history)
                (cons (car ram-jump-to-def-history)
                      (cddr ram-jump-to-def-history)))))
  (let ((default case-fold-search)
        (def-regex (format "^[^\r\n[:alpha:]]+%s" outline))
        (case-fold-search nil))
    (when (save-excursion
            (goto-char (point-min))
            (re-search-forward def-regex nil t))
      (push-mark)
      (goto-char (match-beginning 0))
      (recenter))
    (setq case-fold-search default)))

(define-key emacs-lisp-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)

;;** minibuffer: bindings

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

(straight-use-package
 '(icomplete-vertical :type git :flavor melpa :host github :repo "oantolin/icomplete-vertical"))
(setq icomplete-vertical-prospects-height (/ (frame-height) 6))
(icomplete-vertical-mode -1)

;; credit to https://gitlab.com/protesilaos/dotfiles/-/blob/e8d6268866fb77c0aec6a6b68c9e7183daa65347/emacs/.emacs.d/emacs-init.org

(defun prot/kill-ring-yank-complete ()
    "Insert the selected `kill-ring' item directly at point.
When region is active, `delete-region'.

Sorting of the `kill-ring' is disabled.  Items appear as they
normally would when calling `yank' followed by `yank-pop'."
    (interactive)
    (let ((kills                    ; do not sort items
           (lambda (string pred action)
             (if (eq action 'metadata)
                 '(metadata (display-sort-function . identity)
                            (cycle-sort-function . identity))
               (complete-with-action
                action kill-ring string pred)))))
      (icomplete-vertical-do
          (:separator 'dotted-line :height (/ (frame-height) 4))
        (when (use-region-p)
          (delete-region (region-beginning) (region-end)))
        (insert
         (completing-read "Yank from kill ring: " kills nil t)))))

(define-key global-map (kbd "C-s-y") #'prot/kill-ring-yank-complete)
(define-key icomplete-minibuffer-map (kbd "C-v") #'icomplete-vertical-toggle)

;;**** minibuffer/completion/icomplete: minibuffer actions

;; check Omar Antolín Camarena's "embark"
;; library: https://github.com/oantolin/embark for more

(defmacro prot/minibuffer-completion-act (name doc &rest body)
  `(defun ,name ()
     ,doc
     (interactive)
     (let ((candidate (car completion-all-sorted-completions)))
       (when (and (minibufferp)
                  (bound-and-true-p icomplete-mode))
         ,@body))))

(prot/minibuffer-completion-act
 prot/minibuffer-kill-completion
 "Place minibuffer candidate to the top of the `kill-ring'."
 (kill-new `,candidate)
 (message "Copied %s to kill-ring" (propertize `,candidate 'face 'success)))

(prot/minibuffer-completion-act
 prot/minibuffer-insert-completion
 "Insert minibuffer candidate in last active window."
 (with-minibuffer-selected-window (insert `,candidate)))

(prot/minibuffer-completion-act
 prot/minibuffer-insert-completion-exit
 "Like `prot/minibuffer-insert-completion' but exit minibuffer."
 (prot/minibuffer-insert-completion)
 (top-level))

;;**** minibuffer/completion/icomplete: bindings

(define-prefix-command 'prot/minibuffer-completion-map)

(define-key prot/minibuffer-completion-map (kbd "w") 'prot/minibuffer-kill-completion)
(define-key prot/minibuffer-completion-map (kbd "i") 'prot/minibuffer-insert-completion)
(define-key prot/minibuffer-completion-map (kbd "j") 'prot/minibuffer-insert-completion-exit)

(define-key minibuffer-local-completion-map (kbd "M-o") prot/minibuffer-completion-map)

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

(define-key global-map (kbd "s-l") #'prot/focus-minibuffer-or-completions)

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
        '(orderless-regexp
          ;; orderless-initialism
          ))

  (defun prot/orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot/orderless-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

  (setq orderless-style-dispatchers
        '(prot/orderless-literal-dispatcher
          prot/orderless-initialism-dispatcher))
  (add-hook 'minibuffer-exit-hook
            'orderless-remove-transient-configuration))

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

(straight-use-package
 '(org :type git :repo "https://code.orgmode.org/bzg/org-mode.git" :local-repo "org"))

;;** org-mode: functions

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

(define-key global-map (kbd "C-h a") 'apropos)

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

  (define-key org-mode-map (kbd "C-c C-p") (lambda (arg) (interactive "p")
                                             (ram-push-mark-for-none-consecutive-cmd arg #'org-previous-visible-heading)))
  (define-key org-mode-map (kbd "C-c C-n") (lambda (arg) (interactive "p")
                                             (ram-push-mark-for-none-consecutive-cmd arg #'org-next-visible-heading)))

  (define-key org-mode-map (kbd "<M-f6>") 'org-previous-visible-heading)
  (define-key org-mode-map (kbd "<M-S-f6>") 'org-backward-heading-same-level)

  (define-key org-mode-map (kbd "C-c z") 'ram-org-hide-block-toggle-all)
  ;; originally bound to 'org-table-copy-down
  (define-key org-mode-map (kbd "<S-return>") 'newline-and-indent)
  (define-key org-mode-map (kbd "M-h") (lambda () (interactive) (push-mark) (org-mark-element))))

;;** org-mode common settings

(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-hide-block-all)
(add-hook 'org-mode-hook (lambda () (variable-pitch-mode t)))

;; modify org-emphasis-regexp-components, 3rd entry, to include char to emphasis markup
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
(with-eval-after-load "org"
  (setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\""))

(setq org-hide-emphasis-markers t)
;; setting this to nil "unhides" the emphasis markers
;; (setq org-descriptive-links nil)
(setq org-src-window-setup 'current-window)
(setq org-hide-leading-stars nil)
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-return-follows-link t)
(setq org-imenu-depth 7)

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
               '("n" . "name"))
  (add-to-list 'org-structure-template-alist
               '("hd" . "header")))

;;** org-mode: org-babel

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
     ;; (scribble . t)
     (css . t))))

;;*** org-mode/org-babel: org-babel-eval-in-repl

(straight-use-package
 '(org-babel-eval-in-repl :type git :flavor melpa :host github :repo "diadochos/org-babel-eval-in-repl"))

(with-eval-after-load "ob"
  (require 'eval-in-repl-racket)
  (require 'org-babel-eval-in-repl)
  (define-key ram-leader-map-tap-org (kbd "e") 'ober-eval-block-in-repl)
  (define-key ram-leader-map-tap-org (kbd "E") 'ober-eval-in-repl)
  )

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


;;* outline

;;** outline: setup

(outline-minor-mode t)

(with-eval-after-load "outline"
  (require 'foldout))

;;** outline: bicycle
(straight-use-package
 '(bicycle :type git :flavor melpa :host github :repo "tarsius/bicycle"))

(add-hook 'emacs-lisp-mode-hook #'outline-minor-mode)
(add-hook 'clojure-mode-hook #'outline-minor-mode)

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
      (foldout-exit-fold arg)
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

;;** outline: bindings

(with-eval-after-load "outline"
  (define-key outline-minor-mode-map (kbd "<tab>") #'bicycle-cycle))

(define-key ram-leader-map-tap-global (kbd "n") #'outline-next-visible-heading)
(define-key ram-leader-map-tap-global (kbd "p") #'outline-previous-visible-heading)
(define-key ram-leader-map-tap-global (kbd "f") #'outline-forward-same-level)
(define-key ram-leader-map-tap-global (kbd "b") #'outline-backward-same-level)
(define-key ram-leader-map-tap-global (kbd "o") #'outline-show-all)
(define-key ram-leader-map-tap-global (kbd "q") #'ram-outline-hide-all)
(define-key ram-leader-map-tap-global (kbd "u") #'outline-up-heading)
(define-key ram-leader-map-tap-global (kbd "d") #'prot/outline-down-heading)
(define-key ram-leader-map-tap-global (kbd "z") #'ram-toggle-narrow-outline-heading)

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

;; breaks lispy-mode key bindings
;; (add-hook 'racket-mode-hook #'racket-unicode-input-method-enable)

(with-eval-after-load 'racket-mode
  (define-key racket-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun))

;;** racket: repl

(with-eval-after-load "racket-repl"
  (define-key racket-repl-mode-map (kbd "<f2>") #'racket-repl-submit))

;;* lispy
(straight-use-package 'lispy)

(defun conditionally-enable-lispy ()
  (when (eq this-command 'eval-expression)
    (lispy-mode 1)))

(add-hook 'lisp-interaction-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'minibuffer-setup-hook 'conditionally-enable-lispy)
(add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
(add-hook 'racket-repl-mode-hook (lambda () (lispy-mode 1)))

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
			  (t (1- org-inlinetask-min-level))))))
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
	   (line-end-position 0) (line-end-position) nil))))))

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
         (lispy-define-key map "p" 'lispy-eval-other-window)
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

(with-eval-after-load "lispy"
  (define-key lispy-mode-map
    (kbd "(")
    (lambda () (interactive) (expand-abbrev) (lispy-parens current-prefix-arg) ())))

;;* avy

;; (use-package avy
;;   :defer t
;;   :config
;;   ;; apply avy to all windows?
;;   (setq avy-all-windows nil)
;;   ;; avy-lead-face-0
;;   ;; Dim all windows when displaying overlay for targets
;;   (setq avy-background t)
;;   (setq avy-timeout-seconds 0.4)
;;   ;; (setq highlight-first t)
;;   ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-keys-alist#avy-keys
;;   (setq avy-keys '(?s ?a ?r ?e ?t ?i ?u ?n ?o ?p ?l ?m ?f ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d))
;;   ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
;;   (setq avy-style 'at-full)
;;   (setq avy-styles-alist '(
;;                            ;; (avy-goto-char-2 . post)
;;                            (avy-goto-word-1 . at-full)
;;                            ;; (avy-goto-char-2 . pre)
;;                            (avy-goto-char-2 . at-full)
;;                            (avy-goto-char-timer . at-full)
;;                            (ram-avy-goto-subword-2 . at-full)
;;                            (ram-avy-goto-paragraph-start . post)))
;;   ;; (setq avy-keys (nconc
;;   ;;                     (number-sequence ?1 ?9)
;;   ;;                     '(?0)))
;;   (setq avy-orders-alist
;;         '((avy-goto-char . avy-order-closest)
;;           (avy-goto-char-2 . avy-order-closest)
;;           (avy-goto-word-1 . avy-order-closest)
;;           (lispy-ace-paren . ram-avy-order-furthest)
;;           (lispy-ace-char . avy-order-closest)
;;           (lispy-ace-symbol . avy-order-closest)
;;           (lispy-ace-subword . avy-order-closest)
;;           (ram-avy-goto-subword-2 . avy-order-closest)
;;           (ram-avy-goto-paragraph-start . ram-avy-order-furthest)))
;;   (setq avy-dispatch-alist
;;         '((?- . avy-action-kill-move)
;;           (?! . avy-action-kill-stay)
;;           (?\' . avy-action-teleport)
;;           (?` . avy-action-mark)
;;           (?/ . avy-action-copy)
;;           (?? . avy-action-yank)
;;           (?. . avy-action-ispell)
;;           (?# . avy-action-zap-to-char))))

(straight-use-package
 '(avy :type git :flavor melpa :host github :repo "abo-abo/avy"))

(with-eval-after-load 'avy
  (setq avy-all-windows nil)
  ;; avy-lead-face-0
  ;; Dim all windows when displaying overlay for targets
  (setq avy-background t)
  (setq avy-timeout-seconds 0.4)
  ;; (setq highlight-first t)
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-keys-alist#avy-keys
  (setq avy-keys '(?s ?a ?r ?e ?t ?i ?u ?n ?o ?p ?l ?m ?f ?h ?c ?g ?x ?b ?z ?w ?y ?v ?q ?j ?k ?d))
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
  (setq avy-style 'at-full)
  (setq avy-styles-alist '(
                           ;; (avy-goto-char-2 . post)
                           (avy-goto-word-1 . at-full)
                           ;; (avy-goto-char-2 . pre)
                           (avy-goto-char-2 . at-full)
                           (avy-goto-char-timer . at-full)
                           (ram-avy-goto-subword-2 . at-full)
                           (ram-avy-goto-paragraph-start . post)))

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

  (setq avy-dispatch-alist
        '((?- . avy-action-kill-move)
          (?! . avy-action-kill-stay)
          (?\' . avy-action-teleport)
          (?` . avy-action-mark)
          (?/ . avy-action-copy)
          (?? . avy-action-yank)
          (?. . avy-action-ispell)
          (?# . avy-action-zap-to-char))))

;;** avy custom commands
;; https://github.com/abo-abo/avy/wiki/custom-commands
(eval-after-load "avy"
  '(progn
     (defvar ram-avy--overlays-back nil
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
       (mapc #'delete-overlay ram-avy--overlays-back)
       (setq ram-avy--overlays-back nil)
       ;; (avy--remove-leading-chars)
       )

     (defun ram-avy--keyboard-advice (fn &rest args)
       (unwind-protect
           (apply fn args)
         (when ram-avy--overlays-back
           (ram-avy--done))))

     (advice-add 'keyboard-quit :around #'ram-avy--keyboard-advice)
     ;; (advice-add 'minibuffer-keyboard-quit :before #'ram-avy--done)

     (defun ram-avy--make-backgrounds ()
       "Create dim a background overlay for selected window"
       (when avy-background
         (setq ram-avy--overlays-back
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
       (avy-with ram-avy-goto-subword-2
         (let ((char1 (downcase char1))
               (char2 (downcase char2)))
           (avy-goto-subword-0
            arg (lambda ()
                  (and (char-after)
                       (eq (downcase (char-after)) char1)
                       (if (eq char2 ?*)
                           t
                         (save-excursion (forward-char) (eq (downcase (char-after)) char2)))))))))
     ;; (add-to-list 'avy-styles-alist '(ram-avy-goto-subword-2 . at-full))
     ;; (add-to-list 'avy-orders-alist '(ram-avy-goto-subword-2 . avy-orders-closest))

     (defun avy-goto-top-paren ()
       (interactive)
       (avy--generic-jump "^(" nil))

     (defun avy-goto-paragraph-start ()
       (interactive)
       (avy--generic-jump "\n\n[ \t]*[[:graph:]]" nil))

     (defun ram-avy-goto-paragraph-start ()
       (interactive)
       (ram-avy--make-backgrounds)
       (avy-with ram-avy-goto-paragraph-start
         (avy-goto-paragraph-start)
         (re-search-forward "[[:graph:]]" nil t 1)
         (backward-char))
       (ram-avy--done))
     (add-to-list 'avy-styles-alist '(ram-avy-goto-paragraph-start . post))
     (add-to-list 'avy-orders-alist '(ram-avy-goto-paragraph-start . ram-avy-order-furthest))

     (defun ram-with-backgound ()
       (interactive)
       (ram-avy--make-backgrounds)
       (ram-avy-goto-subword-2))))

;;** avy keybindings

(defun ram-avy-goto-subword-2-dim ()
  "Dim window before Invoking `ram-avy-goto-subword-2'."
  (interactive)
  (ram-avy--make-backgrounds)
  (call-interactively #'ram-avy-goto-subword-2)
  (ram-avy--done))

(eval-after-load "avy"
  '(progn
     ;; (define-key global-map (kbd "s-s") (lambda () (interactive)
     ;;                                      (ram-avy--make-backgrounds)
     ;;                                      (avy-goto-word-or-subword-1)
     ;;                                      (ram-avy--done)))
     (define-key global-map (kbd "s-s") #'ram-avy-goto-subword-2-dim)

     ;; (define-key global-map (kbd "s-s") 'avy-goto-char-2)
     (define-key global-map (kbd "s-r") 'ram-avy-goto-paragraph-start)
     (define-key global-map (kbd "s-R") 'avy-goto-top-paren)
     ;; (define-key global-map (kbd "s-d") 'avy-goto-char-in-line)
     (define-key global-map (kbd "s-N") 'avy-resume)
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

;;* brackets, parentheses, parens

;;** ram-highlight-sexps

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
  (define-key clojure-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  (define-key clojure-mode-map (kbd "s-E") #'ram-switch-to-clojure-repl)
  ;; (require 'flycheck-clj-kondo)
  )

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
  (define-key python-mode-map (kbd "<M-f5>") 'ram-jump-to-outline)
  ;; (define-key python-mode-map (kbd "<M-S-f5>") 'ram-jump-to-def)
  ;; (define-key python-mode-map (kbd "<M-f19>") #'ram-toggle-narrow-to-defun)
  ;; (require 'flycheck-clj-kondo)
  )

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
  (add-to-list 'super-save-triggers 'ram-edit-abbrev-file))

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
(setq enable-recursive-minibuffers t)
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

;;** mode-line: timer

;; (cancel-timer mode-line-timer)
(run-at-time 1 nil '(lambda () (force-mode-line-update t)))
(defvar mode-line-timer
  (run-with-timer 2 4 #'(lambda () (force-mode-line-update t))))
(defvar mode-line-cpu-temp-timer
  (run-with-timer 2 3 #'ram-get-cpu-temp))
(defvar mode-line-memory-stats-timer
  (run-with-timer 2 3.1 #'ram-get-memory-stats))

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

(defvar sh-memory-query-proc (make-process
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
            (match-string-no-properties 1 vc-mode))


           ;;  ;; Otherwise, indicate confusion
           ;;  (t
           ;;   "?")
           ))

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
               '((((class color) (min-colors 88))
                  ;; :box (:line-width 2 :color "grey55" :style nil)
                  :box nil
                  :weight light
                  :background "grey55" :foreground "black" :height 155)))

(face-spec-set 'mode-line-inactive
               '(;; (default
                 ;;   :inherit mode-line)
                 (((class color) (min-colors 88))
                  ;; :box (:line-width 2 :color "grey90" :style nil)
                  :box nil
                  :weight light
                  :foreground "grey20" :background "grey90" :height 155)))


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
                                       'face '((:foreground "green4")))
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
                 ;; display only if frame is right of other frame
                 (when (not (= 0 (car (frame-position (window-frame (get-buffer-window))))))
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
                       (format "%s%s%s %s%s "
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

;;* themes: doom
;; (straight-use-package
;;  '(doom-themes :type git :flavor melpa :files (:defaults "themes/*.el" "doom-themes-pkg.el") :host github :repo "hlissner/emacs-doom-themes"))
;; Global settings (defaults)
;; (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;       doom-themes-enable-italic t)
                                        ; if nil, italics is universally disabled
;; (load-theme 'doom-one t)

;; Corrects (and improves) org-mode's native fontification.
;; (doom-themes-org-config)

;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

;;* themes: sanityinc-tomorrow-night
;; check enabled themes in custom-enabled-themes
(straight-use-package
 '(color-theme-sanityinc-tomorrow :type git :flavor melpa :host github :repo "purcell/color-theme-sanityinc-tomorrow"))
;; (load-theme 'sanityinc-tomorrow-night t nil)
;; (load-theme 'sanityinc-tomorrow-day t)

;;* themes: modus-vivendi-theme
(straight-use-package
 '(modus-operandi-theme :type git :flavor melpa :files
                       ("modus-operandi-theme.el" "modus-operandi-theme-pkg.el") :host gitlab :repo "protesilaos/modus-themes"))

;; (load-theme 'modus-vivendi t)
(progn
  ;; (setq modus-operandi-theme-diffs nil)
  (setq modus-operandi-theme-diffs 'desaturated)
  ;; (setq modus-operandi-theme-diffs 'fg-only)
 (load-theme 'modus-operandi t))

;;* search

;;** search: functions

(defun prot/find-file-vc-or-dir (&optional arg)
    "Find file by name that belongs to the current project or dir.
With \\[universal-argument] match files by contents.  This
requires the command-line executable called 'rg' or 'ripgrep'."
    (interactive "P")
    (let* ((default-directory (file-name-directory
                               (or (locate-dominating-file "." ".git" )
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
(define-key isearch-mode-map (kbd "<C-return>")  'contrib/isearch-done-opposite-end )

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

(define-key isearch-mode-map (kbd "[") #'ram-isearch-done-to-open-paren)
(define-key isearch-mode-map (kbd "]") #'ram-isearch-done-to-close-paren)

;;* packages


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

(add-hook 'imenu-after-jump-hook #'prot/imenu-recenter-pulse)
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
(define-key global-map (kbd "M-s-d") #'iy-go-to-or-up-to-continue-backward)


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

;;** packages: recentf

(setq recentf-auto-cleanup 'never) ;; disable before we start recentf!
(recentf-mode 1)
(setq recentf-max-saved-items 4000)
(run-at-time nil (* 1 60) 'recentf-save-list)

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
  (let ((f (icomplete-vertical-do ()
             (completing-read "Recent files: "
                              recentf-list nil t))))
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

;;* Spelling

;;** flycheck

;;*** flycheck settings

;; credit to https://joelkuiper.eu/spellcheck_emacs
(dolist (hook '(text-mode-hook
                org-mode))
  (add-hook hook (lambda () (flyspell-mode 1))))

(dolist (hook '(emacs-lisp-mode-hook
                clojure-mode-hook
                python-mode-hook
                js-mode-hook))
  (add-hook hook
            '(lambda ()
               (flyspell-prog-mode))))

(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word))

;;*** flycheck bindings

(with-eval-after-load "flyspell"
  (define-key flyspell-mode-map (kbd "C-,") nil)
  (define-key global-map (kbd "s-M-c") 'ispell-word)

  (define-key global-map (kbd "s-M-C") 'flyspell-check-next-highlighted-word)

  ;; hyper key is disables in favor of f1, ... keys
  ;; (define-key global-map (kbd "C-H-e") 'flycheck-next-error)
  ;; (define-key global-map (kbd "C-H-E") 'flycheck-previous-error)
  )

;;*** flyspell-goto-previous-error

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

;;* System

;;** system: comments

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

(define-key global-map (kbd "s-v") #'ram-next-comment)
(define-key global-map (kbd "s-k") #'ram-previous-comment)

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
        (if (> (x-display-pixel-width) 2000)
            (progn
              (set-frame-parameter frame 'font "Operator Mono Medium-19")
              (custom-set-faces
               '(font-lock-comment-face ((t (:family "Operator Mono Light-19")))))
              (custom-set-faces
               '(font-lock-string-face ((t (:family "Operator Mono Light-19" :slant italic))))))
          (progn
            (set-frame-parameter frame 'font "Operator Mono Medium-12")
            (custom-set-faces
             '(font-lock-comment-face ((t (:family "Operator Mono Light-12")))))
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

(set-face-attribute 'fixed-pitch nil :font "FiraCode-18")

;; (set-face-attribute 'variable-pitch nil :font "FiraGo-18")

;; (set-face-attribute 'variable-pitch nil :font "Calibri-20")
(set-face-attribute 'variable-pitch nil :font "Verdana-19")

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
(setq debug-on-error t)
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

;; (setq nlinum-highlight-current-line t)
;; (setq-default display-line-numbers t)
;; highlght the current line only in gui.
;; (when window-system (global-hl-line-mode t))

;; (global-hl-line-mode)

;; do not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; highlights matching parens
;; disable in favor of mic-paren

;; (show-paren-mode 1)
(setq show-paren-delay 0)
(setq show-paren-style 'expression)
(setq show-paren-style 'parenthesis)
(set-face-background 'show-paren-match "#42444a")

;; (set-face-attribute 'show-paren-match nil
;;                     :foreground nil
;;                     :weight 'normal :underline nil :overline nil :slant 'normal)

;; Default Browser
(setq browse-url-browser-function 'browse-url-generic
    browse-url-generic-program "qutebrowser")
;; kill line and newline char
(setq kill-whole-line t)

;;*** system/general settings: ring-bell

(setq visible-bell nil)
(setq ring-bell-function #'ignore)

;;*** system/general settings: language

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;*** system/general settings: *scratch*

(defun ram-switch-to-scratch ()
  (interactive)
  (switch-to-buffer "*scratch*"))

(define-key ram-leader-map-tap-global (kbd "'") 'ram-switch-to-scratch)

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

;;** system: syntax

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
                                         (modify-syntax-entry ?_ "w" python-mode-syntax-table)
                                         ;; (modify-syntax-entry ?- "w" python-mode-syntax-table)
                                         )))

;;** system: whitespace

(setq-default
 show-trailing-whitespace t)
(add-hook 'cider-test-report-mode-hook '(lambda () (setq-default show-trailing-whitespace nil)))
(add-hook 'buffer-menu-mode-hook '(lambda () (setq-default show-trailing-whitespace nil)))
(add-hook 'minibuffer-setup-hook '(lambda () (setq-default show-trailing-whitespace nil)))
(add-hook 'eshell-mode-hook '(lambda () (setq-default show-trailing-whitespace t)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
(define-key emacs-lisp-mode-map (kbd "<S-return>") 'newline-and-indent)

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
(setq dired-recursive-copies (quote always))
;; “top” means ask once
(setq dired-recursive-deletes (quote top))

(setq dired-isearch-filenames 'dwim)

;;** dired bindings

;; default behavior would create a new buffer for each visited dir, change it with the following:
;; was dired-advertised-find-file
;; (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; was dired-up-directory
;; (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

(global-set-key (kbd "C-c j") 'dired-jump)

;;* hippie

(define-key global-map (kbd "M-/") (make-hippie-expand-function
                                    '(
                                      ;; try-expand-all-abbrevs
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

;;** custom: replace-or-delete-pair

;; credit to https://emacs.stackexchange.com/a/37648
(defun yf/replace-or-delete-pair (open)
  "Replace pair at point by OPEN and its corresponding closing character.
The closing character is lookup in the syntax table or asked to
the user if not found."
  (interactive
   (list
    (read-char
     (format "Replacing pair %c%c by (or hit RET to delete pair):"
             (char-after)
             (save-excursion
               (forward-sexp 1)
               (char-before))))))
  (if (memq open '(?\n ?\r))
      (delete-pair)
    (let ((close (cdr (aref (syntax-table) open))))
      (when (not close)
        (setq close
              (read-char
               (format "Don't know how to close character %s (#%d) ; please provide a closing character: "
                       (single-key-description open 'no-angles)
                       open))))
      (yf/replace-pair open close))))

(defun yf/replace-pair (open close)
  "Replace pair at point by respective chars OPEN and CLOSE.
If CLOSE is nil, lookup the syntax table. If that fails, signal
an error."
  (let ((close (or close
                   (cdr-safe (aref (syntax-table) open))
                   (error "No matching closing char for character %s (#%d)"
                          (single-key-description open t)
                          open)))
        (parens-require-spaces))
    (insert-pair 1 open close))
  (delete-pair)
  (backward-char 1))

(define-key ram-leader-map-tap-global (kbd "/") 'yf/replace-or-delete-pair)

;;** custom: jump to the end of top level sexp

(defun ram-jump-to-last-bracket (&optional direction)
  "Jump to the end of top level sexp."
  (interactive)
  (let* ((p (point))
         (direction (or direction 1))
         (beginning-of-defun-p (save-excursion
                                 (end-of-defun -1)
                                 (beginning-of-defun -1)
                                 (= p (point)))))
    (condition-case nil
        (cond
         ((and (= -1 direction) beginning-of-defun-p) (beginning-of-defun) (forward-sexp))
         (beginning-of-defun-p (forward-sexp))
         ((= -1 direction) (beginning-of-defun 2) (forward-sexp))
         (t
          (beginning-of-defun)
          (forward-sexp)
          (when (>= p (point))
            (beginning-of-defun -1)
            (forward-sexp))))
      (error nil))))

;; "C-M-e" bound to end-of-defun by default
(define-key global-map (kbd "<M-f2>") (lambda () (interactive) (push-mark) (ram-jump-to-last-bracket)))
(define-key global-map (kbd "<M-S-f2>") (lambda () (interactive) (push-mark) (ram-jump-to-last-bracket -1)))
(define-key global-map (kbd "<M-f1>") (lambda () (interactive) (push-mark) (beginning-of-defun)))
(define-key global-map (kbd "<M-S-f1>") (lambda () (interactive) (push-mark) (beginning-of-defun -1)))
