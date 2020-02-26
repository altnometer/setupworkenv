(defvar my-term-shell "/usr/bin/zsh")
(defadvice ansi-term (before force-bash)
  (interactive (list my-term-shell)))
(ad-activate 'ansi-term)

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; Where to display *Help* buffer

;  (add-to-list 'display-buffer-alist
               ;; '("*Help*" display-buffer)
               ;; '("*Help*"
               ;;   (display-buffer-in-side-window)
               ;;   (inhibit-same-window . t)
               ;;   (window-height . 0.4))
               ;; '("\\*help"
               ;;   (display-buffer-reuse-window display-buffer-use-some-window display-buffer-in-side-window)
               ;;   (side . right)
               ;;   (inhibit-same-window . t)
               ;;   (window-width . 0.5))
 ;              )
  (add-to-list 'display-buffer-alist
                    `(,(rx bos "*helm" (* not-newline) "*" eos)
                         (display-buffer-in-side-window)
                         (inhibit-same-window . t)
                         (window-height . 0.4)))

;; use "y", "n" for confermations requiring "yes", "no"
(fringe-mode '(20 . 20))
(defalias 'yes-or-no-p 'y-or-n-p)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (global-linum-mode t)
(setq inhibit-startup-message t)
(setq scroll-corservatively 100)
(setq ring-bell-function 'ignore)
(setq nlinum-highlight-current-line t)
;; highlght the current line only in gui.
;; (when window-system (global-hl-line-mode t))
(global-hl-line-mode t)
;; do not create backup files
(setq make-backup-files nil)
(setq auto-save-default nil)
;; highlights matching parens
(show-paren-mode 1)
;; Default Browser
(setq browse-url-browser-function 'browse-url-generic
  browse-url-generic-program "qutebrowser")
;; kill line and newline char
(setq kill-whole-line t)

(setq blink-cursor-mode nil)
(setq blink-cursor-blinks 1)

(setq-default indent-tabs-mode nil)
(add-hook 'clojure-mode-hook
    (lambda () (setq-local evil-shift-width 2)))
(add-hook 'emacs-lisp-mode-hook
    (lambda () (setq-local evil-shift-width 2)))
(add-hook 'lisp-interaction-mode-hook
    (lambda () (setq-local evil-shift-width 2)))

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
(add-hook 'python-mode-hook (lambda () (
                                        (progn
                                          (modify-syntax-entry ?_ "w" python-mode-syntax-table)
                                          ;; (modify-syntax-entry ?- "w" python-mode-syntax-table)
                                          ))))

(setq-default display-line-numbers t)

(setq auto-revert-use-notify nil
      auto-revert-interval 1
      auto-revert-check-vc-info t)

(global-unset-key (kbd "M-("))
;; https://stackoverflow.com/a/6037523
(defun beginning-of-line-or-indentation ()
  "move to beginning of line, or indentation"
  (interactive)
  (if (bolp)
      (back-to-indentation)
    (beginning-of-line)))
(global-set-key (kbd "C-a") #'beginning-of-line-or-indentation)
;; (eval-after-load "cc-mode"
;;   '(define-key c-mode-base-map (kbd "C-a") 'beginning-of-line-or-indentation))

;; undo and redo changes in window configuration
;; with "C-c left", "C-c right"
(when (fboundp 'winner-mode)
  (winner-mode 1)
  (bind-key (kbd "<C-left>") 'winner-undo)
  (bind-key (kbd "<C-right>") 'winner-redo))

(setq-default
  show-trailing-whitespace t)
(add-hook 'cider-test-report-mode-hook '(lambda () (setq-default show-trailing-whitespace nil)))
(add-hook 'buffer-menu-mode-hook '(lambda () (setq-default show-trailing-whitespace nil)))
(add-hook 'minibuffer-setup-hook '(lambda () (setq-default show-trailing-whitespace nil)))
(add-hook 'before-save-hook 'delete-trailing-whitespace)

(defun my-elisp-hook ()
  (outline-hide-sublevels 1))
(add-hook 'emacs-lisp-mode-hook #'my-elisp-hook)

(use-package elisp-lint
  :disabled
  :config
   ;; (elisp-lint-ignored-validators . ("byte-compile" "backage-format"))
;;     ((emacs-lisp-mode . ((fill-column . 80)
;;                          (indent-tabs-mode . nil)
;;                          (elisp-lint-ignored-validators . ("byte-compile" "backage-format"))
;;                          (elisp-lint-indent-specs . ((describe . 1)
;;                                                      (it . 1))))))
    )

;; (set-face-font 'default "-MS  -Consolas-normal-normal-normal-*-23-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-PfEd-Inconsolata-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")
(set-face-font 'default "-PfEd-Terminus (TTF)-normal-normal-normal-*-24-*-*-*-m-0-iso10646-1")
;; (set-face-font 'default "-ADBO-Source Code Pro-normal-normal-normal-*-22-*-*-*-m-0-iso10646-1")

;; default Latin font (e.g. Consolas)
;; (set-face-attribute 'default nil :family "Consolas")
  ;; default font size (point * 10)
;; change font size for the current frame
;; (set-face-attribute 'default (selected-frame) :height 181)
;; (set-face-attribute 'default nil :height 181)
;; use specific font for Korean charset.
;; if you want to use different font size for specific charset,
;; add :size POINT-SIZE in the font-spec.
(set-fontset-font t 'hangul (font-spec :name "NanumGothicCoding"))

;; (use-package beacon
;;   :init
;;   (setq beacon-size 100)
;;   (setq beacon-color "#00FF00")
;;   (setq beacon-blink-when-buffer-changes t)
;;   (setq beacon-blink-when-window-changes t)
;;   (setq beacon-blink-when-window-scrolls t)
;;   (setq beacon-blink-when-point-moves-vertically 20)
;;   (setq beacon-blink-duration 0.8)
;;   (setq beacon-blink-delay 0.2)
;; ;; (defcustom beacon-dont-blink-major-modes '(t magit-status-mode magit-popup-mode
;; ;;                                        inf-ruby-mode
;; ;;                                        mu4e-headers-mode
;; ;;                                        gnus-summary-mode gnus-group-mode)
;; ;;   "A list of major-modes where the beacon won't blink.
;; ;; Whenever the current buffer satisfies `derived-mode-p' for
;; ;; one of the major-modes on this list, the beacon will not
;; ;; blink."
;; ;;   :type '(repeat symbol))

;; ;; (defcustom beacon-dont-blink-commands '(next-line previous-line
;; ;;                                             forward-line)
;;   :config
;;   (beacon-mode 1))

(use-package which-key
  :init
  (which-key-mode))

(use-package flycheck
  ;; :disabled
  ;; :init (global-flycheck-mode)
  :config
  (add-hook 'clojure-mode-hook #'flycheck-mode)
  )

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'org-mode-hook #'rainbow-delimiters-mode))
  ;; (rainbow-delimiters-mode 1)

(use-package rainbow-mode
  :init
  (add-hook 'prog-mode-hook 'rainbow-mode)
  (add-hook 'help-mode-hook 'rainbow-mode))

(use-package sudo-edit
  :bind
  ("C-c S" . sudo-edit))

(use-package yasnippet
  :config
  (use-package yasnippet-snippets)
  (yas-reload-all))

(defun kill-curr-buffer ()
  (interactive)
  ;; do not save buffers that strat with "*"
  (when (string-match "^\*.*$" (buffer-name (current-buffer)))
      (save-buffer (current-buffer)))
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-x k") #'kill-curr-buffer)

;;(global-set-key (kbd "C-x C-b") 'ibuffer)

(use-package company
  :diminish
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  (setq company-minimum-prefix-length 0)
  ;; upper case, lower case ignore when searching.
  (setq completion-ignore-case nil)
  (setq company-dabbrev-ignore-case nil)
  ;; do not downcase (lower case) the candidates (if upper case exist)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay nil))

(with-eval-after-load 'company
  ;; (define-key global-map (kbd "s-c") 'company-complete)
  (global-set-key (kbd "TAB") #'company-indent-or-complete-common)
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous)

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
           company-yasnippet
           )
          (company-abbrev company-dabbrev)
          ))

  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (list '(company-capf :with company-dabbrev)
                         (car company-backends)))))

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

  ;; (with-eval-after-load 'company
  ;;   (add-hook 'emacs-lisp-mode 'company-mode)
  ;;   (add-hook 'cider-repl-mode-hook 'company-mode)
  ;;   (add-hook 'cider-mode-hook 'company-mode))

  ;; (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  ;; (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)
  )
