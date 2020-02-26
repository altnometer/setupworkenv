;; packages.el --- Package configuration

(require 'use-package)

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

(defvar ram-leader-map-tap (make-sparse-keymap)
  "Keymap for \"leader tap key\" shortcuts.")

(defvar ram-leader-map-hold (make-sparse-keymap)
  "Keymap for \"leader hold key\" shortcuts.")

(define-key global-map (kbd "<SunProps>") ram-leader-map-tap)
(define-key global-map (kbd "<cancel>") ram-leader-map-hold)

;;** Bindings
(define-key global-map (kbd "H-o") 'other-window)
(define-key global-map (kbd "H-O") (lambda () (interactive) (other-window -1)))
(define-key ram-leader-map-tap (kbd "n") 'comint-dynamic-complete-filename)
(define-key ram-leader-map-tap (kbd "w") 'delete-other-windows)
(define-key ram-leader-map-tap (kbd "W") 'delete-window)
(define-key ram-leader-map-tap (kbd "a") (lambda () (interactive) (find-file "~/.emacs.d/lisp/ram-abbrev.el")))
(define-key ram-leader-map-tap (kbd "i") 'completion-at-point)

(defun ram-switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(define-key ram-leader-map-tap (kbd "p") 'ram-switch-to-previous-buffer)


;;* magit
(use-package magit
  :bind (("C-c g" . 'magit-file-dispatch)
         ("C-c C-g" . 'magit-dispatch))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (setq magit-save-repository-buffers 'dontask))

(define-key global-map (kbd "s-m") 'magit-status)

;;* Windows, Buffers
;;** General Window Settings
;; used for splitting windows sensibly, default width 160, height 80
;; nil values would forbid splitting
(setq split-width-threshold 150)
(setq split-height-threshold 180)

;;** Inital Window Split
;; https://www.simplify.ba/articles/2016/01/25/display-buffer-alist/
;; https://www.reddit.com/r/emacs/comments/cpdr6m/any_additional_docstutorials_on_displaybuffer_and/
(defun ram-init-win-splits ()
  "Split an initial frame window into convinient configuration."
  (interactive)
  (if (> (window-width) 300)
      (progn
        ;; 107 columns wide, makes last(of 3) win 2 col narrower
        ;; (split-window (selected-window) 107 "right")
        (split-window (selected-window) 159 "right")
        ;; Go to next window
        ;; (other-window 1)
        ;; (split-window (selected-window) 107 "righ")
        ;; Start eshell in current window
        ;; (eshell)
        ;; Go to previous window
        ;; (other-window -1)
        ;; never open any buffer in window with shell
        ;; (set-window-dedicated-p (nth 1 (window-list)) t)
        )))

(ram-init-win-splits)

;;** Displaying Info Buffers
;; Modes
(defvar ram-info-modes '(lisp-interaction-mode
                         occur-mode
                         diff-mode
                         magit-diff-mode
                         helpful-mode
                         help-mode
                         Man-mode
                         woman-mode
                         Info-mode
                         pydoc-mode
                         eww-mode       ;eww is used for python docs
                         pylookup-mode))
;; Buffers
(defvar ram-info-buffers '("*ob-ipython-inspect*"
                           "*Ivy Help*"))

(defun ram-info-buffer-p (buf act)
  "Check if BUF belongs to help buffers."
  (or (member (buffer-local-value 'major-mode  (get-buffer buf)) ram-info-modes)
      (member (if (stringp buf) buf (buffer-name buf)) ram-info-buffers)))

(defun ram-display-buffer-common-window (buffer action)
  "Display buffer in a common window"
  (let ((mode (buffer-local-value 'major-mode (get-buffer buffer))))
    (if (member mode ram-info-modes)
        (progn
          (let ((window (car (remove-if-not
                              (lambda (w) (member
                                           (buffer-local-value 'major-mode (window-buffer w)) ram-info-modes))
                              (window-list)))))
            (when window (set-window-buffer window buffer))
            window)))))

(add-to-list 'display-buffer-alist
             `(ram-info-buffer-p        ;predicate
               (;; display-buffer-reuse-window
                ram-display-buffer-common-window
                display-buffer-reuse-mode-window
                ;; display-buffer-use-some-window
                display-buffer-in-side-window) ;functions to try
               (side . right)
               (window-width . 0.5)
               (mode . ,ram-info-modes)
               (inhibit-same-window . nil)))

(add-to-list 'display-buffer-alist
             `("^.*\\.clj$"
               (display-buffer-reuse-window
                display-buffer-same-window)
               (inhibit-same-window . nil)))

;;** Display Auxiliary Buffers
;; Modes
;; (defvar my/subcode-modes '(inferior-python-mode
;;                            compilation-mode
;;                            shell-mode
;;                            eshell-mode
;;                            occur-mode
;;                            navi-mode
;;                            flycheck-error-list-mode))
;; (add-to-list 'display-buffer-alist
;;              `(,(lambda (buf act)
;;                   (member (buffer-mode buf) my/subcode-modes))
;;                (display-buffer--maybe-same-window
;;                 display-buffer-reuse-window
;;                 display-buffer-reuse-mode-window
;;                 display-buffer-at-bottom)
;;                (side . bottom)
;;                (mode . ,my/subcode-modes)
;;                (window-height . 0.25)
;;                (quit-restore ('window 'window nil nil))))

;; (add-to-list 'display-buffer-alist
;;              '("\\*help"
;;                (display-buffer-reuse-window display-buffer-use-some-window display-buffer-in-side-window)
;;                (side . right)
;;                (inhibit-same-window . t)
;;                (window-width . 0.5)))

;;* org-mode
;;** org-mode bindings
(bind-key "C-h a" 'apropos)

(defun ram-org-hide-block-toggle-all ()
  "interactive org-hide-block-toggle-all"
  (interactive)
  (org-hide-block-toggle-all))

(define-key org-mode-map (kbd "H-s") 'org-next-visible-heading)
(define-key org-mode-map (kbd "H-S") 'org-forward-heading-same-level)
(define-key org-mode-map (kbd "H-r") 'org-previous-visible-heading)
(define-key org-mode-map (kbd "H-R") 'org-backward-heading-same-level)
(define-key org-mode-map (kbd "C-H-S") 'org-next-block)
(define-key org-mode-map (kbd "C-H-R") 'org-previous-block)
(define-key org-mode-map (kbd "C-c z") 'ram-org-hide-block-toggle-all)

;;** org-mode common settings
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'org-hide-block-all)
;; export org file as html
(use-package htmlize)
(require 'htmlize)
;; modify org-emphasis-regexp-components, 3rd entry, to include char to emphasis markup
;; https://emacs.stackexchange.com/questions/13820/inline-verbatim-and-code-with-quotes-in-org-mode
(setcar (nthcdr 2 org-emphasis-regexp-components) " \t\r\n,\"")
(setq org-hide-emphasis-markers t)
(setq org-descriptive-links nil)
(setq org-src-window-setup 'current-window)
(setq org-hide-leading-stars nil)
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-return-follows-link t)

;;** org-mode snippets
(add-to-list 'org-structure-template-alist
             '("el" "#+BEGIN_SRC emacs-lisp\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist
             '("cl" "#+BEGIN_SRC clojure\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist
             '("cls" "#+BEGIN_SRC clojurescript\n?\n#+END_SRC"))
(add-to-list 'org-structure-template-alist
             '("n" "#+NAME: ?"))
(add-to-list 'org-structure-template-alist
             '("he" "#+HEADER: ?"))

;;** org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell . t)
   (js . t)
   (emacs-lisp . t)
   (clojure . t)
   (python . t)
   (css . t)))


;; https://github.com/winterTTr/ace-jump-mode
(use-package ace-jump-mode
  :load-path "site-lisp/ace-jump-mode"
  :diminish
  :bind ("C-c <SPC>" . ace-jump-word-mode)
  :init
  (setq ace-jump-mode-move-keys '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9)))


;;* git-gutter
;; https://github.com/syohex/emacs-git-gutter
;; https://github.com/nschum/fringe-helper.el
;; https://github.com/syohex/emacs-git-gutter-fringe
(use-package git-gutter-fringe
  :defer 0.5
  :load-path "site-lisp/emacs-git-gutter-fringe"
  :diminish git-gutter
  :config
  (global-git-gutter-mode t)
  ;; (remove-hook 'git-gutter:update-hooks 'magit-refresh-buffer-hook)
  ;; (advice-add #'select-window :after (lambda () (git-gutter t)))
  ;;(git-gutter:linum-setup)
  (custom-set-variables
   '(git-gutter:update-interval 1)
   ;; don't ask y/n before staging/reverting
   '(git-gutter:ask-p nil)
   ;; don't log/message
   '(git-gutter:verbosity 0)
   ;; count unstaged hunks in the current buffer
   '(git-gutter:buffer-hunks 1)
   '(git-gutter:statistic 1)))
        ;; '(git-gutter:disabled-modes image-mode)

     ;; (set-face-background 'git-gutter-fr:modified "purple")
     ;; (set-face-foreground 'git-gutter-fr:added "green")
     ;; (set-face-foreground 'git-gutter-fr:deleted "red")


;;* hydra
(use-package hydra)
;; https://github.com/abo-abo/hydra#foreign-keys

;;** hydra-window
;; f11 is bound toggle-frame-full-screen by default
(global-unset-key (kbd "<f11>"))

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
(global-set-key (kbd "<f11>") 'hydra-window/body)

;;** hydra-git-gutter

(defhydra hydra-git-gutter (
                            :columns 4
                            :exit nil
                            :foreign-keys nil)
  "git-gutter"
  ("n" git-gutter:next-hunk "next")
  ("p" git-gutter:previous-hunk "prev")
  ("f" (lambda ()
         (interactive)
         (progn
           (goto-char (point-min))      ;move to first line
           (git-gutter:next-hunk 1)))
   "first")
  ("c" (lambda ()
         (interactive
          (progn
            (save-buffer)
            (if (get-buffer-window "*git-gutter:diff*") (delete-window (get-buffer-window "*git-gutter:diff*")))
            (magit-commit-create))))
   "commit" :exit t)
  ("N" git-gutter:end-of-hunk "end")
  ("m" git-gutter:mark-hunk "mark")
  ("d" git-gutter:popup-hunk "diff")
  ("r" git-gutter:revert-hunk "revert")
  ("s" git-gutter:stage-hunk "stage")
  ("SPC" nil "quit" :exit t)
  ("q" nil "quit" :exit t))

(define-key ram-leader-map-tap (kbd "h") 'hydra-git-gutter/body)

;;** hydra-multicursor

;; based on: https://github.com/abo-abo/hydra/wiki/multiple-cursors
(defhydra hydra-multiple-cursors (
                             ;; https://stackoverflow.com/questions/53798055/hydra-disable-interpretation-of-prefix-argument
                             ;; only C-u starts a universal prefix, 0..9, -, self-insert
                             :base-map (make-sparse-keymap)
                             :exit nil
                             :foreign-keys nil
                             :hint nil)
  "
   Next          Prev                 % 2(mc/num-cursors) selected
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
(setq package-check-signature nil)
(use-package cider
  :config
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
  ;; (unbind-key "M-s" cider-repl-mode-map)
  ;; :pin melpa-stable


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

;;** lispy settings
(eval-after-load "lispy"
  `(progn
     (setq lispy-avy-keys '(?s ?a ?r ?e ?t ?d ?n ?u ?o ?p ?l ?m ?f))
     (setq lispy-eval-display-style 'overlay)
     ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
     ;; (setq lispy-avy-style-char 'at-full)
     (setq lispy-avy-style-char 'at)
     (setq lispy-avy-style-paren 'at-full)
     (setq lispy-avy-style-symbol 'at-full)

     ;; replace a global binding with own function
     ;; (define-key lispy-mode-map (kbd "C-e") 'my-custom-eol)
     ;; replace a global binding with major-mode's default
     ;; (define-key lispy-mode-map (kbd "C-j") nil)
     ;; replace a local binding
     ;; (lispy-define-key lispy-mode-map "s" 'lispy-down)
     ))

;;** fix outline hide/expand cycling
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
(add-hook 'lispy-mode-hook (lambda () (advice-add #'org-show-children :override #'my-org-show-children)))

;; https://github.com/abo-abo/lispy/issues/57
;;*** lispy-mode-map-special
;; my custom lispy-mode-map-special to modify for BEAKL layout
;; (eval-after-load 'lispy )
(require 'lispy)
(setq lispy-mode-map-special
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
    map))
(lispy-set-key-theme '(special lispy c-digits))
;;** lispy bindings
(define-key ram-leader-map-tap (kbd "e") 'lispy-eval-and-comment)

;;* avy

(use-package avy
  :config
  ;; apply avy to all windows?
  (setq avy-all-windows nil)
  ;; avy-lead-face-0
  ;; Dim all windows when displaying overlay for targets
  (setq avy-background t)
  (setq avy-timeout-seconds 0.4)
  ;; (setq avy-highlight-first t)
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-keys-alist#avy-keys
  (setq avy-keys '(?s ?a ?r ?e ?t ?d ?n ?u ?o ?p ?l ?m ?f))
  ;; https://github.com/abo-abo/avy/wiki/defcustom#avy-style
  (setq avy-styles-alist '(;; (avy-goto-char-2 . post)
                           ;; (avy-goto-char-2 . pre)
                           (avy-goto-char-2 . at-full)
                           (avy-goto-char-timer . at-full)))
  ;; (setq avy-keys (nconc
  ;;                     (number-sequence ?1 ?9)
  ;;                     '(?0)))
  )

(eval-after-load "avy"
  '(progn
     ;; (define-key global-map (kbd "s-s") 'avy-goto-word-1-below)
     (define-key global-map (kbd "s-s") 'avy-goto-char-2-below)
     ;; (define-key global-map (kbd "s-r") 'avy-goto-word-1-above)
     (define-key global-map (kbd "s-r") 'avy-goto-char-2-above)
     (define-key global-map (kbd "s-d") 'avy-goto-char-in-line)
     (define-key global-map (kbd "s-S") 'avy-goto-line-below)
     (define-key global-map (kbd "s-R") 'avy-goto-line-above)
     (define-key global-map (kbd "s-N") 'avy-resume)
     (define-key global-map (kbd "s-n") 'avy-next)
     (define-key global-map (kbd "s-p") 'avy-prev)))

;;* projectile

(use-package projectile
  :diminish t
  :config
  (projectile-mode 1)
  (setq projectile-completion-system 'ivy)
  (define-key projectile-mode-map (kbd "<f12>") 'projectile-command-map)
  ;; <f7> because it is same place as 't' which the default binding for this command
  (define-key projectile-mode-map (kbd "<f12> <f7>") 'projectile-toggle-between-implementation-and-test)
  )

;;* multiple-cursors

(use-package multiple-cursors
  :config
  ;; (bind-key "C->" 'mc/mark-next-like-this)
  ;; (define-prefix-command 'ram/multiple-cursors-map)
  ;; (global-set-key (kbd "<f9>") 'ram/multiple-cursors-map)
  ;; (define-key ram/multiple-cursors-map (kbd "s") 'mc/mark-next-like-this)
  ;; (define-key ram/multiple-cursors-map (kbd "S") 'mc/unmark-next-like-this)
  ;; (define-key ram/multiple-cursors-map (kbd "r") 'mc/mark-previous-like-this)
  ;; (define-key ram/multiple-cursors-map (kbd "R") 'mc/unmark-previous-like-this)
  )

;;* Linters

;; First install the package:
(use-package flycheck-clj-kondo)

;;* Clojure Mode

;; then install the checker as soon as `clojure-mode' is loaded
(use-package clojure-mode
  :config
  (require 'flycheck-clj-kondo))



;;* super-save
(use-package super-save
  :config
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode 1))

;;* View key presses and commans
(use-package command-log-mode)

;;* ivy, swiper, counsel:
;; https://oremacs.com/swiper/
;; installing counsel would install swipel, ivy as dependencies
(straight-use-package
 '(counsel :type git :flavor melpa :files ("counsel.el" "counsel-pkg.el") :host github :repo "abo-abo/swiper"))
;;** ivy
;;*** ivy Settings
(ivy-mode 1)
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

(ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
(ivy-set-occur 'swiper 'swiper-occur)
(ivy-set-occur 'swiper-isearch 'swiper-occur)
(ivy-set-occur 'swiper-multi 'swiper-occur) ; TODO does not work

(add-hook 'ivy-occur-mode #'hl-line-mode)

(define-key ram-leader-map-tap (kbd "<up>") 'ivy-push-view)
(define-key ram-leader-map-tap (kbd "<down>") 'ivy-switch-view)
(define-key global-map (kbd "C-S-r") 'ivy-resume)

(define-key ivy-occur-mode-map  (kbd "f") 'forward-char)
(define-key ivy-occur-mode-map  (kbd "b") 'backward-char)
(define-key ivy-occur-mode-map  (kbd "n") 'ivy-occur-next-line)
(define-key ivy-occur-mode-map  (kbd "p") 'ivy-occur-previous-line)
(define-key ivy-occur-mode-map  (kbd "<C-return>") 'ivy-occur-press)

(define-key global-map (kbd "C-c C-r") 'ivy-resume)

;;*** ivy Extentions
;; credit to:  https://github.com/jkitchin/scimax
;; TODO: ivy-org-jump-to-headline, ivy-lispy-jump-to-headline: change name, make one function
(defun ivy-org-jump-to-headline ()
  "Jump to heading in the current buffer."
  (interactive)
  (let ((headlines '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      ;; this matches org headings in elisp too.
	      "^\\(;; \\)?\\(\\*+\\)\\(?: +\\(.*?\\)\\)?[ 	]*$"  nil t)
	(cl-pushnew (list
		     (format "%-80s"
			     (match-string 0))
		     (cons 'position (match-beginning 0)))
		    headlines)))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(goto-char (cdr (assoc 'position candidate)))
			(outline-show-entry)))))

(defun ivy-lispy-jump-to-headline ()
  "Jump to heading in the current buffer."
  (interactive)
  (let ((headlines '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
	      ;; this matches org headings in elisp too.
	      "^;;\\(?:;\\(;[^#]\\)\\|\\(\\*+\\)\\)\\(?: +\\(.*?\\)\\)?[ ]*$"  nil t)
	(cl-pushnew (list
		     (format "%-80s"
			     (match-string 0))
		     (cons 'position (match-beginning 0)))
		    headlines)))
    (ivy-read "Headline: "
	      (reverse headlines)
	      :action (lambda (candidate)
			(org-mark-ring-push)
			(goto-char (cdr (assoc 'position candidate)))
			(outline-show-entry)))))

(define-key org-mode-map (kbd "H-h") 'ivy-org-jump-to-headline)
(define-key clojure-mode-map (kbd "H-h") 'ivy-lispy-jump-to-headline)
(define-key emacs-lisp-mode-map (kbd "H-h") 'ivy-lispy-jump-to-headline)

;;*** ivy Packages
;; ivy-rich
(straight-use-package
 '(ivy-rich :type git :flavor melpa :files ("*.el" "ivy-rich-pkg.el") :host github :repo "Yevgnen/ivy-rich"))
(setcdr (assq t ivy-format-functions-alist)
        #'ivy-format-function-line)
(ivy-rich-mode 1)

;; ivy-posframe
(straight-use-package
 '(ivy-posframe :type git :flavor melpa :host github :repo "tumashu/ivy-posframe"))
(setq ivy-posframe-height-alist
      '((swiper . 15)
        (swiper-isearch . 15)
        (t . 10)))
(setq ivy-posframe-display-functions-alist
      '((complete-symbol . ivy-posframe-display-at-point)
        (swiper . nil)
        ;; (swiper-isearch . nil)
        (t . ivy-posframe-display-at-frame-center)))

(setq ivy-posframe-parameters
      '((width . 90)))

(ivy-posframe-mode 1)

;; prescient
(use-package prescient
  :after (counsel)
  :custom
  (prescient-history-length 50)
  (prescient-save-file "~/.emacs.d/prescient-items")
  (prescient-filter-method '(fuzzy initialism regexp))
  :config
  (prescient-persist-mode 1))

(use-package ivy-prescient
  :after (prescient ivy)
  :custom
  (ivy-prescient-sort-commands
   '(:not swiper ivy-switch-buffer counsel-switch-buffer))
  (ivy-prescient-retain-classic-highlighting t)
  (ivy-prescient-enable-filtering t)
  (ivy-prescient-enable-sorting t)
  :config
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
          (ivy-lispy-jump-to-headline . prot/ivy-prescient-filters)
          (ivy-switch-buffer . prot/ivy-prescient-filters)
          (t . ivy-prescient-re-builder)))
  (ivy-prescient-mode 1))

;;** swiper

(eval-after-load "ivy"
  '(progn
     (setq swiper-action-recenter t)
     (setq swiper-goto-start-of-match t)
     (setq swiper-include-line-number-in-search t)
     (define-key global-map (kbd "C-S-s") 'swiper)
     (define-key global-map (kbd "M-s s") 'swiper-multi)
     (define-key global-map (kbd "M-s w") 'swiper-thing-at-point)))

;;** counsel

(use-package counsel
  :after ivy
  :custom
  (counsel-yank-pop-preselect-last t)
  (counsel-yank-pop-separator "\n—————————\n")
  (counsel-rg-base-command
   "rg -SHn --no-heading --color never --no-follow --hidden %s")
  (counsel-find-file-occur-cmd          ; TODO Simplify this
   "ls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 ls -d --group-directories-first")
  :config
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

(define-key global-map (kbd "M-x") 'counsel-M-x)
(define-key global-map (kbd "C-x f") 'counsel-find-file)
(define-key global-map (kbd "C-x C-f") 'counsel-find-file)
(define-key global-map (kbd "C-x d") 'counsel-dired)

(define-key global-map (kbd "C-x b") 'counsel-switch-buffer)
(define-key global-map (kbd "C-x B") 'counsel-switch-buffer-other-window)

(define-key global-map (kbd "C-x C-r") 'counsel-recentf)
(define-key global-map (kbd "C-h f") 'counsel-describe-function)
(define-key global-map (kbd "C-h v") 'counsel-describe-variable)

(define-key global-map (kbd "s-f") 'counsel-find-file)
(define-key global-map (kbd "s-F") 'find-file-other-window)

(define-key global-map (kbd "s-b") 'counsel-switch-buffer)
(define-key global-map (kbd "s-B") 'counsel-switch-buffer-other-window)

(define-key ram-leader-map-tap (kbd "y") 'counsel-yank-pop)

(define-key global-map (kbd "s-l") 'counsel-dired)
(define-key global-map (kbd "s-L") 'dired-other-window)
;; (define-key global-map (kbd "s-r") 'counsel-recentf)
;; (define-key global-map (kbd "s-M-z") 'prot/counsel-fzf-rg-files)
;; (define-key global-map (kbd "s-M-r") 'counsel-rg)
(define-key global-map (kbd "s-M-g") 'counsel-git-grep)

(define-key ivy-minibuffer-map (kbd "C-r") 'counsel-minibuffer-history)
(define-key ivy-minibuffer-map (kbd "s-y") 'ivy-next-line)        ; Avoid 2× `counsel-yank-pop'
(define-key ivy-minibuffer-map (kbd "C-SPC") 'ivy-restrict-to-matches)

;; (define-key global-map (kbd "<f1> l") 'counsel-find-library)
;; (define-key global-map (kbd "<f2> u") 'counsel-unicode-char)
;; (define-key global-map (kbd "C-c g") 'counsel-git)
;; (define-key global-map (kbd "C-c j") 'counsel-git-grep)
;; (define-key global-map (kbd "C-c k") 'counsel-ag)

;;* Modeline
;;** Define faces

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
     (  ((class color))
        (:background "#F05B80"  :foreground "black")  )   ; "#F04040" maybe?
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that are under version control
but which have been edited."
  :group 'MY/mode)

(defface my/mode:vc-in-sync
  `(
     (  ((class color))
        (:background "#60CC60"  :foreground "black")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that are under version control
and which are in sync with the respository."
  :group 'MY/mode)

(defface my/mode:vc-none
  `(
     (  ((class color))
        (:background "#70A0D0"  :foreground "black")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files that are not under version
control"
  :group 'MY/mode)

(defface my/mode:vc-unknown
  `(
     (  ((class color))
        (:background "#FF0000"  :foreground "white")  )
     (  t
        (:weight bold :underline t)  )
   )
  "VC status tag face for files whose version-control status
cannot be determined."
  :group 'MY/mode)

;;** Define functions
(defvar my-vc-mode-attrs
  '((""  . (" NoVC "  my/mode:vc-none))
    ("-" . (" VC = "  my/mode:vc-in-sync))
    (":" . (" VC > "  my/mode:vc-edited))
    ("@" . (" VC + "  my/mode:vc-added))
    ("?" . (" ?VC? "  my/mode:vc-unknown))
    )
  "Lookup table to translate vc-mode character into another string/face."
)

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
            ""))

         ;; Fetch properties list for the class character above
         (props (cdr (assoc class my-vc-mode-attrs))))
    ;; (cadr (cdr (assoc ":" my-vc-mode-attrs)))

    ;; (concat (propertize (car props) 'face (cadr props))
    ;;         branch)
    (concat (propertize (format "%s " branch) 'face (cadr props)))))

(setq-default mode-line-format
              '(
                ;; %, * or hyphen for read only, changed, saved
                " %*%*%* "
                ;; mode-line-modified
                (:eval (propertize "%b " 'face font-lock-keyword-face
                                   'help-echo (buffer-file-name)))
                (:eval (if (boundp 'git-gutter-mode) (propertize (format "+%s " (car (git-gutter:statistic))) 'face '((:foreground "chartreuse3")))))
                (:eval (if (boundp 'git-gutter-mode) (propertize (format "-%s " (cdr (git-gutter:statistic))) 'face '((:foreground "chocolate")))))
                (:eval (my-mode-line-vc-info))
                ;; (:eval (propertize vc-mode 'face '((:foreground "DarkGoldenrod2"))))
                ;; line and column
                " ("
                "%02l" "," "%01c"
                ") "
                ;; value of `mode-name'
                (:eval (propertize " %m " 'face '((:foreground "plum3"))))
                ;; "-- user: "
                ;; value of user
                ;; (getenv "USER")
                ))

                ;;   (setq evil-normal-state-tag   (propertize " <N> " 'face '((:background "DarkGoldenrod2" :foreground "black")))
                ;;       evil-emacs-state-tag    (propertize " <E> " 'face '((:background "SkyBlue2"       :foreground "black")))
                ;;       evil-insert-state-tag   (propertize " <I> " 'face '((:background "chartreuse3"    :foreground "black")))
                ;;       evil-replace-state-tag  (propertize " <R> " 'face '((:background "chocolate"      :foreground "black")))
                ;;       evil-motion-state-tag   (propertize " <M> " 'face '((:background "plum3"          :foreground "black")))
                ;;       evil-visual-state-tag   (propertize " <V> " 'face '((:background "gray"           :foreground "black")))
                ;;       evil-operator-state-tag (propertize " <O> " 'face '((:background "sandy brown"    :foreground "black"))))a
;; (remove-hook 'magit-refresh-buffer-hook (lambda () (message "MAGIT REFRESH BUFFER IS CALLED!")))
;; (remove-hook 'magit-refresh-buffer-hook 'revert-buffer)
;; (remove-hook 'magit-refresh-buffer-hook 'force-mode-line-update)

;;* Doom Theme
(straight-use-package
   '(doom-themes :type git :flavor melpa :files (:defaults "themes/*.el" "doom-themes-pkg.el") :host github :repo "hlissner/emacs-doom-themes"))
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

;;* Isearch
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

(bind-key (kbd "M-s M-o") 'multi-occur global-map)
(bind-key (kbd "C-SPC")  'prot/isearch-mark-and-exit isearch-mode-map)
(bind-key (kbd "DEL")  'contrib/isearchp-remove-failed-part-or-last-char isearch-mode-map)
(bind-key (kbd "<C-return>")  'contrib/isearch-done-opposite-end  isearch-mode-map)
;;* Miscellaneous
(straight-use-package 'iedit)

;;* Spelling
;;** flycheck

;;** flycheck settings
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

;;** flycheck bindings
(define-key global-map (kbd "s-M-c") 'ispell-word)

(define-key global-map (kbd "s-M-C") 'flyspell-check-next-highlighted-word)

(define-key global-map (kbd "H-e") 'flycheck-next-error)
(define-key global-map (kbd "H-E") 'flycheck-previous-error)
;;** flyspell-goto-previous-error
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

;; bind-key* form would override all minor modes
(bind-key* (kbd "C-,") 'flyspell-goto-previous-error)
(define-key global-map (kbd "H--") 'flyspell-goto-previous-error)

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

(define-key ram-leader-map-tap (kbd "-") 'my-pop-local-mark-ring)
(define-key ram-leader-map-tap (kbd ".") 'unpop-to-mark-command)


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

(define-key ram-leader-map-tap (kbd "SPC") 'smart-open-line)
(define-key ram-leader-map-tap (kbd "S-SPC") 'smart-open-line-above)

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

(require 'dired)
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
(define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)

;; was dired-up-directory
(define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file "..")))

(global-set-key (kbd "C-c j") 'dired-jump)

;;* hippie

(define-key global-map (kbd "M-/") (make-hippie-expand-function
                                    '(
                                      ;; try-expand-all-abbrevs
                                      try-expand-dabbrev-visible
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev
                                      try-expand-file-name-partially
                                      try-expand-file-name
                                      try-expand-lisp-symbol-partially
                                      try-expand-lisp-symbol
                                      try-expand-line) t))

(define-key org-mode-map (kbd "M-/") (make-hippie-expand-function
                                    '(
                                      ;; try-expand-all-abbrevs
                                      try-expand-dabbrev-visible
                                      try-expand-dabbrev-all-buffers
                                      try-expand-dabbrev
                                      try-expand-file-name-partially
                                      try-expand-file-name
                                      try-expand-line) t))

;; (
;;  try-expand-list
;;  ;;  try-expand-dabbrev-from-kill
