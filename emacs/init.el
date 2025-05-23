;; fix "bad request issue"
;; https://github.com/syl20bnr/spacemacs/issues/12535
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;* prevent garbage collection for while loading configuration
;; (setq gc-cons-threshold 50000000)

;; (add-hook 'emacs-startup-hook 'my/set-gc-threshold)
;; (defun my/set-gc-threshold ()
;;   "Reset `gc-cons-threshold' to its default value."
;;   (setq gc-cons-threshold 800000))

;;* debug

(setq debug-on-error t)
(setq debug-ignored-errors nil)

;; credit to https://github.com/Fuco1/.emacs.d
(defmacro my-with-elapsed-timer (text &rest body)
  (declare (indent 1))
  (let ((nowvar (make-symbol "now")))
    `(let ((,nowvar (current-time)))
       (message "%s..." ,text)
       (prog1 (progn ,@body)
         (let ((elapsed
                (float-time (time-subtract (current-time) ,nowvar))))
           (message "%s...done (%.3fs)" ,text elapsed))))))

(require 'package)
;; safely use a symlinked ~/.emacs.d
(setq user-emacs-directory (file-truename "~/.emacs.d/"))
(setq package-enabled-at-startup nil)

;; https://github.com/raxod502/straight.el#getting-started
(my-with-elapsed-timer "straight.el"
(defvar bootstrap-version)
;; (setq straight-repository-branch "develop")
(setq straight-repository-branch "master")
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage)))

;; (setq straight-use-package-by-default t)

(setq straight-recipe-overrides
      '((nil . ((eval-in-repl :type git :host github
                              :repo "altnometer/eval-in-repl")))))

(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
			 ))

;; if compiled version is old, use the source
(setq load-prefer-newer t)

(package-initialize)

;; ;; bootstrap use-package
;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

;; (straight-use-package 'use-package)
;; (setq use-package-verbose t)

(my-with-elapsed-timer "Initializing packages"
  ;; https://github.com/magnars/dash.el
  (straight-use-package 'dash)
  (straight-use-package 'dash-functional)
  ;; https://github.com/rejeep/f.el
  (straight-use-package 'f)
  ;; https://github.com/magnars/s.el
  (straight-use-package 's)

  (require 'f))

(add-to-list 'load-path "~/.emacs.d/lisp")

;; load config/packages.el
(my-with-elapsed-timer "Loading config/packages.el"
  (when (file-readable-p "~/.emacs.d/config/packages.el")
    (load-file (expand-file-name "~/.emacs.d/config/packages.el"))))

;; load lisp/ram-abbrev.el after config
(my-with-elapsed-timer "Loading lisp/ram-abbrev.el"
  (when (file-readable-p "~/.emacs.d/lisp/ram-abbrev.el")
    (load-file (expand-file-name "~/.emacs.d/lisp/ram-abbrev.el"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   (vector "#1d1f21" "#cc6666" "#b5bd68" "#f0c674" "#81a2be" "#b294bb"
           "#8abeb7" "#c5c8c6"))
 '(beacon-color "#cc6666")
 '(company-quickhelp-color-background "#4F4F4F")
 '(company-quickhelp-color-foreground "#DCDCCC")
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("feb8e98a8a99d78c837ce35e976ebcc97abbd8806507e8970d934bb7694aa6b3"
     "6cbf6003e137485fb3f904e76fb15bc48abc386540f43f54e2a47a9884e679f6"
     "4a9f595fbffd36fe51d5dd3475860ae8c17447272cf35eb31a00f9595c706050"
     "5e0b63e0373472b2e1cf1ebcc27058a683166ab544ef701a6e7f2a9f33a23726"
     "dd854be6626a4243375fd290fec71ed4befe90f1186eb5b485a9266011e15b29"
     "8c75e2bdf8d1293c77a752dd210612cfb99334f7edd360a42a58a8497a078b35"
     "669e05b25859b9e5b6b9809aa513d76dd35bf21c0f16d8cbb80fb0727dc8f842"
     "e7666261f46e2f4f42fd1f9aa1875bdb81d17cc7a121533cad3e0d724f12faf2"
     "b60f08ddc98a95485ec19f046a81d5877b26ab80a67782ea5b91a00ea4f52170"
     "bc99493670a29023f99e88054c9b8676332dda83a37adb583d6f1e4c13be62b8"
     "5091eadbb87fa0a168a65f2c3e579d1a648d764f12ab9d3ab7bdefca709cd2a5"
     "9d54f3a9cf99c3ffb6ac8e84a89e8ed9b8008286a81ef1dbd48d24ec84efb2f1"
     "a4b9eeeabde73db909e6b080baf29d629507b44276e17c0c411ed5431faf87dd"
     "dc677c8ebead5c0d6a7ac8a5b109ad57f42e0fe406e4626510e638d36bcc42df"
     "1ca1f43ca32d30b05980e01fa60c107b02240226ac486f41f9b790899f6f6e67"
     "eb94e44599a45c495ad472ad551a40b87cbc4bae9631211e7a78d72b102c61b1"
     "1897b97f63e91a792e8540c06402f29d5edcbfb0aafd64b1b14270663d6868ee"
     "4b0b568d63b1c6f6dddb080b476cfba43a8bbc34187c3583165e8fb5bbfde3dc"
     "a02836a5807a687c982d47728e54ff42a91bc9e6621f7fe7205b0225db677f07"
     "c6b93ff250f8546c7ad0838534d46e616a374d5cb86663a9ad0807fd0aeb1d16"
     "e47c0abe03e0484ddadf2ae57d32b0f29f0b2ddfe7ec810bd6d558765d9a6a6c"
     "92d8a13d08e16c4d2c027990f4d69f0ce0833c844dcaad3c8226ae278181d5f3"
     "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce"
     "585942bb24cab2d4b2f74977ac3ba6ddbd888e3776b9d2f993c5704aa8bb4739"
     "8f97d5ec8a774485296e366fdde6ff5589cf9e319a584b845b6f7fa788c9fa9a"
     "a22f40b63f9bc0a69ebc8ba4fbc6b452a4e3f84b80590ba0a92b4ff599e53ad0"
     "bb68c8e9cf542b744c66ab37ee63704d0b8c72a9de7a24314cb41d831396ea47"
     "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a"
     "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016"
     "11e57648ab04915568e558b77541d0e94e69d09c9c54c06075938b6abc0189d8"
     "e9d8bfc1a5f875e25e6e8d7f041b12d3fdc7fafa55e7854d1495c35dbf548e0c"
     "33782c24e19f7c5e2e1433ac24cbb388cd1c6dcd9c7740914844cc1a07e15b45"
     "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476"
     "0598c6a29e13e7112cfbc2f523e31927ab7dce56ebb2016b567e1eff6dc1fd4f"
     "fa2af0c40576f3bde32290d7f4e7aa865eb6bf7ebe31eb9e37c32aa6f4ae8d10"
     "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77"
     "a2cde79e4cc8dc9a03e7d9a42fabf8928720d420034b66aecc5b665bbf05d4e9"
     "85d1dbf2fc0e5d30f236712b831fb24faf6052f3114964fdeadede8e1b329832"
     default))
 '(fci-rule-color "#073642")
 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(frame-background-mode 'dark)
 '(haskell-process-args-stack-ghci
   '("--ghci-options=-ferror-spans -fshow-loaded-modules" "--no-build"
     "--no-load"))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map (solarized-color-blend it "#002b36" 0.25)
          '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900"
            "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50)
     ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D"
     "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"
     "#002b36" "#002b36"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-sexp-background-color "#efebe9")
 '(ibuffer-saved-filter-groups nil)
 '(ibuffer-saved-filters
   '(("dired-translation" (directory . "translation") (mode . dired-mode))
     ("dired" (mode . dired-mode))
     ("programming"
      (or (derived-mode . prog-mode) (mode . ess-mode)
          (mode . compilation-mode)))
     ("text document"
      (and (derived-mode . text-mode) (not (starred-name))))
     ("TeX"
      (or (derived-mode . tex-mode) (mode . latex-mode)
          (mode . context-mode) (mode . ams-tex-mode)
          (mode . bibtex-mode)))
     ("web"
      (or (derived-mode . sgml-mode) (derived-mode . css-base-mode)
          (derived-mode . js-base-mode)
          (derived-mode . typescript-ts-base-mode) (mode . js2-mode)
          (derived-mode . haml-mode) (mode . sass-mode)))
     ("gnus"
      (or (mode . message-mode) (mode . mail-mode)
          (mode . gnus-group-mode) (mode . gnus-summary-mode)
          (mode . gnus-article-mode)))))
 '(ivy-prescient-enable-filtering t)
 '(ivy-prescient-enable-sorting t)
 '(ivy-prescient-retain-classic-highlighting t)
 '(ivy-prescient-sort-commands '(:not swiper ivy-switch-buffer counsel-switch-buffer))
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D"
     "#2aa198" "#d33682" "#6c71c4"))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(safe-local-variable-values
   '((org-roam-db-location
      . "~/.local/share/emacs/my.emacs.d/org-roam.db")
     (org-roam-directory . "~/backup/books")
     (vc-prepare-patches-separately)
     (diff-add-log-use-relative-names . t)
     (vc-git-annotate-switches . "-w")
     (eval progn
           (make-variable-buffer-local
            'cider-jack-in-nrepl-middlewares)
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "cider.nrepl/cider-middleware")
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "portal.nrepl/wrap-portal")
           (add-to-list 'cider-jack-in-nrepl-middlewares
                        "shadow.cljs.devtools.server.nrepl/middleware"))
     (cider-repl-display-help-banner)
     (elisp-lint-indent-specs (git-gutter:awhen . 1))))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f") (40 . "#c8805d801780") (60 . "#bec073400bc0")
     (80 . "#b58900") (100 . "#a5008e550000") (120 . "#9d0091000000")
     (140 . "#950093aa0000") (160 . "#8d0096550000") (180 . "#859900")
     (200 . "#66aa9baa32aa") (220 . "#57809d004c00")
     (240 . "#48559e556555") (260 . "#392a9faa7eaa") (280 . "#2aa198")
     (300 . "#28669833af33") (320 . "#279993ccbacc")
     (340 . "#26cc8f66c666") (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((comp) (comp)))
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00"
                 "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2"
                 "#93115C" "#d33682" "#00736F" "#2aa198" "#839496"
                 "#657b83"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198"
    "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1"
    "#fdf6e3"]))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-lead-face ((t (:inherit nil :background "#e52b50" :foreground "white"))))
 '(avy-lead-face-0 ((t (:inherit nil :background "#4f57f9" :foreground "white"))))
 '(avy-lead-face-1 ((t (:inherit nil :background "gray" :foreground "white"))))
 '(avy-lead-face-2 ((t (:inherit nil :background "#f86bf3" :foreground "white"))))
 '(fixed-pitch ((t (:family "Operator Mono" :height 190 :weight semi-light))))
 '(font-lock-comment-face ((t (:family "Operator Mono Light-19"))))
 '(font-lock-doc-face ((t (:family "Operator Mono Light-19" :slant italic))))
 '(font-lock-string-face ((t (:family "Operator Mono Light-19" :slant italic))))
 '(region ((t (:inherit nil :background "#5ada88" :foreground "#100a14"))))
 '(variable-pitch ((t (:family "Bembo" :height 260 :weight normal)))))
(put 'magit-diff-edit-hunk-commit 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'dired-find-alternate-file 'disabled nil)
(put 'erase-buffer 'disabled nil)
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
(put 'scroll-left 'disabled nil)
(put 'narrow-to-page 'disabled nil)
