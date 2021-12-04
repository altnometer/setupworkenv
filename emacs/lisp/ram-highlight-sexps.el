;;; ram-highlight-sexps.el --- highlight surrounding parentheses
;;
;; Based on highlight-sexps.el
;;
;; Copyright (C) 2011 David Rysdam
;;
;; Author: David Rysdam <david * rysdam org>
;; Version: 0.9.1
;; Keywords: faces, matching, s-expression, sexp
;; URL: http://david.rysdam.org/src/emacs/highlight-sexps.el
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; Based on highlight-parentheses:
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 1.0.1
;; Keywords: faces, matching
;; URL: http://nschum.de/src/emacs/highlight-parentheses/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; The number of nested s-expressions highlighted is
;; determined by the number of colors defined.
;;
;;; Code:

;;; vars for highlighting inner and outer sexps

(eval-when-compile (require 'cl))

(provide 'ram-highlight-sexps)

(defgroup highlight-sexps nil
  "Highlight the nested s-expressions around point"
  :group 'faces
  :group 'matching)

(defun hl-sexp-set (variable value)
  (set variable value)
  (when (fboundp 'hl-sexp-color-update)
    (hl-sexp-color-update)))

(defcustom hl-sexp-colors
  nil
  "*List of colors for the highlighted sexps.
The list starts with the the inside sexp and moves outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)


(defun ram-change-color (color by-n)
  "Return color changed by BY-N."
  (let* ((color-checked (if (not (string= color "unspecified-bg"))
                    color
                  (if (string= frame-background-mode "dark")
                      "#000000"
                    "#ffffff")))
         (color-ls (color-values color-checked)))
    (if (< (color-distance "black" color)
           (color-distance "white" color))
        (apply #'format "#%04x%04x%04x"
               (mapcar (lambda (c) (+ c by-n)) color-ls))
      (apply #'format "#%04x%04x%04x"
             (mapcar (lambda (c) (- c by-n)) color-ls)))))

(defun ram-make-highlight-color (base-color adjust-by-1 adjust-by-2)
  "Return a list of two colors derived from BASE-COLOR changed by adjust-by-1, adjust-by-2."
  (list (ram-change-color base-color adjust-by-1)
        (ram-change-color base-color adjust-by-2)))

  ;; '("#36454F" "#51484F")
  ;; '("#21242b" "#3f444a")                ; #3f444a is doom-one-theme base4
(defvar hl-sexp-background-colors
  (ram-make-highlight-color (face-background 'default) 0 8000)
  "*List of colors for highlighted sexps backgrounds.
The list starts with the inside parentheses and moves outwards.")

(defcustom hl-sexp-colors-when-at-del
  nil
  "*List of colors for the highlighted sexps when the point is before or after a delimiter.
The list starts with the the inside sexp and moves outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#354230")) ; kombu green

;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#232b2b")) ; charlston green

;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#36454f")) ; charcoal

;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#3b444b")) ; arcenic

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#4f666a")) ; stormcloud

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#534b4f")) ; dark liver

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#51484f")) ; quartz

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#4f3a3c")) ; dark puce

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#5e644f")) ; old bamboo

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#2f4f4f")) ; darkslate grey

;; (overlay-put (cadr hl-sexp-overlays-when-at-del) 'face '(:background "#353839")) ; onyx

;; triadic of doom base "#21242b"
;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#2b2124"))
;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#21242b"))
;; (overlay-put (car hl-sexp-overlays-when-at-del) 'face '(:background "#242b21"))

;; (bg '("#282c34" nil nil))
;; (bg-alt '("#21242b" nil nil))
;; (base0 '("#1B2229" "black" "black"))
;; (base1 '("#1c1f24" "#1e1e1e" "brightblack"))
;; (base2 '("#202328" "#2e2e2e" "brightblack"))
;; (base3 '("#23272e" "#262626" "brightblack"))
;; (base4 '("#3f444a" "#3f3f3f" "brightblack"))
;; (base5 '("#5B6268" "#525252" "brightblack"))
;; (base6 '("#73797e" "#6b6b6b" "brightblack"))
;; (base7 '("#9ca0a4" "#979797" "brightblack"))
;; (base8 '("#DFDFDF" "#dfdfdf" "white"))
;; (fg '("#bbc2cf" "#bfbfbf" "brightwhite"))
;; (fg-alt '("#5B6268" "#2d2d2d" "white"))

(defvar hl-sexp-background-colors-when-at-del
  (ram-make-highlight-color (face-background 'default) 2000 4000)
  "*List of colors for highlighted sexps backgrounds.
The list starts with the inside parentheses and moves outwards.")

(defcustom hl-sexp-siblings-number
  20
  "*Number of sibling sexps to highlight."
  :type '(integer)
  :group 'highlight-sexps)

(defcustom hl-sexp-colors-siblings
  ;; '("wheat4" "yellow1")
  '("wheat4" "wheat4")
  "*List of colors for highlighting sibling sexps."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defcustom hl-sexp-background-siblings
  ;; '("wheat2" "wheat4")
  '("wheat2" "DarkGoldenrod3")
  "*List of colors for highlighting sibling sexps backgrounds."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defcustom hl-sexp-paren-siblings-attributes
  ;; '((:weight extra-bold) (:weight extra-bold))
  '((:weight normal) (:weight normal))
  "*Face attributes for highlighting sibling parentheses."
  :type '(plist)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defvar hl-sexp-paren-siblings-overlays nil
  "Overlays for highlighting sibling parentheses.")
(make-variable-buffer-local 'hl-sexp-paren-siblings-overlays)

(defcustom hl-sexp-highlight-adjacent nil
  "If the point is immediately before or after sexp, highlight it as the inner sexp,
just like show-paren-mode."
  :type '(boolean)
  :group 'highlight-sexps)

(defcustom hl-sexp-masking-overlays-number 80
  "Determines how many lines is possible to mask."
  :type '(boolean)
  :group 'highlight-sexps)

(defface hl-sexp-face nil
  "*Face used for highlighting sexps.
Color attributes might be overriden by `hl-sexp-colors' and
`hl-sexp-background-colors'."
  :group 'highlight-sexps)

(defvar hl-sexp-overlays nil
  "Overlays for highlighting sexps.")
(make-variable-buffer-local 'hl-sexp-overlays)

(defvar hl-sexp-overlays-when-at-del nil
  "Overlays for highlighting sexps when point is either before delimiter or after.")
(make-variable-buffer-local 'hl-sexp-overlays-when-at-del)

;;; vars for highlighting inner and outer sexps parens

(defvar hl-sexp-paren-overlays nil
  "Overlays for highlighting parens.")
(make-variable-buffer-local 'hl-sexp-paren-overlays)

(defcustom hl-sexp-paren-colors
  ;; '("cyan" "cyan" "red4" "red4" "OrangeRed4" "OrangeRed4")
  '("cyan" "cyan" "red1" "red2" "OrangeRed1" "OrangeRed2")
  ;; '("firebrick1" "firebrick1" "green2" "green2")
  "*List of colors for highlighting pairs of parentheses.
The list starts with the pair for the inside parentheses and
moves outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defcustom hl-sexp-paren-background-colors
  ;; '("purple1" "purple1" "DodgerBlue1" "DodgerBlue1" "aquamarine3" "aquamarine3")
  '("MediumOrchid2" "orchid1" "DodgerBlue3" "SkyBlue1" "aquamarine4" "aquamarine3")
  "*List of background colors for highlighting pairs of parentheses.
The list starts with the pair for the inside parentheses and
moves outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defcustom hl-sexp-paren-attributes
  ;; '((:weight extra-bold) (:weight extra-bold) (:weight extra-bold) (:weight extra-bold))
  '((:weight normal) (:weight normal) (:weight normal) (:weight normal))
  "*List of attributes for highlighting pairs of parentheses.
The list starts with the pair for the inside parentheses and
moves outwards."
  :type '(choice plist function)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defface hl-sexp-paren-face nil
  "*Face used for highlighting parens.
Color attributes might be overriden by `hl-sexp-paren-colors'."
  :group 'highlight-sexps)

(defvar hl-sexp-paren-when-at-del-overlays nil
  "Overlays for highlighting parentheses when the point is next to them.")
(make-variable-buffer-local 'hl-sexp-paren-when-at-del-overlays)

(defcustom hl-sexp-paren-when-at-del-colors
  ;; '("cyan" "cyan" "red4" "red4" "OrangeRed4" "OrangeRed4")
  '("cyan" "cyan" "red1" "red2" "OrangeRed1" "OrangeRed2")
  "*List of colors for highlighting pairs of parentheses when the point is next to them.
The list starts with the pair for the inside parentheses and
moves outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

;; (overlay-put (car hl-sexp-paren-when-at-del-overlays) 'face '(:background "dodger blue" :foreground "red4" :weight extra-bold))
(defcustom hl-sexp-paren-when-at-del-background-colors
  ;; '("purple1" "purple1" "DodgerBlue1" "DodgerBlue1" "aquamarine3" "aquamarine3")
  '("MediumOrchid2" "orchid1" "DodgerBlue3" "SkyBlue1" "aquamarine4" "aquamarine3")
  "*List of background colors for highlighting pairs of parentheses when the point is next to them.
The list starts with the pair for the inside parentheses and
moves outwards."
  :type '(repeat color)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defcustom hl-sexp-paren-when-at-del-attributes
  ;; '((:weight extra-bold) (:weight extra-bold)
  ;;   (:weight extra-bold) (:weight extra-bold)
  ;;   (:weight extra-bold) (:weight extra-bold))
  '((:weight normal) (:weight normal)
    (:weight normal) (:weight normal)
    (:weight normal) (:weight normal))
  "*List of attributes for highlighting pairs of parentheses when the point is next to them.
The list starts with the pair for the inside parentheses and
moves outwards."
  :type '(choice plist function)
  :set 'hl-sexp-set
  :group 'highlight-sexps)

(defface hl-sexp-paren-when-at-del-face nil
  "*Face used for highlighting parentheses.
Color attributes might be overriden by
`hl-sexp-paren-when-at-del-colors',
`hl-sexp-paren-when-at-del-background-colors',
`hl-sexp-paren-when-at-del-attributes'."
  :group 'highlight-sexps)

;;; vars for masking excessive whitespace created by highlighting overlays

(defvar hl-sexp-mask-whitespace-overlays nil)
(make-variable-buffer-local 'hl-sexp-mask-whitespace-overlays)

(defvar hl-sexp-mask-leading-space-background-color
  (let ((bg (face-background 'default)))
    (if (or (not (stringp bg))
            (string-match "\\`unspecified-" bg))
        "#000000"
      bg))
  "*Background overlay face color to mask leading spaces.")

(defface hl-sexp-mask-leading-space-face nil
  "*Face used for covering whitespace modified by other overlays.
Color attributes might be overriden by `hl-sexp-mask-leading-space-background-color'."
  :group 'highlight-sexps)

(defvar hl-sexps-last-point -1
  "The last point for which sexps were highlighted.
This is used in tandem with `hl-sexps-last-max-point' to prevent
repeating highlighting the same sexps in the same context.")
(make-variable-buffer-local 'hl-sexps-last-point)

(defvar hl-sexps-last-max-point -1
  "The last max point when sexps were highlighted.
This is used in tandem with `hl-sexps-last-point' to prevent
repeating highlighting the same sexps in the same context.")
(make-variable-buffer-local 'hl-sexps-last-max-point)

(defvar last-at-opening-paren-p nil
  "True if last delimiter visited was an opening one.")
(make-variable-buffer-local 'last-at-opening-paren-p)

(defun hl-sexp-highlight ()
  "Highlight the nested s-expressions around point"
  (when (not (and
              (= (point) hl-sexps-last-point)
              (= (point-max) hl-sexps-last-max-point)))
    (setq hl-sexps-last-point (point))
    (setq hl-sexps-last-max-point (point-max))
    (let* ((p (point))
           (at-delimiter-p (or (memq (char-after p) '(?\( ?\{ ?\[ ?\<))
                               (memq (char-before p) '(?\) ?\} ?\] ?\>))))
           (overlays-sexp (if at-delimiter-p
                              (list hl-sexp-overlays-when-at-del hl-sexp-overlays)
                            (list hl-sexp-overlays hl-sexp-overlays-when-at-del)))
           (overlays-parens (if at-delimiter-p
                                (list hl-sexp-paren-when-at-del-overlays hl-sexp-paren-overlays)
                              (list hl-sexp-paren-overlays hl-sexp-paren-when-at-del-overlays)))
           (overlays-siblings hl-sexp-paren-siblings-overlays)
           (sexp-list (hl-sexp-end-points p
                                          (length (car overlays-sexp))))
           (sexps-for-mask sexp-list)
           (sexps-for-parens (hl-sexp-end-points p
                                                 (length (car overlays-parens))))
           (sexps-siblings (if (caadr sexps-for-parens)
                               (hl-sexp-get-siblings-end-points (1+ (caadr sexps-for-parens)) hl-sexp-siblings-number)))
           pos1
           pos2
           at-opening-paren-p)
      (condition-case err
          (while (and (car overlays-sexp) sexp-list)
            (let* ((overlay (pop (car overlays-sexp)))
                   (sexp (pop sexp-list))
                   (pos1 (car sexp))
                   (pos2 (cadr sexp)))
              (move-overlay overlay pos1 pos2)))
        (error nil))
      (when sexps-for-mask
        (hl-sexp-mask-leading-space sexps-for-mask))
      (when sexps-for-parens
        (while (and (car overlays-parens) sexps-for-parens)
          (let* ((overlay-1 (pop (car overlays-parens)))
                 (overlay-2 (pop (car overlays-parens)))
                 (sexp (pop sexps-for-parens))
                 (pos1 (car sexp))
                 (pos2 (cadr sexp)))
            ;; highlight parens next to cursor in different color
            (if (or at-opening-paren-p
                    (and (not at-delimiter-p)
                         last-at-opening-paren-p)
                    (and at-delimiter-p
                         (or (= pos1 p) (= (1+ pos1) p))))
                (progn
                  (setq at-opening-paren-p t)
                  (setq last-at-opening-paren-p t)
                  (move-overlay overlay-1 pos1 (1+ pos1))
                  (move-overlay overlay-2 (1- pos2) pos2))
              (progn
                (setq at-opening-paren-p nil)
                (setq last-at-opening-paren-p nil)
                (move-overlay overlay-2 pos1 (1+ pos1))
                (move-overlay overlay-1 (1- pos2) pos2))))))
      (when sexps-siblings
        (while (and overlays-siblings sexps-siblings)
          (let* ((overlay-1 (pop overlays-siblings))
                 (overlay-2 (pop overlays-siblings))
                 (sexp (pop sexps-siblings))
                 (pos1 (car sexp))
                 (pos2 (cadr sexp)))
            (if at-opening-paren-p
                (progn (move-overlay overlay-1 pos1 (1+ pos1))
                       (move-overlay overlay-2 (1- pos2) pos2))
              (progn (move-overlay overlay-2 pos1 (1+ pos1))
                     (move-overlay overlay-1 (1- pos2) pos2))))))
      (dolist (ov (car overlays-sexp))
        (move-overlay ov 1 1))
      (dolist (ov (cadr overlays-sexp))
        (move-overlay ov 1 1))
      (dolist (ov (car overlays-parens))
        (move-overlay ov 1 1))
      (dolist (ov (cadr overlays-parens))
        (move-overlay ov 1 1))
      (dolist (ov overlays-siblings)
        (move-overlay ov 1 1)))))

;;;###autoload
(define-minor-mode ram-highlight-sexps-mode
  "Minor mode to highlight an expanding set of surrounding s-expressions."
  :init-valuer: nil :lighter " hl-s" :keymap nil
  (mapc 'delete-overlay hl-sexp-overlays)
  (mapc 'delete-overlay hl-sexp-overlays-when-at-del)
  (mapc 'delete-overlay hl-sexp-mask-whitespace-overlays)
  (mapc 'delete-overlay hl-sexp-paren-overlays)
  (mapc 'delete-overlay hl-sexp-paren-when-at-del-overlays)
  (mapc 'delete-overlay hl-sexp-paren-siblings-overlays)

  (kill-local-variable 'hl-sexp-overlays)
  (kill-local-variable 'hl-sexp-overlays-when-at-del)
  (kill-local-variable 'hl-sexp-mask-whitespace-overlays)
  (kill-local-variable 'hl-sexp-paren-overlays)
  (kill-local-variable 'hl-sexp-paren-when-at-del-overlays)
  (kill-local-variable 'hl-sexp-paren-siblings-overlays)

  (kill-local-variable 'hl-sexps-last-point)
  (kill-local-variable 'hl-sexps-last-max-point)
  (kill-local-variable 'last-at-opening-paren-p)
  (remove-hook 'post-command-hook 'hl-sexp-highlight t)
  (when (and ram-highlight-sexps-mode
             (not (eq major-mode 'messages-buffer-mode))
             (not (string= (buffer-name) "*Messages*")))
    (hl-sexp-create-overlays)
    (hl-sexp-create-overlays-when-at-del)
    (hl-sexp-create-overlays-masking-leading-space (* 2 hl-sexp-masking-overlays-number))
    (hl-sexp-create-paren-overlays)
    (hl-sexp-create-paren-when-at-del-overlays)
    (hl-sexp-create-paren-sibling-overlays)
    (add-hook 'post-command-hook 'hl-sexp-highlight nil t)))

(defun hl-sexp-create-overlays ()
  "Create some sexp overlays."
  (let* ((fg hl-sexp-colors)
         (bg hl-sexp-background-colors)
         (count (max (length fg) (length bg)))
         (num count)
         attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg))))
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg))))
      (pop bg)
      (push (make-overlay 0 0) hl-sexp-overlays)
      (overlay-put (car hl-sexp-overlays) 'face attributes)
      ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
      ;; (overlay-put (car hl-sexp-overlays) 'priority num)
      (overlay-put (car hl-sexp-overlays) 'priority nil)
      (cl-decf num))
    (setq hl-sexp-overlays (nreverse hl-sexp-overlays))))

(defun hl-sexp-create-overlays-when-at-del ()
  "Create some sexp overlays used when the point is either before or after a delimiter."
  (let* ((fg hl-sexp-colors-when-at-del)
         (bg hl-sexp-background-colors-when-at-del)
         (count (max (length fg) (length bg)))
         (num count)
         attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg))))
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg))))
      (pop bg)
      (push (make-overlay 0 0) hl-sexp-overlays-when-at-del)
      (overlay-put (car hl-sexp-overlays-when-at-del) 'face attributes)
      ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
      ;; (overlay-put (car hl-sexp-overlays-when-at-del) 'priority num)
      (overlay-put (car hl-sexp-overlays-when-at-del) 'priority nil)
      (cl-decf num))
    (setq hl-sexp-overlays-when-at-del (nreverse hl-sexp-overlays-when-at-del))))

(defun hl-sexp-create-paren-sibling-overlays ()
  "Create overlays for highlighting sibling parentheses."
  (let* ((fg hl-sexp-colors-siblings)
         (bg hl-sexp-background-siblings)
         (num hl-sexp-siblings-number)
         attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-face))
      (when fg
        (setq attributes (plist-put attributes :foreground
                                    (if (evenp num)
                                        (car fg)
                                      (cadr fg)))))
      (when bg
        (setq attributes (plist-put attributes :background
                                    (if (evenp num)
                                        (car bg)
                                      (cadr bg)))))
      (push (make-overlay 0 0) hl-sexp-paren-siblings-overlays)
      (overlay-put (car hl-sexp-paren-siblings-overlays) 'face attributes)
      ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
      ;; (overlay-put (car hl-sexp-paren-siblings-overlays) 'priority num)
      (overlay-put (car hl-sexp-paren-siblings-overlays) 'priority nil)
      (cl-decf num))))

(defun hl-sexp-color-update ()
  (setq hl-sexp-background-colors-when-at-del
        (ram-make-highlight-color (face-background 'default) 2000 4000))
  (setq hl-sexp-background-colors
        (ram-make-highlight-color (face-background 'default) 0 8000))
  (setq hl-sexp-mask-leading-space-background-color
        (face-background 'default))

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when hl-sexp-overlays
        (mapc 'delete-overlay hl-sexp-overlays)
        (mapc 'delete-overlay hl-sexp-overlays-when-at-del)
        (mapc 'delete-overlay hl-sexp-mask-whitespace-overlays)
        (mapc 'delete-overlay hl-sexp-paren-overlays)
        (mapc 'delete-overlay hl-sexp-paren-when-at-del-overlays)
        (mapc 'delete-overlay hl-sexp-paren-siblings-overlays)

        (kill-local-variable 'hl-sexp-overlays)
        (kill-local-variable 'hl-sexp-overlays-when-at-del)
        (kill-local-variable 'hl-sexp-mask-whitespace-overlays)
        (kill-local-variable 'hl-sexp-paren-overlays)
        (kill-local-variable 'hl-sexp-paren-when-at-del-overlays)
        (kill-local-variable 'hl-sexp-paren-siblings-overlays)

        (setq hl-sexp-overlays nil)
        (setq hl-sexp-overlays-when-at-del nil)
        (setq hl-sexp-mask-whitespace-overlays nil)
        (setq hl-sexp-paren-overlays nil)
        (setq hl-sexp-paren-when-at-del-overlays nil)
        (setq hl-sexp-paren-siblings-overlays nil)

        (hl-sexp-create-overlays)
        (hl-sexp-create-overlays-when-at-del)
        (hl-sexp-create-overlays-masking-leading-space (* 2 hl-sexp-masking-overlays-number))
        (hl-sexp-create-paren-overlays)
        (hl-sexp-create-paren-when-at-del-overlays)
        (hl-sexp-create-paren-sibling-overlays)
        (let ((hl-sexps-last-point -1)
              (hl-sexps-last-max-point -1)
              (last-at-opening-paren-p nil))
          (hl-sexp-highlight))))))

(defun hl-sexp-start-of-sexp (pt)
  "Start of the s-expression surrounding PT."
  (save-excursion (cadr (syntax-ppss pt))))

(defun hl-sexp-end-of-sexp (pt)
  "End of s-expression that matches beginning point PT."
  (condition-case nil
	  (scan-sexps pt 1)
	(error nil)))

(defun hl-sexp-end-points (pt n)
  "Get beginning and ending points of N depths of s-expressions
surrounding PT."
  (let (results prev next
                (p pt))
    (when hl-sexp-highlight-adjacent
      (cond ((memq (char-before p) '(?\) ?\} ?\] ?\>))
             (setq p (1- p)))
            ((memq (char-after p) '(?\( ?\{ ?\[ ?\<))
             (setq p (1+ p)))))
    (dotimes (i n (nreverse results))
      (when (not (= p 0))
        (setq prev (hl-sexp-start-of-sexp p))
        (when prev
          (setq next (hl-sexp-end-of-sexp prev))
          (when next
            (push (list prev next) results)
            (setq p prev)))))))

(defun hl-sexp-get-siblings-end-points (pt n)
  "Return sibling lists endpoints or nil if none exist."
  (let (end-points)
    (condition-case nil
        (save-excursion
          (goto-char pt)
          (while (> n 0)
            (forward-list)
            (push (list (scan-sexps (point) -1) (point)) end-points)
            (setq n (1- n))))
      (error nil))
    end-points))

;;; handle masking excessive whitespace created by highlighting overlays

(defun hl-sexp-create-overlays-masking-leading-space (count)
  "Create overlays that mask leading spaces for highlighted sexps."
  (let* ((bg hl-sexp-mask-leading-space-background-color)
         (num count)
         attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-mask-leading-space-face))
      (setq attributes (plist-put attributes :background bg))
      (push (make-overlay 0 0) hl-sexp-mask-whitespace-overlays)
      (overlay-put (car hl-sexp-mask-whitespace-overlays) 'face attributes)
      ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
      ;; (overlay-put (car hl-sexp-mask-whitespace-overlays) 'priority num)
      (overlay-put (car hl-sexp-mask-whitespace-overlays) 'priority nil)
      (cl-decf num))
    (setq hl-sexp-mask-whitespace-overlays (nreverse hl-sexp-mask-whitespace-overlays))))

(defun get-leading-space-positions (begin end)
  "Return a seq of alists with match-beginning and match-end for
leading whitespace in the region delimited with BEGIN and END."
  (let ((matches))
    (save-match-data
      (save-excursion
        (goto-char begin)
        (while (search-forward-regexp "^[^[:graph:]]+" end t 1)
          (push `(,(match-beginning 0) . ,(match-end 0)) matches))))
    matches))

(defun get-trailing-space-positions (begin end)
  "Return a seq of alists with match-beginning and match-end for
trailing whitespace in the region delimited with BEGIN and END."
  (let ((matches))
    (save-match-data
      (save-excursion                   ; some comment {exp}:
        (goto-char begin)
        (while (search-forward-regexp "[^;[:space:]][[:space:]]*?\\(?:\\s<+[[:print:]]*\\|[[:space:]]*?\\)\n" (1+ end) t 1)
          (push `(,(1+ (match-beginning 0)) . ,(match-end 0)) matches))))
    matches))

(defun hl-sexp-mask-leading-space (segments)
  "Create overlays used for masking leading spaces for highlighted sexps."
  (let* ((overlays hl-sexp-mask-whitespace-overlays)
         (begin-end (car (last segments)))
         (begin (car begin-end))
         (end (cadr begin-end))
         (lead-segs (nreverse (get-leading-space-positions begin end)))
         (trail-segs (nreverse (get-trailing-space-positions begin end))))
    (while (and overlays lead-segs)
      (let* ((overlay (pop overlays))
             (pos1-pos2 (pop lead-segs))
             (pos1 (car pos1-pos2))
             (pos2 (cdr pos1-pos2)))
        (move-overlay overlay pos1 pos2)))
    (while (and overlays trail-segs)
      (let* ((overlay (pop overlays))
             (pos1-pos2 (pop trail-segs))
             (pos1 (car pos1-pos2))
             (pos2 (cdr pos1-pos2)))
        (move-overlay overlay pos1 pos2)))
    (dolist (ov overlays)
      (move-overlay ov 1 1))))

;;; handle masking excessive whitespace created by highlighting overlays

(defun hl-sexp-create-paren-overlays ()
  "Create overlays for highlighting parens."
  (let* ((fg hl-sexp-paren-colors)
         (bg hl-sexp-paren-background-colors)
         (attr hl-sexp-paren-attributes)
         (count (max (length fg) (length bg) (length attr)))
         (num count)
         attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-paren-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg))))
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg))))
      (pop bg)
      (when (car attr)
        (cl-loop for (key . (val . _rest)) on (car attr) by #'cddr
              do (setq attributes
                       (plist-put attributes key val))))
      (pop attr)
      (push (make-overlay 0 0) hl-sexp-paren-overlays)
      (overlay-put (car hl-sexp-paren-overlays) 'face attributes)
      ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
      ;; (overlay-put (car hl-sexp-paren-overlays) 'priority num)
      (overlay-put (car hl-sexp-paren-overlays) 'priority 1)
      (cl-decf num))
    (setq hl-sexp-paren-overlays (nreverse hl-sexp-paren-overlays))))

(defun hl-sexp-create-paren-when-at-del-overlays ()
  "Create overlays for highlighting delimiters when the point is next to them."
  (let* ((fg hl-sexp-paren-when-at-del-colors)
         (bg hl-sexp-paren-when-at-del-background-colors)
         (attr hl-sexp-paren-when-at-del-attributes)
         (count (max (length fg) (length bg) (length attr)))
         (num count)
         attributes)
    (while (> num 0)
      (setq attributes (face-attr-construct 'hl-sexp-paren-when-at-del-face))
      (when (car fg)
        (setq attributes (plist-put attributes :foreground (car fg))))
      (pop fg)
      (when (car bg)
        (setq attributes (plist-put attributes :background (car bg))))
      (pop bg)
      (when (car attr)
        (cl-loop for (key . (val . _rest)) on (car attr) by #'cddr
              do (setq attributes
                       (plist-put attributes key val))))
      (pop attr)
      (push (make-overlay 0 0) hl-sexp-paren-when-at-del-overlays)
      (overlay-put (car hl-sexp-paren-when-at-del-overlays) 'face attributes)
      ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
      ;; (overlay-put (car hl-sexp-paren-when-at-del-overlays) 'priority num)
      (overlay-put (car hl-sexp-paren-when-at-del-overlays) 'priority 1)
      (cl-decf num))
    (setq hl-sexp-paren-when-at-del-overlays (nreverse hl-sexp-paren-when-at-del-overlays))))

;;; highlight-sexps.el ends here
