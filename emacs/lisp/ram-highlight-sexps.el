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
(defvar-local hl-sexp-background-colors
  ;; '("thistle1" "LightSteelBlue1")
  '("LavenderBlush1" "LightSteelBlue1")
  ;; (ram-make-highlight-color (face-background 'default) 0 8000)
  "*List of colors for highlighting backgrounds in parentheses-like expressions.
The first color is for current parenthesis group, the second is
for its parent.")

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

(defvar-local hl-sexp-background-colors-when-at-del
  ;; '("DarkSeaGreen1" "LightSteelBlue1")
  '("honeydew1" "LightSteelBlue1")
  ;; (ram-make-highlight-color (face-background 'default) 2000 4000)
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

(defvar-local hl-sexp-paren-siblings-overlays nil
  "Overlays for highlighting sibling parentheses.")

(defcustom hl-sexp-highlight-adjacent nil
  "If the point is immediately before or after sexp, highlight it as the inner sexp,
just like show-paren-mode."
  :type '(boolean)
  :group 'highlight-sexps)

;; my window fits 70 lines approximately, it is doubled somewhere in
;; the code (refactor that into leading and trailing parts).
(defcustom hl-sexp-masking-overlays-number 70
  "Determines how many lines is possible to mask."
  :type '(boolean)
  :group 'highlight-sexps)

(defface hl-sexp-face nil
  "*Face used for highlighting sexps.
Color attributes might be overriden by `hl-sexp-colors' and
`hl-sexp-background-colors'."
  :group 'highlight-sexps)

(defvar-local hl-sexp-overlays nil
  "Overlays for highlighting sexps.")

(defvar-local hl-sexp-overlays-when-at-del nil
  "Overlays for highlighting sexps when point is either before delimiter or after.")

;;; vars for highlighting inner and outer sexps parens

(defvar-local hl-sexp-paren-overlays nil
  "Overlays for highlighting parens.")

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

(defvar-local hl-sexp-paren-when-at-del-overlays nil
  "Overlays for highlighting parentheses when the point is next to them.")

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

(defvar-local hl-sexp-mask-whitespace-overlays nil)

(defvar-local hl-sexp-mask-space-background-color
  (let ((bg (face-background 'default)))
    (if (or (not (stringp bg))
            (string-match "\\`unspecified-" bg))
        "#888888"
      bg))
  "*Background overlay face color to mask leading and trailing spaces.")

(defface hl-sexp-mask-space-face nil
  "*Face used for covering leading and trailing whitespace.
Color attributes might be overriden by `hl-sexp-mask-space-background-color'."
  :group 'highlight-sexps)

;; TODO: reconsider:
;;       - there are case when end points do not change sexps in between change
;;         e.g., slurping
(defvar-local hl-sexp-last-point -1
  "The last point for which sexps were highlighted.
This is used in tandem with `hl-sexp-last-max-point' to prevent
repeating highlighting the same sexps in the same context.")

(defvar-local hl-sexp-last-max-point -1
  "The last max point when sexps were highlighted.
This is used in tandem with `hl-sexp-last-point' to prevent
repeating highlighting the same sexps in the same context.")

(defvar-local last-at-opening-paren-p nil
  "True if last delimiter visited was an opening one.")

;;; delimiters

(defvar-local hl-sexp-delimiters "{}()[]<>"
  "A string of open and close delimiters pairs.")

(defvar-local hl-sexp-open-delimiters
  (let ((delims))
    (dotimes (idx (length hl-sexp-delimiters))
      (when (= (% idx 2) 0)
        (setq delims (cons (aref hl-sexp-delimiters idx) delims))))
    delims)
  "A list of open delimiter characters.")

(defvar-local hl-sexp-close-delimiters
  (let ((delims))
    (dotimes (idx (length hl-sexp-delimiters))
      (when (= (% idx 2) 1)
        (setq delims (cons (aref hl-sexp-delimiters idx) delims))))
    delims)
  "A list of close delimiter characters.")

(defun hl-sexp-highlight ()
  "Highlight the nested s-expressions around point"
  (condition-case err
      (when t ;; try running on every post-command
        ;; when slurping, the endpoint do not change but inside sexps change
        ;; (not (and
          ;;         (= (point) hl-sexp-last-point)
          ;;         (= (point-max) hl-sexp-last-max-point)))
        (setq hl-sexp-last-point (point))
        (setq hl-sexp-last-max-point (point-max))
        (let* ((p (point))
               (ppss (syntax-ppss))
               (at-delimiter-p (and (not (nth 3 ppss)) ; not in string
                                    (not (nth 4 ppss)) ; not in comment
                                    (or (memq (char-after p) hl-sexp-open-delimiters)
                                        (memq (char-before p) hl-sexp-close-delimiters))))
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
               (sexps-siblings (when (> (length sexps-for-parens) 1) ; length one means the outer most level of parens
                                 (hl-sexp-get-siblings-end-points (caar sexps-for-parens) hl-sexp-siblings-number)))
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
            (hl-sexp-mask-leading-trailing-space sexps-for-mask))
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
                     (pos2 (cdr sexp)))
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
            (move-overlay ov 1 1))))
    ((debug error) (signal (car err) (cdr err)))))

;;;###autoload
(define-minor-mode ram-highlight-sexps-mode
  "Minor mode to highlight an expanding set of surrounding s-expressions."
  :init-value: nil :lighter " hl-s" :keymap nil
  (if ram-highlight-sexps-mode
      (progn (hl-sexp-create-all-overlays)
             (hl-sexp-highlight)
             (add-hook 'post-command-hook 'hl-sexp-highlight nil t))
    ;; (mapc 'delete-overlay hl-sexp-overlays)
    ;; (mapc 'delete-overlay hl-sexp-overlays-when-at-del)
    ;; (mapc 'delete-overlay hl-sexp-mask-whitespace-overlays)
    ;; (mapc 'delete-overlay hl-sexp-paren-overlays)
    ;; (mapc 'delete-overlay hl-sexp-paren-when-at-del-overlays)
    ;; (mapc 'delete-overlay hl-sexp-paren-siblings-overlays)

    ;; (kill-local-variable 'hl-sexp-overlays)
    ;; (kill-local-variable 'hl-sexp-overlays-when-at-del)
    ;; (kill-local-variable 'hl-sexp-mask-whitespace-overlays)
    ;; (kill-local-variable 'hl-sexp-paren-overlays)
    ;; (kill-local-variable 'hl-sexp-paren-when-at-del-overlays)
    ;; (kill-local-variable 'hl-sexp-paren-siblings-overlays)

    (setq hl-sexp-last-point -1)
    (setq hl-sexp-last-max-point -1)
    (setq last-at-opening-paren-p -1)
    (hl-sexp-hide-all-overlays)
    (remove-hook 'post-command-hook 'hl-sexp-highlight t)
    ))

;;; hide overlays

(defun hl-sexp-hide-all-overlays ()
  "Hide all overlays."
  (mapc (lambda (ov) (move-overlay ov 1 1)) hl-sexp-overlays)
  (mapc (lambda (ov) (move-overlay ov 1 1)) hl-sexp-overlays-when-at-del)
  (mapc (lambda (ov) (move-overlay ov 1 1)) hl-sexp-mask-whitespace-overlays)
  (mapc (lambda (ov) (move-overlay ov 1 1)) hl-sexp-paren-overlays)
  (mapc (lambda (ov) (move-overlay ov 1 1)) hl-sexp-paren-when-at-del-overlays)
  (mapc (lambda (ov) (move-overlay ov 1 1)) hl-sexp-paren-siblings-overlays))

;;; create overlays

(defun hl-sexp-create-all-overlays ()
  "Create needed overlays."
  (hl-sexp-create-overlays)
  (hl-sexp-create-overlays-when-at-del)
  (hl-sexp-create-overlays-masking-leading-space (* 2 hl-sexp-masking-overlays-number))
  (hl-sexp-create-paren-overlays)
  (hl-sexp-create-paren-when-at-del-overlays)
  (hl-sexp-create-paren-sibling-overlays))

(defun hl-sexp-create-overlays ()
  "Create some sexp overlays."
  (when (null hl-sexp-overlays)
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
        (overlay-put (car hl-sexp-overlays) 'priority '(nil . 1))
        ;; (overlay-put (car hl-sexp-overlays) 'priority nil)
        (cl-decf num))
      (setq hl-sexp-overlays (nreverse hl-sexp-overlays)))))

(defun hl-sexp-create-overlays-when-at-del ()
  "Create some sexp overlays used when the point is either before or after a delimiter."
  (when (null hl-sexp-overlays-when-at-del)
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
        (overlay-put (car hl-sexp-overlays-when-at-del) 'priority '(nil . 1))
        ;; (overlay-put (car hl-sexp-overlays-when-at-del) 'priority nil)
        (cl-decf num))
      (setq hl-sexp-overlays-when-at-del (nreverse hl-sexp-overlays-when-at-del)))))

(defun hl-sexp-create-paren-sibling-overlays ()
  "Create overlays for highlighting sibling parentheses."
  (when (null hl-sexp-paren-siblings-overlays)
    (let* ((fg hl-sexp-colors-siblings)
           (bg hl-sexp-background-siblings)
           ;; we need two overlays for one sibling (for opening and closing delimiter)
           (num (* 2 hl-sexp-siblings-number))
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
        (overlay-put (car hl-sexp-paren-siblings-overlays) 'priority '(nil . 2))
        ;; (overlay-put (car hl-sexp-paren-siblings-overlays) 'priority 2)
        (cl-decf num)))))

(defun hl-sexp-color-update ()
  (setq hl-sexp-background-colors-when-at-del
        ;; (ram-make-highlight-color (face-background 'default) 2000 4000)
        '("DarkSeaGreen1" "LightSteelBlue1"))
  (setq hl-sexp-background-colors
        ;; (ram-make-highlight-color (face-background 'default) 0 8000)
        '("thistle1" "LightSteelBlue1"))
  (setq hl-sexp-mask-space-background-color
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
        (let ((hl-sexp-last-point -1)
              (hl-sexp-last-max-point -1)
              (last-at-opening-paren-p nil))
          (hl-sexp-highlight))))))

(defun hl-sexp-start-of-sexp (pt)
  "Start of the s-expression surrounding PT."
  (let ((ppss (syntax-ppss)))
    (save-excursion
      (when (or (nth 3 ppss)
                (nth 4 ppss))
        (goto-char (nth 8 ppss)))
      (cadr (syntax-ppss pt)))))

(defun hl-sexp-end-of-sexp (pt)
  "End of s-expression that matches beginning point PT."
  (condition-case nil
	  (scan-sexps pt 1)
	(error nil)))

(defun hl-sexp-end-points (point depth)
  "Get beginning and ending points of DEPTH depths of s-expressions
surrounding POINT."
  (let (results prev next
                (p point))
    (when hl-sexp-highlight-adjacent
      (cond ((memq (char-before p) hl-sexp-close-delimiters)
             (setq p (1- p)))
            ((memq (char-after p) hl-sexp-open-delimiters)
             (setq p (1+ p)))))
    (dotimes (i depth (nreverse results))
      (when (not (= p 0))
        (setq prev (hl-sexp-start-of-sexp p))
        (when prev
          (setq next (hl-sexp-end-of-sexp prev))
          (when next
            (push (list prev next) results)
            (setq p prev)))))))

(defun hl-sexp-get-siblings-end-points (point siblings-number)
  "Return a list of sibling list endpoints."
  (let* ((limit-forward (/ siblings-number 2))
         (limit-backward (- siblings-number limit-forward))
         (last-point-forward point)     ; where find-forward stopped
         end-points)
    (cl-labels ((find-forward (limit acc)
                  "Return LIMIT sibling lists looking forward."
                  ;; exclude current list
                  (when (and (char-after)
                             (memq (char-after) hl-sexp-open-delimiters))
                    (forward-list))
                  (condition-case nil
                      (forward-list)
                    (error (cons limit (list acc)))
                    (:success (let ((acc (cons (cons (scan-sexps (point) -1) (point)) acc)))
                                (if (> limit 1)
                                    (find-forward (1- limit) acc)
                                  (cons (1- limit) (list acc)))))))
                (find-backward (limit acc)
                  "Return LIMIT sibling lists looking backward."
                  ;; exclude current list
                  (when (and (char-before)
                             (memq  (char-before) hl-sexp-close-delimiters))
                    (backward-list))
                  (condition-case nil
                      (backward-list)
                    (error (cons limit (list acc)))
                    (:success (let ((acc (cons (cons (point) (scan-sexps (point) 1)) acc)))
                                (if (> limit 1)
                                    (find-backward (1- limit) acc)
                                  (cons (1- limit) (list acc))))))))
      (save-excursion
        (goto-char point)
        (cl-destructuring-bind (limit acc) (find-forward limit-forward '())
          (setq limit-forward limit)
          (setq end-points (append acc end-points))
          (setq last-point-forward (or (cdar acc)
                                       last-point-forward))))
      (setq limit-backward (+ limit-backward limit-forward))
      (save-excursion
        (goto-char point)
        (cl-destructuring-bind (limit acc) (find-backward limit-backward '())
          (setq limit-backward limit)
          (setq end-points (append acc end-points))))
      (when (> limit-backward 0)
        ;; found less siblings than wanted,
        ;; continue looking the opposite direction.
        (setq limit-forward limit-backward)
        (save-excursion
          (goto-char last-point-forward)
          (cl-destructuring-bind (limit acc) (find-forward limit-forward '())
            (setq end-points (append acc end-points)))))
      end-points)))

;;; handle masking excessive whitespace created by highlighting overlays

(defun hl-sexp-create-overlays-masking-leading-space (count)
  "Create overlays that mask leading spaces for highlighted sexps."
  (when (null hl-sexp-mask-whitespace-overlays)
    (let* ((bg hl-sexp-mask-space-background-color)
           (num count)
           attributes)
      (while (> num 0)
        (setq attributes (face-attr-construct 'hl-sexp-mask-space-face))
        (setq attributes (plist-put attributes :background bg))
        (setq attributes (plist-put attributes :extend t))
        (push (make-overlay 0 0) hl-sexp-mask-whitespace-overlays)
        (overlay-put (car hl-sexp-mask-whitespace-overlays) 'face attributes)
        ;; setting 'priority to positive integer hides over overlays: lispy, mark region etc.
        ;; (overlay-put (car hl-sexp-mask-whitespace-overlays) 'priority num)
        (overlay-put (car hl-sexp-mask-whitespace-overlays) 'priority '(nil . 2))
        (cl-decf num))
      (setq hl-sexp-mask-whitespace-overlays (nreverse hl-sexp-mask-whitespace-overlays)))))

(defun get-leading-space-positions (begin end)
  "Return a seq of alists with match-beginning and match-end for
leading whitespace in the region delimited with BEGIN and END."

  ;; FIXME: a rude approximation of search limits by begin and end
  ;; fix it if you wish, but it seems not worth the time.
  (let ((begin (max begin (or (ignore-error (error user-error)
                                (save-excursion
                                  (next-line (- hl-sexp-masking-overlays-number))
                                  (point)))
                              (point-min))))
        (end
         ;; if END point is located before (point)
         ;; choose (point) instead, otherwise #'search-forward-regexp
         ;; will not work
         (max (point)
              (min end
                   ;; get point at either
                   ;;   - current location plus max possible overlays
                   ;;   - point-max
                   (or (ignore-error (error user-error)
                         (save-excursion
                           (next-line hl-sexp-masking-overlays-number)
                           (point)))
                       (point-max)))))
        (ov-counter 0)
        matches)
    (save-match-data
      (save-excursion
        (goto-char begin)
        (push (cons (point-at-bol) (point)) matches)
        (while (and (search-forward-regexp "^[^[:graph:]]+" end t 1)
                    (<= ov-counter hl-sexp-masking-overlays-number))
          (push `(,(match-beginning 0) . ,(match-end 0)) matches)
          (setq ov-counter (1+ ov-counter)))))
    matches))

(defun ram-hl-sexps-get-trailing-space-positions (begin end)
  "Return a seq of alists with match-beginning and match-end for
trailing whitespace in the region delimited with BEGIN and END."

  ;; FIXME: a rude approximation of search limits by begin and end
  ;; fix it if you wish, but it seems not worth the time.
  (let ((begin (max begin (or (ignore-error (error user-error)
                                (save-excursion
                                  (next-line (- hl-sexp-masking-overlays-number))
                                  (point)))
                              (point-min))))
        (end (min end
                  (or (ignore-error (error user-error)
                        (save-excursion
                          (next-line hl-sexp-masking-overlays-number)
                          (point)))
                      (point-max))))
        (ov-counter 0)
        matches)
    (save-match-data
      (save-excursion
        (goto-char begin)
        ;; !!! the following regexp fails:
        ;; 1. attempts to work only for ';' comments
        ;; 2. fails when ';' in string,
        ;; (while (search-forward-regexp "[^;[:space:]][[:space:]]*?\\(?:\\s<+[[:print:]]*\\|[[:space:]]*?\\)\n" (1+ end) t 1)
        ;;   (push `(,(1+ (match-beginning 0)) . ,(match-end 0)) matches))
        (while (and (search-forward-regexp "[[:space:]]*?\n" (1+ end) t 1)
                    (<= ov-counter hl-sexp-masking-overlays-number))
          (push (cons (match-beginning 0) (match-end 0)) matches)
          (setq ov-counter (1+ ov-counter)))))
    matches))

;; This implementation is too slow, seems like calling (syntax-ppss) at
;; every step in 'while' is too expensive
;; Also, limit the number of iteration either by
;; - num of available overlays
;; - num of visible lines
;; - END argument

;; (defun ram-hl-sexps-get-trailing-space-positions (begin end)
;;   "Construct for `hl-sexp-mask-whitespace-overlays'
;;  match-beginning and match-end for
;; trailing whitespace in the region delimited with BEGIN and END."
;;   (let ((matches))
;;     (save-excursion
;;       (goto-char begin)
;;       (while (< (point) end)
;;         (end-of-line)
;;         ;; inline comments are masked over.
;;         ;; but if there nothing but a comment on the line, do not mask it over:
;;         (when (not (save-excursion (back-to-indentation) (forward-char) (nth 4 (syntax-ppss))))
;;          ;; while in inline comment:
;;          (while (nth 4 (syntax-ppss))
;;            ;; go back
;;            (backward-char))
;;          ;; while looking back at whitespace:
;;          (while
;;              (eq (char-before) (string-to-char " "))
;;            ;; go back
;;            (backward-char)))
;;         (push (cons (point) (1+ (point-at-eol))) matches)
;;         (when (not (eobp))
;;           (next-line)
;;           (beginning-of-line))))
;;     matches))

;; Works but still is too slow
;; (defun ram-hl-sexps-get-trailing-space-positions (begin end)
;;   "Construct for `hl-sexp-mask-whitespace-overlays'

;;  match-beginning and match-end for
;; trailing whitespace in the region delimited with BEGIN and END."
;;   (let ((begin (max (window-start) begin))
;;         (end (min (window-end) end))
;;         (ov-counter 0)
;;         matches)
;;     (save-excursion
;;       (goto-char begin)
;;       (while (and (< (point) end)
;;                   (< ov-counter (/ hl-sexp-masking-overlays-number 2)))
;;         (push (cons (point-at-eol) (1+ (point-at-eol))) matches)
;;         (setq ov-counter (1+ ov-counter))
;;         (if (eq (point-at-eol) (point-max))
;;             (end-of-line)
;;           (next-line)))
;;       )
;;     matches))

;; TODO: consider putting the highlighting overlays only over visible
;; part of the line. This way, you will not need to mask anything.
(defun hl-sexp-mask-leading-trailing-space (segments)
  "Hide leading and trailing spaces for highlighted sexps."
  (let* ((overlays hl-sexp-mask-whitespace-overlays)
         (begin-end (car (last segments)))
         (begin (car begin-end))
         (end (cadr begin-end))
         (lead-segs (nreverse (get-leading-space-positions begin end)))
         (trail-segs (nreverse (ram-hl-sexps-get-trailing-space-positions begin end))))
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
  (when (null hl-sexp-paren-overlays)
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
        (overlay-put (car hl-sexp-paren-overlays) 'priority 3)
        (cl-decf num))
      (setq hl-sexp-paren-overlays (nreverse hl-sexp-paren-overlays)))))

(defun hl-sexp-create-paren-when-at-del-overlays ()
  "Create overlays for highlighting delimiters when the point is next to them."
  (when (null hl-sexp-paren-when-at-del-overlays)
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
        (overlay-put (car hl-sexp-paren-when-at-del-overlays) 'priority 3)
        (cl-decf num))
      (setq hl-sexp-paren-when-at-del-overlays (nreverse hl-sexp-paren-when-at-del-overlays)))))

;;; highlight-sexps.el ends here
