;;; ram-cursor.el --- Adjust the cursor to the context. -*- lexical-binding: t -*-

;; credit to:
;; Filename: eyedropper.el
;; Description: Pick foreground and background colors at cursor or pointer.
;; Author: Drew Adams
;; Maintainer: Drew Adams (concat "drew.adams" "@" "oracle" ".com")
;; Copyright (C) 2006-2018, Drew Adams, all rights reserved.
;; URL: https://www.emacswiki.org/emacs/download/eyedropper.el


(provide 'ram-cursor)

(defvar ram-cursor-default-type 'box)
(defvar ram-cursor-type-bar '(bar . 3))

(defvar ram-cursor-blink-delay .3)
(defvar ram-cursor-blink-interval .1)
(defvar ram-cursor-blink-times 1)

(setq blink-cursor-delay ram-cursor-blink-delay)
(setq blink-cursor-interval ram-cursor-blink-interval)
(setq blink-cursor-blinks ram-cursor-blink-times)
(setq blink-cursor-alist `((box . ,ram-cursor-type-bar) (bar . box)))

(defvar ram-cursor--timer nil)

(defvar ram-cursor-last-point -1
  "The last point for which the cursor was adjusted to the context.
This is used in tandem with `ram-cursor-last-max-point' to prevent
unnecessary function calls.")
(make-variable-buffer-local 'ram-cursor-last-point)

(defvar ram-cursor-last-max-point -1
  "The last max point when the cursor was adjusted to the context.
This is used in tandem with `ram-cursor-last-point' to prevent
unnecessary function calls.")
(make-variable-buffer-local 'ram-cursor-last-max-point)

(defun ram-cursor-record-defaults ()
  "Record variable values upon the mode activation."
  (setq ram-cursor-default-type cursor-type))

(defvar blink-cursor-end nil
  "Hold custom `blink-cursor-end' command.")
(make-variable-buffer-local 'blink-cursor-end)

(fset 'blink-cursor-end
      (lambda ()
        "This is just a copy of `blink-cursor-end' with resetting `cursor-type' at the end."
        (remove-hook 'pre-command-hook 'blink-cursor-end)
        (internal-show-cursor nil t)
        (when blink-cursor-timer
          (cancel-timer blink-cursor-timer)
          (setq blink-cursor-timer nil)
          (setq cursor-type ram-cursor-type-bar))))

(defun ram-cursor-blink ()
  "Imitate blinking by changing cursor type to 'box and then to 'bar."
  (when (get-buffer-window)
    (if (< 1 (abs (- (point) ram-cursor-last-point)))
        (progn (setq cursor-type 'box)
               (blink-cursor-mode 1))
      (setq cursor-type ram-cursor-type-bar))))


(defun ram-cursor-adjust-style ()
  "Adjust cursor style to context."
  (blink-cursor-mode -1)
  (when
      (not (and
            (= (point) ram-cursor-last-point)
            (= (point-max) ram-cursor-last-max-point)))
    (setq ram-cursor-last-max-point (point-max))
    (let* ((del-open '(?\( ?\{ ?\[ (point)))
           (del-close '(?\) ?\} ?\]))
           (char-b (char-before (point)))
           (char-a (char-after (point)))
           (after-close-del (memq char-b del-close))
           (after-open-del (memq char-b del-open))
           (before-open-del (memq char-a del-open))
           (before-close-del (memq char-a del-close)))

      (cond
       (before-close-del (ram-cursor-blink))
       (before-open-del (ram-cursor-blink))
       (t (setq cursor-type ram-cursor-default-type))))
    (setq ram-cursor-last-point (point))))

;;;###autoload
(define-minor-mode ram-cursor-mode
  "Minor mode to adjust the cursor to the context."
  nil " ram-cursor" nil
  (if ram-cursor-mode
      (progn
        (add-hook 'post-command-hook 'ram-cursor-adjust-style nil t))
    (kill-local-variable 'ram-cursor-last-point)
    (kill-local-variable 'ram-cursor-last-max-point)
    (remove-hook 'post-command-hook 'ram-cursor-adjust-style nil)))


;;;###autoload
(define-globalized-minor-mode global-ram-cursor-mode ram-cursor-mode
  (lambda () (ram-cursor-mode 1)))

;;; ram-cursor.el ends here
