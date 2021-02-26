;;; breadcrumbs.el --- Leave visual traces in inactive buffers. -*- lexical-binding: t; -*-

;; credit to Artur Malabarba https://github.com/Malabarba/beacon
;; credit to David Rysdam http://david.rysdam.org/src/emacs/highlight-sexps.el
;; credit to Nikolaj Schumacher http://nschum.de/src/emacs/highlight-parentheses/

(provide 'breadcrumbs)

(defgroup breadcrumbs nil
  "Leave a visual clue where the cursor was."
  :group 'faces
  :group 'matching)

(defvar breadcrumbs--timer nil)

(defun breadcrumbs-set (variable value)
  (set variable value)
  (when (fboundp 'breadcrumbs-color-update)
    (breadcrumbs-color-update)))

(defcustom breadcrumbs-left-foreground-color
  "black"
  "*Color for the left side of the breadcrumb."
  :type '(repeat color)
  :set 'breadcrumbs-set
  :group 'breadcrumbs)

(defcustom breadcrumbs-right-foreground-color
  "black"
  "*Color for the right side of the breadcrumb."
  :type '(repeat color)
  :set 'breadcrumbs-set
  :group 'breadcrumbs)

(defcustom breadcrumbs-left-background-color
  ;; "#5699AF"
  ;; "#3f444a"
  "#a9a1e1"
  "*Background color for the left side of the breadcrumb."
  :type '(repeat color)
  :set 'breadcrumbs-set
  :group 'breadcrumbs)

(defcustom breadcrumbs-right-background-color
  ;; "#A9A1E1"
  "#73797e"
  "*Background color for the right side of the breadcrumb."
  :type '(repeat color)
  :set 'breadcrumbs-set
  :group 'breadcrumbs)

(defcustom breadcrumbs-remove-duration 0.2
  "Time, in seconds, that it takes to remove the breadcrumb."
  :type 'number)

(defcustom breadcrumbs-remove-delay 0.3
  "Time, in seconds, before removing the breadcrumb."
  :type 'number)

(defcustom breadcrumbs-size 100
  "Size of the breadcrumb in characters."
  :type 'number)

(defcustom breadcrumbs-enabled-major-modes
  '(ivy-occur-mode)
  "A list of major-modes where breadcrumbs is enabled."
  :type '(repeat symbol))

(defcustom breadcrumbs-disabled-major-modes
  '(magit-status-mode magit-diff-mode magit-rev-mode rg-mode)
  "A list of major-modes where breadcrumbs is disabled."
  :type '(repeat symbol))

(defcustom breadcrumbs-disabled-buffer-names
  '("*rg*")
  "A list of buffer names where breadcrumbs is disabled."
  :type '(repeat symbol))

(defface breadcrumbs-left-face nil
  "*Face used for the left side of the breadcrumb
Color attributes might be overriden by `breadcrumbs-left-foreground-color' and
`breadcrumbs-left-background-colors'."
  :group 'breadcrumbs)

(defface breadcrumbs-right-face nil
  "*Face used for the right side of the breadcrumb
Color attributes might be overriden by `breadcrumbs-right-foreground-color' and
`breadcrumbs-right-background-colors'."
  :group 'breadcrumbs)

(defvar breadcrumbs-left-overlay nil
  "An overlay for the left side of the breadcrumb.")
(make-variable-buffer-local 'breadcrumbs-left-overlay)

(defvar breadcrumbs-right-overlay nil
  "An overlay for the right side of the breadcrumb.")
(make-variable-buffer-local 'breadcrumbs-right-overlay)


(defun breadcrumbs-create-overlays ()
  "Create an overlay to mark a cursor position in inactive buffers."
  (let (attributes)
    (setq attributes (face-attr-construct 'breadcrumbs-left-face))
    (when breadcrumbs-left-foreground-color
      (setq attributes (plist-put attributes :foreground breadcrumbs-left-foreground-color)))
    (when breadcrumbs-left-background-color
      (setq attributes (plist-put attributes :background breadcrumbs-left-background-color)))
    (setq breadcrumbs-left-overlay (make-overlay 0 0))
    (overlay-put breadcrumbs-left-overlay 'face attributes)
    (overlay-put breadcrumbs-left-overlay 'priority 1))
  (let (attributes)
    (setq attributes (face-attr-construct 'breadcrumbs-right-face))
    (when breadcrumbs-right-foreground-color
      (setq attributes (plist-put attributes :foreground breadcrumbs-right-foreground-color)))
    (when breadcrumbs-right-background-color
      (setq attributes (plist-put attributes :background breadcrumbs-right-background-color)))
    (setq breadcrumbs-right-overlay (make-overlay 0 0))
    (overlay-put breadcrumbs-right-overlay 'face attributes)
    (overlay-put breadcrumbs-right-overlay 'priority 1)))

;;; Internal variables
(defvar breadcrumbs--previous-place nil)
(defvar breadcrumbs--previous-window nil)

(defun breadcrumbs--record-vars ()
  (unless (window-minibuffer-p)
    (setq breadcrumbs--previous-place (point-marker))
    (setq breadcrumbs--previous-window (selected-window))))

(defun breadcrumbs--post-command ()
  "Leave the visual clue when a buffer looses focus."
  (cond
   ;; sanity check
   ((not (markerp breadcrumbs--previous-place)))

   ;; a minibuffer activity, do nothing
   ((and (minibufferp (current-buffer))
         (minibufferp (marker-buffer breadcrumbs--previous-place))))

   ;; a minibuffer is selected for the first time, put visual clues
   ((and (minibufferp (current-buffer))
         (not (minibufferp (marker-buffer breadcrumbs--previous-place))))
    (setq breadcrumbs--previous-place (point-marker))
    (breadcrumbs-put (window-list-1 nil 'nomini 'A)))

   ;; magit status buffer is selected: we change vars to enable breadcrumbs when it closes
   ((and (equal 'magit-status-mode (with-current-buffer (current-buffer) major-mode))
         (not (equal (current-buffer) (marker-buffer breadcrumbs--previous-place))))
    (setq breadcrumbs--previous-place (point-marker))
    (setq breadcrumbs--previous-window (selected-window)))

   ;; this is how we detect when some windows/buffers change
   ((not (equal (current-buffer) (window-buffer breadcrumbs--previous-window)))
    (breadcrumbs-start))

   ;; when switching buffers
   ((not (equal (marker-buffer breadcrumbs--previous-place)
                (current-buffer)))
    (breadcrumbs-start))

   ;; when switching windows
   ((not (equal breadcrumbs--previous-window (selected-window)))
    (breadcrumbs-start))))

(defun breadcrumbs-start ()
  (let* ((windows (seq-filter
                   (lambda (w) (breadcrumbs-enabled-buffer-p (window-buffer w)))
                   (window-list-1 nil 'nomini 'A)))
         ;; (unselected-windows (cdr windows))
         (selected-win (car windows))
         (unselected-windows (seq-filter
                              (lambda (w) (not (equal w selected-win)))
                              (window-list-1 nil 'nomini 'A))))
    (when unselected-windows
      (breadcrumbs-put unselected-windows))
    (when selected-win
      (breadcrumbs-remove selected-win))))

;;;###autoload
(defun breadcrumbs-blink ()
  "Show the visual clue where the point is. Then remove after
`breadcrumbs-remove-delay' seconds."
  (interactive)
  (breadcrumbs-remove (selected-window)))

;;;###autoload
(define-minor-mode breadcrumbs-mode
  "Minor mode to leave the visual clue when a buffers loses focus."
  nil " breadcrumbs" nil
  (if breadcrumbs-mode
      (progn
        (breadcrumbs-create-overlays)
        (add-hook 'pre-command-hook 'breadcrumbs--record-vars nil nil)
        (add-hook 'post-command-hook 'breadcrumbs--post-command nil nil))
    (delete-overlay breadcrumbs-left-overlay)
    (delete-overlay breadcrumbs-right-overlay)
    (kill-local-variable 'breadcrumbs-left-overlay)
    (kill-local-variable 'breadcrumbs-right-overlay)
    (remove-hook 'pre-command-hook 'breadcrumbs--record-vars nil)
    (remove-hook 'post-command-hook 'breadcrumbs--post-command nil)))

;;;###autoload
(define-globalized-minor-mode global-breadcrumbs-mode breadcrumbs-mode
  (lambda () (breadcrumbs-mode 1)))

(defun breadcrumbs-put (windows)
  "Leave a visual clue where the cursor was."
  (dolist (w windows)
    (let ((buf (window-buffer w)))
      (when buf
        (with-current-buffer buf
          (let* ((start (line-beginning-position))
                 (end (line-end-position))
                 (undersized-by (max 0 (- (min breadcrumbs-size (window-width w))
                                          (- end start)))))
            (when (and breadcrumbs-left-overlay
                       breadcrumbs-right-overlay)
              (move-overlay breadcrumbs-left-overlay start (point))
              (move-overlay breadcrumbs-right-overlay (point) end)
              (if (> undersized-by 0)
                  (overlay-put breadcrumbs-right-overlay
                               'after-string
                               (propertize
                                (make-string undersized-by ? )
                                'face `(:background ,breadcrumbs-right-background-color) 'cursor 1000))))))))))

(defun breadcrumbs-remove (selected-win)
  "Remove the visual clue."
  (breadcrumbs-put (list selected-win))
  (with-current-buffer (window-buffer selected-win)
    (when (and breadcrumbs-left-overlay
               breadcrumbs-right-overlay)
      (when (timerp breadcrumbs--timer)
        (cancel-timer breadcrumbs--timer))
      (let* ((ov-l breadcrumbs-left-overlay)
             (ov-r breadcrumbs-right-overlay)
             (beg-l (overlay-start ov-l))
             (end-l (overlay-end ov-l))
             (beg-r (overlay-start ov-r))
             (end-r (overlay-end ov-r))
             (size-l (max 1 (- end-l beg-l)))
             (after-str-r (overlay-get ov-r 'after-string))
             (size-r (+ (length after-str-r) (max 1 (- end-r beg-r))))
             (size-ov (+ size-l size-r))
             (precision 1)
             (interval (/ breadcrumbs-remove-duration 1.0
                          (* size-ov precision)))
             (proportion-l (ceiling (/ (* size-ov precision) size-l 1.0)))
             (proportion-r (ceiling (/ (* size-ov precision) size-r 1.0)))
             (count 0))

        (setq breadcrumbs--timer
              (run-at-time breadcrumbs-remove-delay
                           interval
                           (lambda ()
                             (when (timerp breadcrumbs--timer)
                               (overlay-put ov-r 'after-string nil)
                               (move-overlay ov-l 1 1)
                               (move-overlay ov-r 1 1)
                               (cancel-timer breadcrumbs--timer)))
                           ;; (lambda ()
                           ;;   (when (timerp breadcrumbs--timer)
                           ;;     (overlay-put ov-r 'after-string nil)
                           ;;     (move-overlay ov-l 1 1)
                           ;;     (move-overlay ov-r 1 1)
                           ;;     (cancel-timer breadcrumbs--timer)))
                           ;; (lambda ()
                           ;;   (if  (> (length after-str-r) 0)
                           ;;       (when  (eq 0 (mod count proportion-r))
                           ;;         (setq after-str-r (substring after-str-r 1))
                           ;;         (overlay-put ov-r 'after-string after-str-r))
                           ;;     (if (< beg-r end-r)
                           ;;         (when (eq 0 (mod count proportion-r))
                           ;;           (setq end-r (1- end-r))
                           ;;           (move-overlay ov-r beg-r end-r))))
                           ;;   (if (< beg-l end-l)
                           ;;       (when (eq 0 (mod count proportion-l))
                           ;;         (setq beg-l (1+ beg-l))
                           ;;         (move-overlay ov-l beg-l end-l)))
                           ;;   (when (and
                           ;;          (>= beg-l end-l)
                           ;;          (>= beg-r end-r)
                           ;;          (= (length after-str-r) 0)
                           ;;          (timerp breadcrumbs--timer))
                           ;;     (move-overlay ov-l 1 1)
                           ;;     (move-overlay ov-r 1 1)
                           ;;     (overlay-put ov-r 'after-string nil)
                           ;;     (cancel-timer breadcrumbs--timer))
                           ;;   (setq count (1+ count)))
                           ))))))

(defun breadcrumbs-color-update ()
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when breadcrumbs-left-overlay
        (delete-overlay breadcrumbs-left-overlay)
        (setq breadcrumbs-left-overlay nil)
        (breadcrumbs-create-overlays)
        (breadcrumbs-put)))))

(defun breadcrumbs-enabled-buffer-p (buf)
  "Returns true if breadcrumbs is enabled for BUF buffer."
  (cond
   ((with-current-buffer buf
      (seq-find #'derived-mode-p breadcrumbs-enabled-major-modes)) t)
   ((with-current-buffer buf
      (seq-find #'derived-mode-p breadcrumbs-disabled-major-modes)) nil)
   ((let* ((buffer-name (if (stringp buf) buf (buffer-name buf)))
           (names breadcrumbs-disabled-buffer-names))
      ;; credit to https://emacs.stackexchange.com/a/28689
      (while (and names (not (string-match (car names) buffer-name)))
        (setq names (cdr names)))
      names) nil)
   ((string-match-p "^[[:space:]]" (buffer-name buf)) nil)
   (t t)))
