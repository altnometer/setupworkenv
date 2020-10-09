;;; ram-eshell-completion.el --- Complete eshell history. -*- lexical-binding: t; -*-

(provide 'ram-eshell-completion)

;;* variable definitions

(defvar ovs nil
  "A list of overlays to highlight matches.")
(make-variable-buffer-local 'ovs)

(defvar search-substrings nil
  "A list of substrings to match an eshell history.")
(make-variable-buffer-local 'search-substrings)

(defvar ram-eshell-history nil
  "A list of substrings to match an eshell history.")
(make-variable-buffer-local 'ram-eshell-history)

(defvar ram-eshell-displayed-candidate nil
  "Indicate the number of the currently displayed candidate.")
(make-variable-buffer-local 'ram-eshell-displayed-candidate)

;;* secondary functions

(defun ram-eshell-backward-kill-word (arg)
  "Execute `backward-kill-word' only if not at `eshell-bol'."
  (interactive "p")
  (let ((bol (save-excursion (eshell-bol) (point))))
    (if (= bol (point))
        (message "Cannot `backward-kill-word' when at `eshell-bol'")
      (backward-kill-word arg))))

(defun ram-eshell-reset-candidates (candidates)
  "Set `ram-eshell-history' to CANDIDATES."
  (setq ram-eshell-history candidates)
  (setq ram-eshell-displayed-candidate 0))


(defun ram-eshell-completion--set-vars ()
  "Set ram-eshell-completion-mode variables."
  (if ovs
      (dolist (ov ovs)
        (move-overlay ov 1 1))
    (setq ovs (list (make-overlay 1 1) (make-overlay 1 1)
                    (make-overlay 1 1) (make-overlay 1 1)
                    (make-overlay 1 1) (make-overlay 1 1)
                    (make-overlay 1 1) (make-overlay 1 1)
                    (make-overlay 1 1) (make-overlay 1 1)))
    (dolist (ov ovs)
      (overlay-put ov 'face '((:foreground "green")))))
  (ram-eshell-reset-candidates (delete-dups
                                (ring-elements eshell-history-ring)))
  (setq ram-eshell-displayed-candidate 0)
  (let ((input (buffer-substring-no-properties
                (save-excursion (eshell-bol) (point)) (point-at-eol))))
    (cond
     ((= (seq-length input) 1)
      (setq search-substrings (list input)))
     ((> (seq-length input) 1)
      (setq search-substrings (cons "" (split-string input " " t "[[:space:]]+")))
      (ram-eshell-reset-candidates
       (orderless-filter (string-join search-substrings " ") ram-eshell-history))
      (ram-eshell--insert-candidate))
     (t (setq search-substrings '(""))))))

(defun ram-eshell-completion--delete-vars ()
  "Delete ram-eshell-completion-mode variables."
  (dolist (ov ovs)
    (delete-overlay ov))
  (kill-local-variable 'ovs)
  (kill-local-variable 'search-substrings))

;;;###autoload
(defun ram-eshell-completion-toggle-mode ()
  "Toggle `ram-eshell-completion-mode'."
  (interactive)
  (if ram-eshell-completion-mode
      (let ((search-string (string-join (reverse search-substrings) " ")))
        (ram-eshell-completion-mode -1)
        (eshell-bol)
        (delete-region (point) (point-at-eol))
        (insert search-string)
        (end-of-line))
    (ram-eshell-completion-mode)))

;;** predicates

(defun ram-eshell--backward-kill-word-p ()
  "Test if `this-command' can be interpreted as `backward-kill-word'

with additional tests for relevance to
`ram-eshell-completion-mode'."
  (and (or (eq this-command 'ram-eshell-backward-kill-word)
           (eq this-command 'kill-region)
           (eq this-command 'backward-kill-word)
           (eq this-command 'lispy-backward-kill-word))
       (car search-substrings)))

;;* pre functions

(defun ram-eshell--completion-pre ()
  (cond
   ;; handle "C-c C-c" key press
   ((eq this-command 'eshell-interrupt-process)
    (ram-eshell-completion-mode -1))
   ((ram-eshell--backward-kill-word-p)
    (message "\n**** backward-kill-word")
    (if (string= "" (car search-substrings))
        (setq search-substrings (cons "" (cddr search-substrings)))
      (setq search-substrings (cons "" (cdr search-substrings))))
    (message (format "**** search substr: %s" search-substrings))
    (ram-eshell-reset-candidates (orderless-filter
                                  (string-join search-substrings " ")
                                  (delete-dups
                                   (ring-elements eshell-history-ring)))))
   ;; ((and (eq this-command 'self-insert-command)
   ;;       (not (or (string= (this-command-keys) " ")
   ;;                (null (this-command-keys)))))
   ;;  (message (format "????? typed in char is: %s" (this-command-keys))))
   ))

;;* post functions

(defun ram-eshell--completion-post ()
  (let* ((inserted-char
          (if (eq this-command 'self-insert-command)
              (this-command-keys))))
    (cond
     ((ram-eshell--backward-kill-word-p)
      (ram-eshell--insert-candidate)
      (forward-word))
     ((not (or (string= inserted-char " ")
               (null inserted-char)))
      (ram-eshell--handle-ins-non-spc inserted-char))
     ((string= inserted-char " ")
      (ram-eshell--handle-ins-spc))
     (t "message: unaccounted condition"))))

;;** post secondary functions

(defun ram-eshell--completion-highligth-matches (ovs search-substrings)
  "Highlight matches."
  (when (and ovs search-substrings)
    (eshell-bol)
    (let* ((r (car search-substrings))
          (ovs* (if (not (string= r ""))
                    (cl-labels ((hl (ovs**)
                                    (cond
                                     ((null ovs**) ovs**)
                                     (t (if (re-search-forward r (point-at-eol) t)
                                          (progn
                                            (move-overlay (car ovs**) (match-beginning 0) (match-end 0))
                                            (hl (cdr ovs**)))
                                        ovs**)))))
                      (hl ovs))
                  ovs)))
      (ram-eshell--completion-highligth-matches ovs* (cdr search-substrings)))))

(defun ram-eshell--insert-candidate (&optional n)
  "Insert Nth candidate."
  (eshell-bol)
  (delete-region (point) (point-at-eol))
  (let* ((scrolling-candidates-p (not (null n)))
         (n (or n 0))
         (candidate (nth n ram-eshell-history)))
    (if (and candidate
             (not (and (string= "" (car search-substrings))
                       (null (cdr search-substrings)))))
        (progn
          (insert (if scrolling-candidates-p
                      (format "%s (%s of %s)" candidate
                              (1+ ram-eshell-displayed-candidate)
                              (seq-length ram-eshell-history))
                    (format "%s (%s)" candidate
                            (seq-length ram-eshell-history))))
          (ram-eshell--completion-highligth-matches ovs search-substrings)
          (re-search-forward (car search-substrings) (point-at-eol) t 1))
      (insert (string-join (reverse search-substrings) " "))
      (end-of-line)
      (ram-eshell-reset-candidates (delete-dups
                                    (ring-elements eshell-history-ring))))))

(defun ram-eshell--handle-ins-spc ()
  (message ">>>> empty space")
  (let (candidates)
    ;; (message (format ">>>>>>>> search-substrings: %s" search-substrings))
    ;; (setq candidates (orderless-filter (string-join search-substrings " ") ram-eshell-history))
    (setq candidates ram-eshell-history)
    ;; next set of chars will be appended to this empty str

    (if (not ram-eshell-history)
        (message "no matches")
      (ram-eshell--insert-candidate)
      (forward-word))

    (message (format ">>> search substrings before: %s" search-substrings))
    (when (not (string= "" (car search-substrings)))
      (setq search-substrings (cons "" search-substrings)))
    (message (format ">>> search substrings after: %s" search-substrings))
    (message (format ">>> search string: %s" (string-join search-substrings " * ")))
    (message (format ">>> candidates num: %s" (seq-length candidates)))))

(defun ram-eshell--handle-ins-non-spc (char)
  (message (format "\n**** not empty space, char: %s" char))
  (message (format "**** search substr: %s" search-substrings))
  ;; (when (not (null candidates)) (setq candidates nil))
  (setcar search-substrings (concat (car search-substrings) char))
  (message (format "**** search substr: %s" search-substrings))
  (ram-eshell-reset-candidates (orderless-filter
                                (string-join search-substrings " ") ram-eshell-history))
  (when (or (not (null (cdr search-substrings)))
            (>= (seq-length (car search-substrings)) 2))
    (ram-eshell--insert-candidate))
  ;; (let (candidates)
  ;;   (setq candidates (orderless-filter (string-join search-substrings " ") ram-eshell-history))
  ;;   (ram-eshell--insert-candidate candidates))
  )

;;* keymap

;;** keymap: bindings

(defvar ram-eshell-completion-mode-map nil
  "Keymap for `ram-eshell-completion-mode'")

(progn
  (setq ram-eshell-completion-mode-map (make-sparse-keymap))

  (define-key ram-eshell-completion-mode-map (kbd "M-p") #'ram-eshell-completion-prev)
  (define-key ram-eshell-completion-mode-map (kbd "M-n") #'ram-eshell-completion-next)
  (define-key ram-eshell-completion-mode-map (kbd "<return>") #'ram-eshell-completion-send-input)
  (define-key ram-eshell-completion-mode-map (kbd "<backspace>") #'ram-eshell-completion-delete-backward-char))

;;** keymap|bindings: functions

(defun ram-eshell-completion-prev ()
  "Insert previous history candidate."
  (interactive)
  (setq ram-eshell-displayed-candidate
        (% (1+ ram-eshell-displayed-candidate) (seq-length ram-eshell-history)))
  (ram-eshell--insert-candidate ram-eshell-displayed-candidate))

(defun ram-eshell-completion-next ()
  "Insert next history candidate."
  (interactive)
  (let ((n ram-eshell-displayed-candidate))
    (if (<= n 0)
        (setq ram-eshell-displayed-candidate (1- (seq-length ram-eshell-history)))
        (setq ram-eshell-displayed-candidate (1- ram-eshell-displayed-candidate))))
  (ram-eshell--insert-candidate ram-eshell-displayed-candidate))

(defun ram-eshell-completion-send-input ()
  "Run `eshell-send-input' after additional logic."
  (interactive)
  (let ((input (buffer-substring-no-properties
                eshell-last-output-end (point-at-eol))))
    (eshell-bol)
    (delete-region (point) (point-at-eol))
    (insert (string-trim-right
             input
             "[[:space:]]*\\(?:([[:digit:]]+)\\|([[:digit:]]+[[:space:]]+of[[:space:]]+[[:digit:]]+)\\)?"))
    (eshell-send-input)
    (if ram-eshell-completion-mode
        (ram-eshell-completion--set-vars)
      (ram-eshell-completion-mode 1))
    ))

(defun ram-eshell-completion-delete-backward-char ()
  "Delete char in `search-substrings'."
  (interactive)
    (message "\n**** pre: delete-backward-char")
    (let ((s (car search-substrings))
          new-s)
      (cond
       ((and (string= "" s) (not (null (cdr search-substrings))))
        (setq search-substrings
              (cons (substring (cadr search-substrings)
                               0 (1- (length (cadr search-substrings))))
                    (cddr search-substrings))))
       ((not (string= "" s))
        (setq new-s (substring s 0 (1- (length s))))
        (if (not (string= "" new-s))
            (setq search-substrings (cons new-s (cdr search-substrings)))
          (if (not (null (cdr search-substrings)))
              (setq search-substrings (cdr search-substrings))
            (setq search-substrings '(""))))))
      (ram-eshell-reset-candidates (orderless-filter
                                    (string-join search-substrings " ")
                                    (delete-dups
                                     (ring-elements eshell-history-ring)))))
    (message (format "**** search substr: %s" search-substrings))
    (ram-eshell--insert-candidate))

;;* minor-mode definition

;;;###autoload
(define-minor-mode ram-eshell-completion-mode
  "Minor mode for eshell history completion."
  nil " ram-eshell-completion-mode" ram-eshell-completion-mode-map
  (if ram-eshell-completion-mode
      (progn
        (ram-eshell-completion--set-vars)
        ;; (local-set-key (kbd "<C-tab>") #'ram-eshell--completion-post)
        (local-set-key (kbd "<M-backspace>") #'ram-eshell-backward-kill-word)
        ;; (add-hook 'pre-command-hook 'breadcrumbs--record-vars nil nil)
        (add-hook 'pre-command-hook 'ram-eshell--completion-pre nil t)
        (add-hook 'post-command-hook 'ram-eshell--completion-post nil t))
    (ram-eshell-completion--delete-vars)
    (remove-hook 'pre-command-hook 'ram-eshell--completion-pre t)
    (remove-hook 'post-command-hook 'ram-eshell--completion-post t)
    ;; (remove-hook 'pre-command-hook 'breadcrumbs--record-vars nil)
    ;; (remove-hook 'post-command-hook 'breadcrumbs--post-command nil)
    ))
