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

;;* secondary functions

(defun ram-eshell-backward-kill-word (arg)
  "Execute `backward-kill-word' only if not at `eshell-bol'."
  (interactive "p")
  (let ((bol (save-excursion (eshell-bol) (point))))
    (if (= bol (point))
        (message "Cannot `backward-kill-word' when at `eshell-bol'")
      (backward-kill-word arg))))

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
  (setq ram-eshell-history (delete-dups
                            (ring-elements eshell-history-ring)))
  (let ((input (buffer-substring-no-properties
                (save-excursion (eshell-bol) (point)) (point-at-eol))))
    (cond
     ((= (seq-length input) 1)
      (setq search-substrings (list input)))
     ((> (seq-length input) 1)
      (setq search-substrings (cons "" (split-string input " " t "[[:space:]]+")))
      (setq ram-eshell-history (orderless-filter (string-join search-substrings " ") ram-eshell-history))
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

(defun ram-eshell--delete-backward-char-p ()
    "Test if `this-command' can be interpreted as `delete-backward-char'

with additional tests for relevance to
`ram-eshell-completion-mode'."
    (and (or (eq this-command 'delete-backward-char)
          (eq this-command 'lispy-delete-backward))
      (not (string-empty-p (car search-substrings)))))

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
   ;; handle <return> key press
   ((eq this-command 'eshell-send-input)
    (ram-eshell-completion-mode -1)
    ;; (funcall this-command)
    )
   ;; handle "C-c C-c" key press
   ((eq this-command 'eshell-interrupt-process)
    (ram-eshell-completion-mode -1)
    ;; (funcall this-command)
    )
   ;; handle <backspace>
   ((ram-eshell--delete-backward-char-p)
    (message "\n**** pre: delete-backward-char")
    (when (not (string= "" (car search-substrings)))
      (setcar search-substrings
              (substring (car search-substrings)
                         0 (1- (length (car search-substrings))))))
    (setq ram-eshell-history (orderless-filter
                              (string-join search-substrings " ")
                              (delete-dups
                               (ring-elements eshell-history-ring))))
    (message (format "**** search substr: %s" search-substrings)))
   ((ram-eshell--backward-kill-word-p)
    (message "\n**** backward-kill-word")
    (if (string= "" (car search-substrings))
        (setq search-substrings (cons "" (cddr search-substrings)))
      (setq search-substrings (cons "" (cdr search-substrings))))
    (message (format "**** search substr: %s" search-substrings))
    (setq ram-eshell-history (orderless-filter
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
     ((ram-eshell--delete-backward-char-p)
      (ram-eshell--insert-candidate))
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

(defun ram-eshell--insert-candidate ()
  "Handle insertion of candidates."
  (eshell-bol)
  (delete-region (point) (point-at-eol))
  (if (and (car ram-eshell-history)
           (not (and (string= "" (car search-substrings))
                     (null (cdr search-substrings)))))
      (progn
        (insert (format "%s (%s)" (car ram-eshell-history) (seq-length ram-eshell-history)))
        (ram-eshell--completion-highligth-matches ovs search-substrings)
        (re-search-forward (car search-substrings) (point-at-eol) t 1))
    (insert (string-join (reverse search-substrings) " "))
    (end-of-line)
    (setq ram-eshell-history (delete-dups
                              (ring-elements eshell-history-ring)))))

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
  (setq ram-eshell-history (orderless-filter (string-join search-substrings " ") ram-eshell-history))
  (when (or (not (null (cdr search-substrings)))
            (>= (seq-length (car search-substrings)) 2))
    (ram-eshell--insert-candidate))
  ;; (let (candidates)
  ;;   (setq candidates (orderless-filter (string-join search-substrings " ") ram-eshell-history))
  ;;   (ram-eshell--insert-candidate candidates))
  )

;;* minor-mode definition

;;;###autoload
(define-minor-mode ram-eshell-completion-mode
  "Minor mode for eshell history completion."
  nil " ram-eshell-completion-mode" nil
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
