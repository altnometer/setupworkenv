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
                eshell-last-output-end (point-at-eol))))
    (cond
     ((= (seq-length input) 1)
      (setq search-substrings (list input)))
     ((> (seq-length input) 1)
      (setq search-substrings (cons "" (split-string input " " t "[[:space:]]+")))
      (ram-eshell-reset-candidates
       (orderless-filter (string-join search-substrings " ") ram-eshell-history))
      (ram-eshell--insert-candidate))
     (t (setq search-substrings (list ""))))))

(defun ram-eshell-completion--trim-input-right ()
  "Replace input with removed counters of candidates."
  (let ((point (point))
        (input (buffer-substring-no-properties
                eshell-last-output-end (point-at-eol))))
    (eshell-bol)
    (delete-region (point) (point-at-eol))
    (insert (string-trim-right
             input
             "[[:space:]]*\\(?:([[:digit:]]+)\\|([[:digit:]]+[[:space:]]+of[[:space:]]+[[:digit:]]+)\\)?"))
    (goto-char point)))

(defun ram-eshell-completion--continues-p ()
  "True if we are in the middle of searching for a completion candidate."
  (not (and (string= "" (car search-substrings))
            (null (cdr search-substrings)))))

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

;;* pre functions

(defun ram-eshell--completion-pre ())

;;* post functions

(defun ram-eshell--completion-post ()
  (let* ((inserted-char
          (if (eq this-command 'self-insert-command)
              (this-command-keys))))
    (cond
     ((not (or (string= inserted-char " ")
               (null inserted-char)))
      (ram-eshell--handle-ins-non-spc inserted-char))
     ((string= inserted-char " ")
      (ram-eshell--handle-ins-spc))
     ((eq this-command 'eshell-interrupt-process)
      ;; (delete-region (eshell-bol) (point-at-eol))
      (ram-eshell-completion--set-vars))
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
  (delete-region eshell-last-output-end (point-at-eol))
  (let* ((scrolling-candidates-p (not (null n)))
         (n (or n 0))
         (candidate (nth n ram-eshell-history)))
    (if (and candidate (ram-eshell-completion--continues-p))
        (progn
          (insert (if scrolling-candidates-p
                      (format "%s (%s of %s)" candidate
                              (1+ ram-eshell-displayed-candidate)
                              (seq-length ram-eshell-history))
                    (format "%s (%s)" candidate
                            (seq-length ram-eshell-history))))
          (ram-eshell--completion-highligth-matches ovs search-substrings)
          (re-search-forward (car search-substrings) (point-at-eol) t 1))
      (when (ram-eshell-completion--continues-p)
        (insert (format "%s (%s)"
                        (string-join (reverse search-substrings) " ")
                        (propertize "No matches" 'face '((:foreground "red")))))
        (beginning-of-line)
        (re-search-forward (car search-substrings) (point-at-eol) t)
        (ram-eshell-reset-candidates (orderless-filter
                                      (string-join search-substrings " ") ram-eshell-history)))
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
  (define-key ram-eshell-completion-mode-map (kbd "<backspace>") #'ram-eshell-completion-delete-backward-char)
  (define-key ram-eshell-completion-mode-map (kbd "<M-backspace>") #'ram-eshell-completion-backward-kill-word)
  (define-key ram-eshell-completion-mode-map (kbd "C-f") #'ram-eshell-completion-forward-char)
  (define-key ram-eshell-completion-mode-map (kbd "C-b") #'ram-eshell-completion-backward-char)
  (define-key ram-eshell-completion-mode-map (kbd "M-f") #'ram-eshell-completion-forward-word)
  (define-key ram-eshell-completion-mode-map (kbd "M-b") #'ram-eshell-completion-backward-word)
  (define-key ram-eshell-completion-mode-map (kbd "C-a") #'ram-eshell-completion-beginning-of-line)
  (define-key ram-eshell-completion-mode-map (kbd "C-e") #'ram-eshell-completion-end-of-line))

;;** keymap|bindings: functions

(defun ram-eshell-completion-prev ()
  "Insert previous history candidate."
  (interactive)
  (if (not (ram-eshell-completion--continues-p))
      (progn (setq this-command 'eshell-previous-matching-input-from-input)
             (eshell-previous-matching-input-from-input 1))
    (setq ram-eshell-displayed-candidate
          (% (1+ ram-eshell-displayed-candidate) (seq-length ram-eshell-history)))
    (ram-eshell--insert-candidate ram-eshell-displayed-candidate)))

(defun ram-eshell-completion-next ()
  "Insert next history candidate."
  (interactive)
  (if (not (ram-eshell-completion--continues-p))
      ;; (eshell-next-matching-input-from-input 1)
      (progn (setq this-command 'eshell-next-matching-input-from-input)
             (eshell-next-matching-input-from-input 1))
    (if (<= ram-eshell-displayed-candidate 0)
        (setq ram-eshell-displayed-candidate (1- (seq-length ram-eshell-history)))
        (setq ram-eshell-displayed-candidate (1- ram-eshell-displayed-candidate))))
  (ram-eshell--insert-candidate ram-eshell-displayed-candidate))

(defun ram-eshell-completion-send-input ()
  "Run `eshell-send-input' after additional logic."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (eshell-send-input)
  (if ram-eshell-completion-mode
      (ram-eshell-completion--set-vars)
    (ram-eshell-completion-mode 1)))

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

(defun ram-eshell-completion-backward-kill-word ()
  "Remove element in `search-substrings'."
  (interactive)
  (message "\n**** backward-kill-word")
  (if (string= "" (car search-substrings))
      (setq search-substrings (cons "" (cddr search-substrings)))
    (setq search-substrings (cons "" (cdr search-substrings))))
  (message (format "**** search substr: %s" search-substrings))
  (ram-eshell-reset-candidates (orderless-filter
                                (string-join search-substrings " ")
                                (delete-dups
                                 (ring-elements eshell-history-ring))))
  (ram-eshell--insert-candidate)
  (forward-word))

(defun ram-eshell-completion-forward-char ()
  "Run `forward-char' after disabling `ram-eshell-completion-mode'."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (ram-eshell-completion-mode -1)
  (forward-char))

(defun ram-eshell-completion-backward-char ()
  "Run `backward-char' after disabling `ram-eshell-completion-mode'."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (ram-eshell-completion-mode -1)
  (backward-char))

(defun ram-eshell-completion-forward-word ()
  "Run `forward-word' after disabling `ram-eshell-completion-mode'."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (ram-eshell-completion-mode -1)
  (forward-word))

(defun ram-eshell-completion-backward-word ()
  "Run `backward-word' after disabling `ram-eshell-completion-mode'."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (ram-eshell-completion-mode -1)
  (backward-word))

(defun ram-eshell-completion-beginning-of-line ()
  "Run `eshell-bol' after disabling `ram-eshell-completion-mode'."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (ram-eshell-completion-mode -1)
  (eshell-bol))

(defun ram-eshell-completion-beginning-of-line ()
  "Run `move-end-of-line' after disabling `ram-eshell-completion-mode'."
  (interactive)
  (ram-eshell-completion--trim-input-right)
  (ram-eshell-completion-mode -1)
  (move-end-of-line))

;;* minor-mode definition

;;;###autoload
(define-minor-mode ram-eshell-completion-mode
  "Minor mode for eshell history completion."
  nil " ram-eshell-completion-mode" ram-eshell-completion-mode-map
  (if ram-eshell-completion-mode
      (progn
        (ram-eshell-completion--set-vars)
        ;; (local-set-key (kbd "<C-tab>") #'ram-eshell--completion-post)
        ;; (add-hook 'pre-command-hook 'breadcrumbs--record-vars nil nil)
        (add-hook 'pre-command-hook 'ram-eshell--completion-pre nil t)
        (add-hook 'post-command-hook 'ram-eshell--completion-post nil t))
    (ram-eshell-completion--delete-vars)
    (remove-hook 'pre-command-hook 'ram-eshell--completion-pre t)
    (remove-hook 'post-command-hook 'ram-eshell--completion-post t)
    ;; (remove-hook 'pre-command-hook 'breadcrumbs--record-vars nil)
    ;; (remove-hook 'post-command-hook 'breadcrumbs--post-command nil)
    ))
