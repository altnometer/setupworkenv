;;; ram-eshell-completion.el --- Complete eshell history. -*- lexical-binding: t; -*-

(provide 'ram-eshell-completion)

;;* variables

;;** variables: eshell-mode

;;** variables: ram-eshell-completion-mode

(defface ram-eshell-completion--hl-match-face
  '((t :foreground "black"
       :background "aquamarine"
       ;; :weight bold
       ;; :underline t
       ))
  "Face for highlighting matches in displayed candidates."
  :group 'ram-eshell--completion)

(defvar ov-candidates nil
  "An overlay to display matching candidates.")
(make-variable-buffer-local 'ov-candidates)

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

(defvar ram-eshell-message-no-matches "No matches"
  "Display this string when no completion candidates found.")

(defvar ram-eshell-trim-right-regex
  (concat
   "[[:space:]]*"
   "\\|"
   "\\(?:([[:digit:]]+)\\|([[:digit:]]+[[:space:]]+of[[:space:]]+[[:digit:]]+)\\)?"
   "\\|"
   (format "\\(?:(%s)\\)?" ram-eshell-message-no-matches))
  "Regex used to trim the input from `ram-eshell-completion-mode' messages.")

(defvar ram-eshell-num-of-displayed-candidates 6
  "Restricts number of displayed completion candidates.")

(defvar ram-eshell-completion--length-of-displayed-candidate 60
  "Length of displayed candidate string.")

(defvar ram-eshell-completion--max-length-word 16
  "Words over this length are truncated.")

(defvar ram-eshell-completion--trancate-symbol "…"
  "A substitute string to indicate truncated parts.")

;;* secondary functions

(defun ram-eshell-reset-candidates (candidates)
  "Set `ram-eshell-history' to CANDIDATES."
  (setq ram-eshell-history candidates)
  (setq ram-eshell-displayed-candidate 0))


(defun ram-eshell-completion--set-vars ()
  "Set ram-eshell-completion-mode variables."
  (if ov-candidates
      (progn
        (overlay-put ov-candidates 'after-string nil)
        (move-overlay ov-candidates 1 1))
    (setq ov-candidates (make-overlay 1 1)))
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
     (t (setq search-substrings (list ""))))))

(defun ram-eshell-completion--trim-input-right ()
  "Replace input with removed counters of candidates."
  (let ((point (point))
        (input (buffer-substring-no-properties
                (eshell-bol) (point-at-eol))))
    (delete-region (eshell-bol) (point-at-eol))
    (insert (string-trim-right
             input
             ram-eshell-trim-right-regex))
    (goto-char point)))

(defun ram-eshell-completion--continues-p ()
  "True if we are in the middle of searching for a completion candidate."
  (not (and (string= "" (car search-substrings))
            (null (cdr search-substrings)))))

(defun ram-eshell-completion--delete-vars ()
  "Delete ram-eshell-completion-mode variables."
  (dolist (ov ovs)
    (delete-overlay ov))
  (delete-overlay ov-candidates)
  (kill-local-variable 'ovs)
  (kill-local-variable 'search-substrings))

(defun ram-eshell-completion--hide-overlay ()
  "Hide overlay."
  (overlay-put ov-candidates 'after-string nil)
  (move-overlay ov-candidates 1 1))

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

;; how to get a trace from commands in 'post-command-hook
;; credit to Johan Bockgård
;; https://lists.gnu.org/archive/html/emacs-devel/2010-07/msg01410.html
(defadvice ram-eshell--completion-post (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

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

(defun ram-eshell--completion-highlight-string-matches (string search-substrings)
  "Highlight SEARCH-SUBSTRINGS matches in STRING."
  (if search-substrings
    (let* ((r (car search-substrings)))
      (when (not (string= r ""))
        (cl-labels ((hl (start)
                        (when (string-match r string start)
                          (add-text-properties
                           (match-beginning 0) (match-end 0)
                           '(face ram-eshell-completion--hl-match-face) string)
                          (hl (match-end 0)))))
          (hl 0)))
      (ram-eshell--completion-highlight-string-matches string (cdr search-substrings)))
    string))

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

(defun ram-eshell-completion--crop-word (word substr)
  "Crop WORD."
  (let ((front-segment (substring word 0 (string-match substr word)))
        beginning-part middle-part
        new-word)
    (when (and (string-match (rx line-start
                           (zero-or-more (not alnum))
                           (one-or-more (or alnum "-"))) word)
             ;; do not include if substr in the match
             (not (s-matches-p substr (match-string 0 word))))
      (setq beginning-part (match-string 0 front-segment)))
    (when (string-match (rx-to-string
                         `(: (group (zero-or-more (or alnum "-"))
                                    (zero-or-more (not alnum))
                                    ,substr
                                    (zero-or-more (not alnum))
                                    (zero-or-more (or alnum "-")))
                             (group (zero-or-more anychar))
                             line-end) t)
                        word)
      (setq middle-part (match-string 1 word))
      (setq new-word (string-join (remove nil (vector beginning-part middle-part))
                                  ram-eshell-completion--trancate-symbol)))
    (when (and new-word
             (not (string= "" (match-string 2 word))))
        (setq new-word (concat new-word ram-eshell-completion--trancate-symbol))
      (string-match substr new-word)
      (add-text-properties
       (match-beginning 0) (match-end 0)
       '(face ram-eshell-completion--hl-match-face) new-word)
      new-word)))

(defun ram-eshell-completion--resize-list (words length resized-p)
  "Remove words that do not contain `ram-eshell-completion--hl-match-face'.
  Keep removing until LENGTH is less than
  `ram-eshell-completion--length-of-displayed-candidate'."
  (if (or (null words)
           (<= length ram-eshell-completion--length-of-displayed-candidate))
      words
    (let* ((word (car words))
          (match-begining
           (text-property-any 0 (length word) 'face 'ram-eshell-completion--hl-match-face word))
          match-end)
      (if match-begining
          (if (>= (length word) ram-eshell-completion--max-length-word)
              (progn
                ;; (message "matched: %s" word)
                (setq match-end
                     (text-property-not-all match-begining (length word) 'face 'ram-eshell-completion--hl-match-face word))
               (cons
                ;; (ram-eshell-completion--crop-word word match-begining match-end)
                (ram-eshell-completion--crop-word word (substring word match-begining match-end))
                (ram-eshell-completion--resize-list (cdr words) length nil)))
            ;; (message "match-begining: %s" match-begining)
            ;; (message "match-end: %s" match-end)
            (cons word (ram-eshell-completion--resize-list (cdr words) length nil)))
        ;; (progn
        ;;     (setq match-end
        ;;           (text-property-not-all match-begining (length word) 'face 'ram-eshell-completion--hl-match-face word))
        ;;     (message "matched: %s" word)
        ;;     (message "match-begining: %s" match-begining)
        ;;     (message "match-end: %s" match-end)
        ;;     (cons word (ram-eshell-completion--resize-list (cdr words) length nil)))
        (if resized-p
            (ram-eshell-completion--resize-list (cdr words) (- length (length word)) resized-p)
          (let ((replacement ram-eshell-completion--trancate-symbol))
            (cons replacement
                  (ram-eshell-completion--resize-list
                   (cdr words)
                   (- length (- (length word) (length replacement))) t))))))))

(defun ram-eshell-completion--resize-str (str)
  "Trim string to `ram-eshell-completion--length-of-displayed-candidate'."
  (let* ((words (reverse (split-string str)))
         (num-spaces (1- (length words)))
         (str-len-no-spaces (- (length str) num-spaces)))
    (string-join (reverse
                  (ram-eshell-completion--resize-list words str-len-no-spaces nil)) " ")))

(defun ram-eshell--make-display-candidates-string (candidates)
  "Make a string of candidates that is displayed to the user."
  (let ((candidates (if (> (length candidates) ram-eshell-num-of-displayed-candidates)
                        (butlast candidates (- (length candidates) ram-eshell-num-of-displayed-candidates))
                      candidates)))
    (concat
     (format " (%s of %s)\n" (1+ ram-eshell-displayed-candidate)
             (length ram-eshell-history))
     (mapconcat (lambda (str)
                  (let ((str (ram-eshell--completion-highlight-string-matches
                              (concat (string-trim str))
                              search-substrings)))
                    (if (> (length str) ram-eshell-completion--length-of-displayed-candidate)
                        (ram-eshell-completion--resize-str str)
                      str)))
                candidates "\n"))))

(defun ram-eshell--display-candidates ()
  "Display completion candidates. "
  (if ram-eshell-history
      (let ((str (ram-eshell--make-display-candidates-string ram-eshell-history)))
        ;; (overlay-put ov-candidates 'after-string nil)
        (move-overlay ov-candidates (point-at-eol) (point-at-eol))
        (overlay-put ov-candidates 'after-string str)
        (put-text-property 0 1 'cursor 0 str))
    (ram-eshell-completion--hide-overlay)))

(defun ram-eshell--insert-candidate (&optional n)
  "Insert Nth candidate."
  (delete-region (eshell-bol) (point-at-eol))
  (let* ((scrolling-candidates-p (not (null n)))
         (n (or n 0))
         (candidate (nth n ram-eshell-history)))
    (if (and candidate (ram-eshell-completion--continues-p))
        (progn
          ;; (insert (if scrolling-candidates-p
          ;;             (format "%s (%s of %s)" candidate
          ;;                     (1+ ram-eshell-displayed-candidate)
          ;;                     (seq-length ram-eshell-history))
          ;;           (format "%s (%s)" candidate
          ;;                   (seq-length ram-eshell-history))))
          (insert (format "%s (%s of %s)" candidate
                          (1+ ram-eshell-displayed-candidate)
                          (seq-length ram-eshell-history)))
          (ram-eshell--completion-highligth-matches ovs search-substrings)
          (eshell-bol)
          (re-search-forward (car search-substrings) (point-at-eol) t 1))
      (when (ram-eshell-completion--continues-p)
        (insert (format "%s (%s)"
                        (string-join (reverse search-substrings) " ")
                        (propertize ram-eshell-message-no-matches 'face '((:foreground "red")))))
        (eshell-bol)
        (re-search-forward (car search-substrings) (point-at-eol) t)))))

(defun ram-eshell--handle-ins-spc ()
  (message ">>>> empty space")
  (let (candidates)
    ;; (message (format ">>>>>>>> search-substrings: %s" search-substrings))
    ;; (setq candidates (orderless-filter (string-join search-substrings " ") ram-eshell-history))
    (setq candidates ram-eshell-history)
    ;; next set of chars will be appended to this empty str

    (if (not ram-eshell-history)
        (progn
          (message (format "ram-eshell-completion-mode: %s" ram-eshell-message-no-matches))
         (setcar search-substrings (concat (car search-substrings) (this-command-keys))))
      ;; (ram-eshell--insert-candidate)
      ;; (forward-word)
      (ram-eshell--display-candidates)
      (message (format ">>> search substrings before: %s" search-substrings))
      (when (not (string= "" (car search-substrings)))
        (setq search-substrings (cons "" search-substrings)))
      (message (format ">>> search substrings after: %s" search-substrings))
      (message (format ">>> search string: %s" (string-join search-substrings " * ")))
      (message (format ">>> candidates num: %s" (seq-length candidates))))))

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
    ;; (ram-eshell--insert-candidate)
    (ram-eshell--display-candidates)))

;;* keymap

;;** keymap: bindings

(defvar ram-eshell-completion-mode-map nil
  "Keymap for `ram-eshell-completion-mode'")

(progn
  (setq ram-eshell-completion-mode-map (make-sparse-keymap))

  (define-key ram-eshell-completion-mode-map (kbd "M-p") #'ram-eshell-completion-prev)
  (define-key ram-eshell-completion-mode-map (kbd "M-n") #'ram-eshell-completion-next)
  (define-key ram-eshell-completion-mode-map (kbd "<backspace>") #'ram-eshell-completion-delete-backward-char)
  (define-key ram-eshell-completion-mode-map (kbd "<M-backspace>") #'ram-eshell-completion-backward-kill-word)
  (define-key ram-eshell-completion-mode-map (kbd "C-f") #'ram-eshell-completion-forward-char)
  (define-key ram-eshell-completion-mode-map (kbd "C-b") #'ram-eshell-completion-backward-char)
  (define-key ram-eshell-completion-mode-map (kbd "M-f") #'ram-eshell-completion-forward-word)
  (define-key ram-eshell-completion-mode-map (kbd "M-b") #'ram-eshell-completion-backward-word)
  (define-key ram-eshell-completion-mode-map (kbd "C-a") #'ram-eshell-completion-beginning-of-line)
  (define-key ram-eshell-completion-mode-map (kbd "C-e") #'ram-eshell-completion-end-of-line))

;;** keymap|bindings: functions

(defun ram-eshell-completion-next ()
  "Insert next history candidate."
  (interactive)
  (if (not (ram-eshell-completion--continues-p))
      (progn (setq this-command 'eshell-previous-matching-input-from-input)
             (eshell-previous-matching-input-from-input 1)
             (ram-eshell-completion-mode -1))
    (setq ram-eshell-displayed-candidate
          (% (1+ ram-eshell-displayed-candidate) (seq-length ram-eshell-history)))
    (ram-eshell--insert-candidate ram-eshell-displayed-candidate)))

(defun ram-eshell-completion-prev ()
  "Insert prev history candidate."
  (interactive)
  (if (not (ram-eshell-completion--continues-p))
      ;; (eshell-next-matching-input-from-input 1)
      (progn (setq this-command 'eshell-next-matching-input-from-input)
             (eshell-next-matching-input-from-input 1)
             (ram-eshell-completion-mode -1))
    (if (<= ram-eshell-displayed-candidate 0)
        (setq ram-eshell-displayed-candidate (1- (seq-length ram-eshell-history)))
      (setq ram-eshell-displayed-candidate (1- ram-eshell-displayed-candidate)))
    (ram-eshell--insert-candidate ram-eshell-displayed-candidate)))

;;;###autoload
(defun ram-eshell-completion-send-input ()
  "Clean input, enable `ram-eshell-completion-mode' and run `eshell-send-input'."
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

(defun ram-eshell-completion-end-of-line ()
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
