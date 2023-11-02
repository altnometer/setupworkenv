;;; rec.el --- Complete eshell history. -*- lexical-binding: t; -*-

(provide 'ram-eshell-completion)

;;* faces

(defface rec--hl-match-face
  '((t :foreground "green3"
       ;; :background "grey"
       ;; :weight bold
       ;; :underline t
       ))
  "Face for highlighting matches in displayed candidates."
  :group 'ram-eshell-completion)

(defface rec--hl-match-selected-face
  '((t :foreground "green3"
       :background "light gray"
       ;; :weight bold
       ;; :underline t
       ))
  "Face for highlighting matches in selected candidates."
  :group 'ram-eshell-completion)

(defface rec--hl-selected-face
  '((t :foreground "black"
       :background "light grey"
       ;; :weight bold
       ;; :underline t
       ))
  "Face for highlighting selected candidates."
  :group 'ram-eshell-completion)

;;* variables

(defvar rec-min-char-to-start-completion 2
  "Restricts number of displayed completion candidates.")

(defvar rec-num-of-displayed-candidates 6
  "Restricts number of displayed completion candidates.")

(defvar rec--max-length-word 32
  "Words over this length are truncated.")

(defvar rec--trancate-symbol "…"
  "A substitute string for truncated parts.")

;;** variables: local

(defvar-local rec-completion-ov nil
  "An overlay to display matching candidates.")

(defvar-local rec-search-substrings nil
  "A list of substrings to match an eshell history.")

(defvar-local rec-history nil
  "A list of substrings to match an eshell history.")

(defvar-local rec-displayed-candidate nil
  "Indicate the number of the currently displayed candidate.")

(defvar-local rec--displaying-candidates-p nil
  "Indicate whether candidates are currently displayed.")

;;* functions: secondary

(defun rec--max-length-of-displayed-candidate ()
  "Calculate max length of displayed candidate string based on window width."
  (- (window-width) 5))

(defun rec-reset-candidates (candidates)
  "Set `rec-history' to CANDIDATES."
  (setq orderless-transient-matching-styles '(orderless-literal))
  (setq rec-history
        (orderless-filter
         (string-join rec-search-substrings " ") candidates))
  (setq orderless-transient-matching-styles nil)
  (setq rec-displayed-candidate 0))

(defun rec--set-vars ()
  "Set `ram-eshell-completion-mode' variables."
  (if rec-completion-ov
      (rec--hide-completion-ov)
    (setq rec-completion-ov (make-overlay 1 1)))
  (rec--reset-history))

(defun rec--delete-vars ()
  "Delete `ram-eshell-completion-mode' variables."
  (delete-overlay rec-completion-ov)
  (kill-local-variable 'rec-search-substrings)
  (kill-local-variable 'rec-history)
  (kill-local-variable 'rec-displayed-candidate)
  (kill-local-variable 'rec--displaying-candidates-p))

(defun rec--reset-history ()
  "Set `rec-history' to `eshell-history-ring'."
  ;; (message "((((((( reseting history")
  (setq rec-displayed-candidate 0)
  (setq rec-history
        (delete-dups
         (ring-elements eshell-history-ring))))

(defun rec--hide-completion-ov ()
  "Hide overlay displaying completion candidates."
  (setq rec--displaying-candidates-p nil)
  (overlay-put rec-completion-ov 'after-string nil)
  (move-overlay rec-completion-ov 1 1))

;;;###autoload
(defun rec-toggle-mode ()
  "Toggle `ram-eshell-completion-mode'."
  (interactive)
  (if ram-eshell-completion-mode
      (ram-eshell-completion-mode -1)
    (ram-eshell-completion-mode)
    (rec-start-completion)))

;;* functions: pre

(defun rec--pre ()
  (rec-uninstall-map))

;;* functions:  post

;; how to get a trace from commands in 'post-command-hook
;; credit to Johan Bockgård
;; https://lists.gnu.org/archive/html/emacs-devel/2010-07/msg01410.html
(defadvice rec--post (around intercept activate)
  (condition-case err
      ad-do-it
    ;; Let the debugger run
    ((debug error) (signal (car err) (cdr err)))))

(defun rec--post ()
  (if rec--displaying-candidates-p
      (rec-install-map))
  (cond
   ((memq this-command '(self-insert-command
                         kill-region
                         yank
                         delete-backward-char
                         delete-char
                         completion-at-point))
    (rec-start-completion))
   ((eq this-command 'eshell-interrupt-process)
    (rec--set-vars))
   ;; ((null this-command)
   ;;  (rec--set-vars))
   (t nil)))

;;* functions: make display candidates

(defun rec--highlight-string-matches (string rec-search-substrings)
  "Highlight REC-SEARCH-SUBSTRINGS matches in STRING."
  (if rec-search-substrings
      (let* ((r (car rec-search-substrings)))
        (when (not (string= r ""))
          (cl-labels ((hl (start)
                          (when (string-match r string start)
                            (add-text-properties
                             (match-beginning 0) (match-end 0)
                             '(face rec--hl-match-face) string)
                            (hl (match-end 0)))))
            (hl 0)))
        (rec--highlight-string-matches string (cdr rec-search-substrings)))
    string))

(defun rec--highlight-selected-candidate (string)
  "Highlight STRING and re-highlight existing highlighting."
  (let ((hl-old-face 'rec--hl-match-face)
        ;; change old highlighting to match backgrounds with "selected" face
        (rehl-old-face 'rec--hl-match-selected-face)
        (hl-face 'rec--hl-selected-face)
        old-props)
    (cl-labels
        ((get-old-props (search-from)
                        (let* ((prop-beg (text-property-any
                                          search-from
                                          (length string) 'face hl-old-face string))
                               (prop-end (and prop-beg
                                              (text-property-not-all
                                               prop-beg (length string) 'face hl-old-face string))))
                          (cond
                           ((not prop-beg) nil)
                           ((not prop-end) (list (cons prop-beg (length string))))
                           (t (cons (cons prop-beg prop-end) (get-old-props prop-end))))))
         (highlight (from exclude)
                    (let ((exclude-beg (car (car exclude)))
                          (exclude-end (cdr (car exclude))))
                      (if exclude
                          (progn
                            (add-text-properties from exclude-beg
                                                 `(face ,hl-face) string)
                            (add-text-properties exclude-beg exclude-end
                                                 `(face ,rehl-old-face) string)
                            (highlight exclude-end (cdr exclude)))
                        (add-text-properties from (length string)
                                             `(face ,hl-face) string)
                        string))))
      (highlight 0 (get-old-props 0)))))

(defun rec--crop-word (word substr)
  "Crop WORD."
  (let (;; segments around substr
        beginning-part   ; any [:alnum:] but with no substr
        middle-part      ; includes substr and [:alnum:] segment after
        new-word)
    ;; set beginning-part if regex for it matches
    (when (and (string-match (rx line-start
                                 (zero-or-more (not alnum))
                                 (one-or-more (or alnum "-" "/"))) word)
               ;; do not include if substr in the match
               (not (s-matches-p substr (match-string 0 word))))
      (setq beginning-part (match-string 0 word)))
    ;; set middle-part to regex match group 1
    (string-match (rx-to-string
                   `(: (group (zero-or-more (or alnum "-" "/"))
                              (zero-or-more (not alnum))
                              ,substr
                              (zero-or-more (not alnum))
                              (zero-or-more (or alnum "-" "/")))
                       (group (zero-or-more anychar))
                       line-end) t)
                  word)
    (setq middle-part (match-string 1 word))
    ;; set new-word by joining parts
    (setq new-word (string-join (remove nil (vector beginning-part middle-part))
                                rec--trancate-symbol))
    ;; add rec--trancate-symbol if needed
    (when (and new-word
               (not (string= "" (match-string 2 word))))
      (setq new-word (concat new-word rec--trancate-symbol)))
    ;; restore 'face property for SUBSTR
    (string-match substr new-word)
    (add-text-properties
     (match-beginning 0) (match-end 0)
     '(face rec--hl-match-face) new-word)
    new-word))

(defun rec--resize-list (words length max-length resized-p)
  "Remove words that do not contain `rec--hl-match-face'.
  Keep removing until LENGTH is less than
  `rec--max-length-of-displayed-candidate'."
  (if (or (null words)
          (<= length max-length))
      words
    (let* ((word (car words))
           (match-begining
            (text-property-any 0 (length word) 'face 'rec--hl-match-face word))
           match-end)
      (if match-begining
          (if (>= (length word) rec--max-length-word)
              (progn
                (setq match-end
                      (text-property-not-all match-begining (length word) 'face 'rec--hl-match-face word))
                (let* ((match-substr (substring word match-begining match-end))
                       (cropped-word (rec--crop-word word match-substr)))
                  (cons
                   cropped-word
                   (rec--resize-list (cdr words) (- length
                                                    (- (length word) (length cropped-word)))
                                     max-length nil))))
            (cons word (rec--resize-list (cdr words) length max-length nil)))
        ;; when previous word was discarded (words resized), 'rec--trancate-symbol
        ;; was already inserted, do not insert it again.
        (if resized-p
            (rec--resize-list (cdr words) (- length (length word)) max-length resized-p)
          (let ((replacement rec--trancate-symbol))
            (cons replacement
                  (rec--resize-list
                   (cdr words)
                   (- length (- (length word) (length replacement)))
                   max-length
                   t))))))))

(defun rec--resize-str (str)
  "Trim string to `rec--max-length-of-displayed-candidate'."
  (let* ((words (reverse (split-string str)))
         (num-spaces (1- (length words)))
         (str-len-no-spaces (- (length str) num-spaces))
         (max-len-str-no-spaces (- (rec--max-length-of-displayed-candidate) num-spaces))
         resized-str)
    (setq resized-str (string-join (reverse (rec--resize-list
                                             words str-len-no-spaces
                                             max-len-str-no-spaces nil)) " "))
    resized-str))

(defun rec--make-display-candidates-string (candidates)
  "Make a string of candidates that is displayed to the user."
  (let ((candidates (if (> (length candidates) rec-num-of-displayed-candidates)
                        (butlast candidates (- (length candidates) rec-num-of-displayed-candidates))
                      candidates)))
    (concat
     (format " (%s of %s)\n" (1+ rec-displayed-candidate)
             (length rec-history))
     (let ((counter 0))
       (mapconcat (lambda (str)
                    (let ((str (rec--highlight-string-matches
                                (concat (string-trim str))
                                rec-search-substrings))
                          new-str)
                      (if (> (length str) (rec--max-length-of-displayed-candidate))
                          (setq new-str (rec--resize-str str))
                        (setq new-str str))
                      (when (= counter rec-displayed-candidate)
                        (setq new-str (rec--highlight-selected-candidate new-str)))
                      (setq counter (1+ counter))
                      new-str))
                  candidates "\n")))))

(defun rec--display-candidates ()
  "Display completion candidates. "
  (setq rec--displaying-candidates-p t)
  (let ((str (rec--make-display-candidates-string rec-history)))
    ;; (overlay-put rec-completion-ov 'after-string nil)
    (move-overlay rec-completion-ov (point-at-eol) (point-at-eol))
    (overlay-put rec-completion-ov 'after-string str)
    (put-text-property 0 1 'cursor 0 str)))

(defun rec--subset-of-substrings-p (l1 l2)
  "True if L1 is a subset of corresponding substrings in L2."
  (cl-labels ((substringp
               (str1 str2)
               (if (< (length str1) (length str2))
                   (if (string= str1 (substring str2 0 (length str1)))
                       t
                     (substringp str1 (substring str2 1 (length str2))))
                 (and str1 str2 (string= str1 str2)))))
    (cond
     ((and l1 (not l2)) nil)
     ((not l1) t)
     ((substringp (car l1) (car l2))
      (rec--subset-of-substrings-p (cdr l1) (cdr l2)))
     (t nil))))

(defun rec-start-completion ()
  (let ((old-rec-search-substrings rec-search-substrings)
        (new-rec-search-substrings
         (mapcar #'regexp-quote
                 (split-string (buffer-substring-no-properties
                                (save-excursion (beginning-of-line) (point))
                                (point-at-eol))))))
    (setq rec-search-substrings new-rec-search-substrings)
    (cond
     ;; empty `rec-search-substrings', reset history, hide completion
     ((or (null rec-search-substrings)
          (not (member t (mapcar (lambda (s) (not (string= "" s))) rec-search-substrings))))
      ;; (message "(((((((( no rec-search-substrings: %s" rec-search-substrings)
      (rec--hide-completion-ov)
      (rec--reset-history))
     ;; all `rec-search-substrings' strings are shorter than `rec-min-char-to-start-completion'
     ;; narrow search candidates, hide completion
     ((not (member t
                   (mapcar (lambda (s)
                             (> (length s) (1- rec-min-char-to-start-completion)))
                           rec-search-substrings)))
      ;; (message "(((((( no rec-search-substrings long enough: %s" rec-search-substrings)
      (rec--hide-completion-ov)
      (rec-reset-candidates rec-history))
     ;; old search substrings are the subset of new ones
     ;; reuse previous candidates to narrow them further
     ((rec--subset-of-substrings-p
       old-rec-search-substrings
       new-rec-search-substrings)
      ;; (message "(((((( rec-search-substrings %s is a subset of %s" old-rec-search-substrings new-rec-search-substrings)
      (rec-reset-candidates rec-history)
      (if rec-history
          (progn (rec--display-candidates)
                 (rec-install-map))
        (rec--hide-completion-ov)))
     (t
      ;; (message "((((((( true case")
      (rec-reset-candidates (delete-dups
                             (ring-elements eshell-history-ring)))
      (if (and rec-search-substrings rec-history)
          (progn (rec--display-candidates)
                 (rec-install-map))
        (rec--hide-completion-ov))))))

;;* keymap|bindings

(defvar ram-eshell-completion-mode-map nil
  "Keymap for `ram-eshell-completion-mode'")

(setq ram-eshell-completion-mode-map (make-sparse-keymap))

;;** keymap|bindings: emulation-mode-map-alists

;; adapted from `company-mode' keymap handling
(defvar rec-active-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\M-p" #'rec-prev)
    (define-key keymap "\M-n" #'rec-next)
    (define-key keymap (kbd "C-p") #'rec-prev)
    (define-key keymap (kbd "C-n") #'rec-next)
    (define-key keymap (kbd "<return>") #'rec-insert-candidate-as-input)
    (define-key keymap (kbd "<C-return>") #'rec-send-input)
    (define-key keymap (kbd "<tab>") #'rec-completion-at-point)
    keymap)
  "Keymap enabled when displaying completion candidates.")

(defvar-local rec-emulation-alist '((t . nil))
  "Keymap active only when displaying completion candidates.")

(defun rec-ensure-emulation-alist ()
  (unless (eq 'rec-emulation-alist (car emulation-mode-map-alists))
    (setq emulation-mode-map-alists
          (cons 'rec-emulation-alist
                (delq 'rec-emulation-alist emulation-mode-map-alists)))))

(defun rec-install-map ()
  (when (not (cdar rec-emulation-alist))
    (setf (cdar rec-emulation-alist) rec-active-keymap)))

(defun rec-uninstall-map ()
  (setf (cdar rec-emulation-alist) nil))

;;** keymap|bindings: commands

(defun rec-next ()
  "Move to next history candidate.
Increment `rec-displayed-candidate'."
  (interactive)
  (setq rec-displayed-candidate
        (% (1+ rec-displayed-candidate)
           (min
            rec-num-of-displayed-candidates
            (length rec-history))))
  (rec--display-candidates))

(defun rec-prev ()
  "Move to previous history candidate.
Decrement `rec-displayed-candidate'."
  (interactive)
  (if (<= rec-displayed-candidate 0)
      (setq rec-displayed-candidate (1- (min
                                         rec-num-of-displayed-candidates
                                         (length rec-history))))
    (setq rec-displayed-candidate (1- rec-displayed-candidate)))
  (rec--display-candidates))

(defun rec-send-input ()
  "Run `eshell-send-input'  with `rec-displayed-candidate' candidate."
  (interactive)
  (let ((input-is-empty-p
         (string=
          "" (string-trim (buffer-substring-no-properties
                           (save-excursion (beginning-of-line) (point)) (point-at-eol))))))
    (unless input-is-empty-p
      (rec--hide-completion-ov)
      (delete-region (save-excursion (beginning-of-line) (point)) (point-at-eol))
      (insert (string-trim (nth rec-displayed-candidate
                                rec-history)))
      (rec--reset-history)))
  (eshell-send-input))

(defun rec-insert-candidate-as-input ()
  "Insert `rec-displayed-candidate' candidate as input.
Disable `ram-eshell-completion-mode'."
  (interactive)
  (delete-region (save-excursion (beginning-of-line) (point)) (point-at-eol))
  (rec--hide-completion-ov)
  (insert (string-trim (nth rec-displayed-candidate
                            rec-history)))
  (when ram-eshell-completion-mode
    (ram-eshell-completion-mode -1)))

(defun rec-completion-at-point ()
  "Run `completion-at-point'  safely.
Disable `ram-eshell-completion-mode' before and enable it after."
  (interactive)
  (rec--hide-completion-ov)
  (completion-at-point))

;;* minor-mode definition

;;;###autoload
(define-minor-mode ram-eshell-completion-mode
  "Minor mode for eshell history completion."
  nil " ram-eshell-completion-mode" ram-eshell-completion-mode-map
  (if ram-eshell-completion-mode
      (progn
        (rec--set-vars)
        (rec-ensure-emulation-alist)
        (add-hook 'pre-command-hook 'rec--pre nil t)
        (add-hook 'post-command-hook 'rec--post nil t))
    (rec--delete-vars)
    (remove-hook 'pre-command-hook 'rec--pre t)
    (remove-hook 'post-command-hook 'rec--post t)))
