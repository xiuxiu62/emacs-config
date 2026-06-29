;;; reponotes-mode.el --- Scan project for dated developer notes and TODOs -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; reponotes-mode scans your C++ project for notes tagged with developer names
;; and optional dates, like:
;;
;;   // TODO(xiu)(16 may 2026): we should probably use something like this...
;;   // NOTE(xiu)(15 jan 2025): this is important...
;;   // DEBUG(xiu): something broken here... (no date)
;;   // WARN(xiu)(01 dec 2024): be careful...
;;
;; The results are displayed in a sortable, filterable list.
;;
;; Usage:
;;   M-x reponotes-show-all
;;   M-x reponotes-show-todos
;;
;;; Code:

(require 'consult)
(require 'projectile)
(require 'seq)
(require 'time-date)

(defgroup reponotes nil
  "Scan project for developer notes."
  :group 'tools
  :prefix "reponotes-")

(defcustom reponotes-tag-patterns
  '(("TODO"   . "TODO\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)")
    ("NOTE"   . "NOTE\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)")
    ("DEBUG"  . "DEBUG\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)")
    ("WARN"   . "WARN\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)")
    ("FIXME"  . "FIXME\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)")
    ("HACK"   . "HACK\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)")
    ("REVIEW" . "REVIEW\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\(?:\\s-*\\)?(?:\\([^)]+\\))?\\s-*:\\s-*\\(.*\\)"))
  "Alist of tag names and their regex patterns.
Pattern captures: group1 = developer, group2 = date (optional), group3 = message."
  :type '(alist :key-type string :value-type regexp)
  :group 'reponotes)

(defcustom reponotes-comment-styles
  '("//" "\\*" "///" "//!")
  "List of comment prefixes to look for."
  :type '(repeat string)
  :group 'reponotes)

(defcustom reponotes-ripgrep-args
  (concat "rg --line-buffered --color=never --max-columns=1000 "
          "--smart-case --no-heading --line-number -U")
  "Ripgrep arguments for searching."
  :type 'string
  :group 'reponotes)

(defcustom reponotes-date-format '("%d %b %Y" . "%d %B %Y")
  "Date formats to parse and display.
Car is the display format, cdr is the parsing format."
  :type '(cons string string)
  :group 'reponotes)

(defcustom reponotes-sort-by 'date-desc
  "Default sort order for notes.
Options: 'date-desc, 'date-asc, 'tag, 'developer, 'file"
  :type '(choice (const :tag "Newest first" date-desc)
                 (const :tag "Oldest first" date-asc)
                 (const :tag "By tag" tag)
                 (const :tag "By developer" developer)
                 (const :tag "By file" file))
  :group 'reponotes)

(defvar reponotes-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'reponotes-jump-to-note)
    (define-key map (kbd "g") 'reponotes-refresh)
    (define-key map (kbd "q") 'kill-current-buffer)
    (define-key map (kbd "f") 'reponotes-filter-by-developer)
    (define-key map (kbd "t") 'reponotes-filter-by-tag)
    (define-key map (kbd "d") 'reponotes-filter-by-date-range)
    (define-key map (kbd "c") 'reponotes-clear-filters)
    (define-key map (kbd "s") 'reponotes-sort-notes)
    (define-key map (kbd "o") 'reponotes-show-oldest)
    (define-key map (kbd "n") 'reponotes-show-newest)
    (define-key map (kbd "D") 'reponotes-show-notes-without-dates)
    map)
  "Keymap for reponotes result buffers.")

(defvar-local reponotes--current-filters nil
  "Current active filters (developer, tag, date-range).")

(defvar-local reponotes--raw-data nil
  "Raw note data before filtering and sorting.")

(defvar-local reponotes--current-sort 'date-desc
  "Current sort order.")

(defun reponotes--parse-date (date-str)
  "Parse DATE-STR into a time value.
Supports formats like '16 may 2026', '16 May 2026', '16 MAY 2026'."
  (when date-str
    (let ((normalized (downcase date-str))
          (month-map '(("jan" . 1) ("feb" . 2) ("mar" . 3) ("apr" . 4)
                       ("may" . 5) ("jun" . 6) ("jul" . 7) ("aug" . 8)
                       ("sep" . 9) ("oct" . 10) ("nov" . 11) ("dec" . 12))))
      (when (string-match "^\\([0-9]+\\)\\s-+\\([a-z]+\\)\\s-+\\([0-9]+\\)$" normalized)
        (let ((day (string-to-number (match-string 1 normalized)))
              (month (cdr (assoc (match-string 2 normalized) month-map)))
              (year (string-to-number (match-string 3 normalized))))
          (when month
            (encode-time 0 0 0 day month year)))))))

(defun reponotes--format-date (date-time)
  "Format DATE-TIME for display."
  (if date-time
      (format-time-string (car reponotes-date-format) date-time)
    "NO DATE"))

(defun reponotes--extract-note-info (line)
  "Extract note information from LINE (from ripgrep)."
  (when (string-match "^\\([^:]+\\):\\([^:]+\\):\\(.*\\)$" line)
    (let ((file-name (match-string 1 line))
          (line-num (string-to-number (match-string 2 line)))
          (content (match-string 3 line)))
      (catch 'found
        (dolist (pattern reponotes-tag-patterns)
          (let ((tag (car pattern))
                (regex (cdr pattern)))
            (when (string-match regex content)
              (let ((developer (match-string 1 content))
                    (date-str (match-string 2 content))
                    (message (match-string 3 content))
                    (date-time nil))
                (when date-str
                  (setq date-time (reponotes--parse-date date-str)))
                (throw 'found
                       (list :tag tag
                             :developer (or developer "unknown")
                             :date-str date-str
                             :date-time date-time
                             :message (string-trim message)
                             :file file-name
                             :line line-num
                             :full-location (format "%s:%d" file-name line-num)
                             :raw content))))))))))

(defun reponotes--run-ripgrep (regexp)
  "Run ripgrep with REGEXP in project root."
  (let* ((project-root (projectile-project-root))
         (default-directory project-root)
         (ripgrep-args (if (and (boundp 'consult--ripgrep-make-args)
                                (fboundp 'consult--ripgrep-make-args))
                           (consult--ripgrep-make-args nil)
                         ""))
         (ripgrep-cmd (format "%s %s -e \"%s\" --type cpp --type cc --type cxx --type h"
                              reponotes-ripgrep-args
                              ripgrep-args
                              regexp)))
    (when (not project-root)
      (user-error "Not in a project (use projectile-switch-project first if needed)"))
    (with-temp-buffer
      (let ((status (call-process-shell-command ripgrep-cmd nil t)))
        (if (eq status 0)
            (split-string (buffer-string) "\n" t)
          (when (not (eq status 1))
            (message "Ripgrep failed with status %d" status))
          nil)))))

(defun reponotes--scan-project ()
  "Scan project for all notes and return list of note plists."
  (let ((search-regex (reponotes--build-search-regexp))
        (notes '()))
    (when-let ((results (reponotes--run-ripgrep search-regex)))
      (dolist (line results)
        (when-let ((note (reponotes--extract-note-info line)))
          (push note notes))))
    (nreverse notes)))

(defun reponotes--build-search-regexp (&optional tag-patterns)
  "Build a comprehensive regexp for finding notes.
If TAG-PATTERNS is nil, use `reponotes-tag-patterns'."
  (let ((patterns (or tag-patterns reponotes-tag-patterns))
        (comment-prefix (regexp-opt reponotes-comment-styles)))
    (format "%s\\s-*%s"
            comment-prefix
            (regexp-opt (mapcar #'car patterns)))))

(defun reponotes--compare-notes (a b)
  "Compare notes A and B based on `reponotes--current-sort'."
  (pcase reponotes--current-sort
    ('date-desc
     (let ((date-a (plist-get a :date-time))
           (date-b (plist-get b :date-time)))
       (if (and date-a date-b)
           (time-less-p date-b date-a)
         (if date-a t nil))))
    ('date-asc
     (let ((date-a (plist-get a :date-time))
           (date-b (plist-get b :date-time)))
       (if (and date-a date-b)
           (time-less-p date-a date-b)
         (if date-a t nil))))
    ('tag
     (string-lessp (plist-get a :tag) (plist-get b :tag)))
    ('developer
     (string-lessp (plist-get a :developer) (plist-get b :developer)))
    ('file
     (string-lessp (plist-get a :file) (plist-get b :file)))
    (_ (time-less-p (plist-get b :date-time) (plist-get a :date-time)))))

(defun reponotes--format-note-line (note)
  "Format NOTE plist for display."
  (let ((tag (plist-get note :tag))
        (dev (plist-get note :developer))
        (date (reponotes--format-date (plist-get note :date-time)))
        (msg (plist-get note :message))
        (file (file-name-nondirectory (plist-get note :file)))
        (line (plist-get note :line)))
    (format "[%s] %s %s@%s:%d: %s"
            tag
            (propertize (format "%-10s" date) 'face 'font-lock-constant-face)
            dev
            file
            line
            (if (> (length msg) 70)
                (concat (substring msg 0 67) "...")
              msg))))

(defun reponotes--insert-note (note)
  "Insert NOTE into current buffer with text properties."
  (let ((start (point)))
    (insert (reponotes--format-note-line note) "\n")
    (put-text-property start (1- (point)) 'reponotes-note note)
    (put-text-property start (1- (point)) 'mouse-face 'highlight)
    (put-text-property start (1- (point)) 'help-echo "mouse-1 or RET: jump to note")
    (when (not (plist-get note :date-time))
      (put-text-property start (1- (point)) 'face 'font-lock-warning-face))))

(defun reponotes--display-results (notes &optional filter sort)
  "Display NOTES in a result buffer with optional FILTER and SORT."
  (let* ((filtered-notes (if filter
                            (seq-filter filter notes)
                          notes))
         (sorted-notes (sort (copy-sequence filtered-notes)
                             (if sort
                                 (lambda (a b) (funcall sort a b))
                               #'reponotes--compare-notes)))
         (buffer-name "*Reponotes*")
         (inhibit-read-only t))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (with-current-buffer (get-buffer-create buffer-name)
      (setq reponotes--raw-data notes
            reponotes--current-filters filter
            reponotes--current-sort (or sort reponotes-sort-by))
      (buffer-disable-undo)
      (erase-buffer)
      (reponotes-mode)
      (insert (propertize "═══════════════════════════════════════════════════════════════\n"
                          'face 'font-lock-keyword-face))
      (insert (propertize "REPONOTES - Project Developer Notes & TODOs\n"
                          'face 'font-lock-keyword-face))
      (insert (propertize "═══════════════════════════════════════════════════════════════\n"
                          'face 'font-lock-keyword-face))
      (insert (propertize (format "Project: %s\n"
                                  (projectile-project-name))
                          'face 'font-lock-comment-face))
      (insert (propertize (format "Notes: %d total, %d shown | Sort: %s"
                                  (length notes)
                                  (length sorted-notes)
                                  (symbol-name reponotes--current-sort))
                          'face 'font-lock-doc-face))
      (when filter
        (insert (propertize " | FILTER ACTIVE" 'face 'font-lock-warning-face)))
      (insert "\n═══════════════════════════════════════════════════════════════\n\n")
      (dolist (note sorted-notes)
        (reponotes--insert-note note))
      (goto-char (point-min))
      (forward-line 5)
      (display-buffer buffer-name
                      '((display-buffer-reuse-window
                         display-buffer-same-window)
                        (reusable-frames . t))))))

(defun reponotes-jump-to-note ()
  "Jump to the note at point."
  (interactive)
  (let ((note (get-text-property (point) 'reponotes-note)))
    (if note
        (let ((file (plist-get note :file))
              (line (plist-get note :line)))
          (find-file file)
          (goto-char (point-min))
          (forward-line (1- line))
          (recenter))
      (user-error "No note at point"))))

(defun reponotes-refresh ()
  "Refresh the notes list."
  (interactive)
  (let ((notes (reponotes--scan-project)))
    (reponotes--display-results notes reponotes--current-filters reponotes--current-sort)))

(defun reponotes-filter-by-developer (developer)
  "Filter notes by DEVELOPER name."
  (interactive (list (completing-read "Developer: "
                                      (seq-uniq
                                       (seq-map (lambda (n)
                                                  (plist-get n :developer))
                                                reponotes--raw-data)))))
  (reponotes--display-results
   reponotes--raw-data
   (lambda (note)
     (string-equal (plist-get note :developer) developer))
   reponotes--current-sort))

(defun reponotes-filter-by-tag (tag)
  "Filter notes by TAG (e.g., TODO, NOTE, etc.)."
  (interactive (list (completing-read "Tag: "
                                      (seq-uniq
                                       (seq-map (lambda (n)
                                                  (plist-get n :tag))
                                                reponotes--raw-data)))))
  (reponotes--display-results
   reponotes--raw-data
   (lambda (note)
     (string-equal (plist-get note :tag) tag))
   reponotes--current-sort))

(defun reponotes-filter-by-date-range (start-date end-date)
  "Filter notes between START-DATE and END-DATE.
Prompt for dates in format like '16 may 2026'."
  (interactive (list (read-string "Start date (e.g., 1 jan 2024): ")
                     (read-string "End date (e.g., 31 dec 2024): ")))
  (let ((start-time (reponotes--parse-date (downcase start-date)))
        (end-time (reponotes--parse-date (downcase end-date))))
    (if (and start-time end-time)
        (reponotes--display-results
         reponotes--raw-data
         (lambda (note)
           (let ((note-time (plist-get note :date-time)))
             (and note-time
                  (not (time-less-p note-time start-time))
                  (time-less-p note-time end-time))))
         reponotes--current-sort)
      (user-error "Invalid date format. Use like '16 may 2026'"))))

(defun reponotes-clear-filters ()
  "Clear all active filters."
  (interactive)
  (reponotes--display-results reponotes--raw-data nil reponotes--current-sort))

(defun reponotes-sort-notes (sort-type)
  "Sort notes by SORT-TYPE."
  (interactive (list (intern (completing-read "Sort by: "
                                              '("date-desc" "date-asc"
                                                "tag" "developer" "file")
                                              nil t))))
  (setq reponotes--current-sort sort-type)
  (reponotes--display-results reponotes--raw-data reponotes--current-filters sort-type))

(defun reponotes-show-oldest ()
  "Show oldest notes first."
  (interactive)
  (reponotes-sort-notes 'date-asc))

(defun reponotes-show-newest ()
  "Show newest notes first."
  (interactive)
  (reponotes-sort-notes 'date-desc))

(defun reponotes-show-notes-without-dates ()
  "Show only notes that don't have dates."
  (interactive)
  (reponotes--display-results
   reponotes--raw-data
   (lambda (note)
     (not (plist-get note :date-time)))
   reponotes--current-sort))

;; Convenience entry points
(defun reponotes-show-all ()
  "Show all notes in the project."
  (interactive)
  (let ((notes (reponotes--scan-project)))
    (reponotes--display-results notes nil reponotes-sort-by)))

(defun reponotes-show-for-developer (developer)
  "Show all notes for DEVELOPER."
  (interactive (list (read-string "Developer tag: ")))
  (let ((notes (reponotes--scan-project)))
    (reponotes--display-results
     notes
     (lambda (note)
       (string-equal (plist-get note :developer) developer))
     reponotes-sort-by)))

(defun reponotes-show-by-tag (tag)
  "Show all notes with specific TAG (TODO, NOTE, etc.)."
  (interactive (list (completing-read "Tag: "
                                      (mapcar #'car reponotes-tag-patterns))))
  (let ((notes (reponotes--scan-project)))
    (reponotes--display-results
     notes
     (lambda (note)
       (string-equal (plist-get note :tag) tag))
     reponotes-sort-by)))

(defun reponotes-show-todos ()
  "Show all TODO notes."
  (interactive)
  (reponotes-show-by-tag "TODO"))

(defun reponotes-show-notes ()
  "Show all NOTE notes."
  (interactive)
  (reponotes-show-by-tag "NOTE"))

(defun reponotes-show-warns ()
  "Show all WARN notes."
  (interactive)
  (reponotes-show-by-tag "WARN"))

(defun reponotes-show-debugs ()
  "Show all DEBUG notes."
  (interactive)
  (reponotes-show-by-tag "DEBUG"))

;; Dashboard summary function
(defun reponotes-dashboard ()
  "Show a summary dashboard of all notes grouped."
  (interactive)
  (let* ((notes (reponotes--scan-project))
         (tag-stats (make-hash-table :test 'equal))
         (dev-stats (make-hash-table :test 'equal))
         (dated-count 0)
         (undated-count 0))
    (dolist (note notes)
      (let ((tag (plist-get note :tag))
            (dev (plist-get note :developer)))
        (puthash tag (1+ (gethash tag tag-stats 0)) tag-stats)
        (puthash dev (1+ (gethash dev dev-stats 0)) dev-stats)
        (if (plist-get note :date-time)
            (setq dated-count (1+ dated-count))
          (setq undated-count (1+ undated-count)))))
    
    (let ((buffer-name "*Reponotes Dashboard*")
          (inhibit-read-only t))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (with-current-buffer (get-buffer-create buffer-name)
        (special-mode)
        (erase-buffer)
        (insert (propertize "═══════════════════════════════════════════════════════════\n"
                            'face 'font-lock-keyword-face))
        (insert (propertize "REPONOTES DASHBOARD\n"
                            'face 'font-lock-keyword-face))
        (insert (propertize "═══════════════════════════════════════════════════════════\n\n"
                            'face 'font-lock-keyword-face))
        
        (insert (propertize (format "Total notes: %d\n" (length notes))
                            'face 'font-lock-function-name-face))
        (insert (propertize (format "  With dates: %d\n" dated-count)
                            'face 'font-lock-type-face))
        (insert (propertize (format "  Without dates: %d\n\n" undated-count)
                            'face 'font-lock-warning-face))
        
        (insert (propertize "By Tag:\n" 'face 'font-lock-comment-face))
        (maphash (lambda (tag count)
                   (insert (format "  %s: %d\n" tag count)))
                 tag-stats)
        (insert "\n")
        
        (insert (propertize "By Developer:\n" 'face 'font-lock-comment-face))
        (maphash (lambda (dev count)
                   (insert (format "  %s: %d\n" dev count)))
                 dev-stats)
        
        (goto-char (point-min))
        (display-buffer buffer-name)))))

;; Define the major mode
(define-derived-mode reponotes-mode special-mode "Reponotes"
  "Major mode for displaying developer notes and TODOs."
  (setq buffer-read-only t)
  (setq truncate-lines t)
  (setq revert-buffer-function (lambda (_ignore-auto _noconfirm)
                                 (reponotes-refresh))))

(provide 'reponotes)

;;; reponotes-mode.el ends here
