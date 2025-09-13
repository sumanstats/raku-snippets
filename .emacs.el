;; Disable unnecessary GUI elements
(tool-bar-mode -1)
(tab-bar-mode 1)


;; -------------------------------
;; Package setup
;; -------------------------------
(require 'package)
(setq package-archives nil)
;; Add only MELPA stable
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; -------------------------------
;; Basic settings
;; -------------------------------
(setq default-directory "C:/Users/suman")
(global-display-line-numbers-mode t)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(set-frame-font "Consolas-18" nil t)
(load-theme 'solarized-light t)
(setq inferior-R-program-name "D:/R-4.5.1/bin/R.exe")
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; -------------------------------
;; Utility functions
;; -------------------------------
(defun title-case (start end)
  "Convert region to title case, capitalizing only the first letter of each word."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\b\\w" nil t)
        (replace-match (upcase (match-string 0)) t t)))))

(defun find-file-in-new-tab-side-by-side (filename &optional wildcards)
  "Open FILENAME in a new tab, side by side with current tab."
  (interactive
   (find-file-read-args "Find file in new tab side by side: " t))
  (tab-bar-new-tab)  ;; opens to the right of current tab
  (find-file filename wildcards))

(global-set-key (kbd "C-x C-f") 'find-file-in-new-tab-side-by-side)


;; -------------------------------
;; UTF-8 and Markdown
;; -------------------------------
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
(setq markdown-command "pandoc")

;; -------------------------------
;; Company + Yasnippet setup
;; -------------------------------
(require 'company)
(require 'yasnippet)
(require 'company-yasnippet)

;; Enable modes
(yas-global-mode 1)
(global-company-mode 1)

;; Basic Company settings
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-selection-wrap-around t)

;; *** CRITICAL MODIFICATION: Disable company during snippet expansion ***
(defun yas-in-snippet-field-p ()
  "Check if cursor is currently in a yasnippet field."
  (and (boundp 'yas--active-field-overlay)
       yas--active-field-overlay
       (overlay-buffer yas--active-field-overlay)
       (>= (point) (overlay-start yas--active-field-overlay))
       (<= (point) (overlay-end yas--active-field-overlay))))

(defun check-snippet-field-and-toggle-company ()
  "Check if we're in a snippet field and toggle company accordingly."
  (if (yas-in-snippet-field-p)
      (progn
        (setq-local company-idle-delay nil)
        (when company-pseudo-tooltip-overlay
          (company-cancel)))  ; Cancel any active company popup
    (setq-local company-idle-delay 0.1)))

(add-hook 'post-command-hook 'check-snippet-field-and-toggle-company)

;; Additional hooks for more reliable state management
(add-hook 'yas-before-expand-snippet-hook
          (lambda () 
            ;; Don't disable immediately, let the field detection handle it
            nil))

(add-hook 'yas-after-exit-snippet-hook
          (lambda () 
            (setq-local company-idle-delay 0.1)
            ;; Force company to be available again
            (run-with-idle-timer 0.1 nil 'company-idle-begin)))


;; CRITICAL: Configure backends to show both dabbrev AND yasnippet
(setq company-backends
      '((company-dabbrev           ; Previously typed words (all buffers)
         company-yasnippet         ; Snippet keys
         company-capf              ; Language server completions
         company-files)))          ; File names

;; Configure dabbrev to collect words properly
(setq company-dabbrev-ignore-case nil)      ; Case-sensitive matching
(setq company-dabbrev-downcase nil)         ; Don't change case
(setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")  ; Word characters
(setq dabbrev-case-fold-search nil)         ; Case-sensitive search

;; Popup styling (keep your existing settings)
(set-face-attribute 'company-tooltip nil
  :background "#2e3440" :foreground "#d8dee9" :height 1.1
  :box '(:line-width 1 :color "#4c566a"))
(set-face-attribute 'company-tooltip-selection nil
  :background "#5e81ac" :foreground "#eceff4" :weight 'bold)
(set-face-attribute 'company-tooltip-common nil
  :foreground "#88c0d0" :weight 'bold)
(set-face-attribute 'company-tooltip-annotation nil
  :foreground "#81a1c1" :slant 'italic)

;; Keybindings
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "RET") 'company-complete-selection)

;; Cutomising tab
;; Active tab
(set-face-attribute 'tab-bar-tab nil
                    :background "#4c7ed9"  ;; bright blue
                    :foreground "#eceff4"  ;; white text
                    :weight 'bold
                    :box '(:line-width 2 :color "#81a1c1"))

(set-face-attribute 'tab-bar nil
                    :background "#2e3440"  ;; very dark gray
                    :foreground "#d8dee9")
;; ---------------------------------------------------
;; Load markdown snippets
;; load markdown-snippets.el as Lisp, not as a snippet file
;; ---------------------------------------------------
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'markdown-snippets)



;; -------------------------------
;; Frequent Words Completion
;; -------------------------------
(defvar my-frequent-words
  '("Braun's JJ" "heterogeneously" "enhancing" "irregular" "SUVmax"
    "thickness" "blood loss" "duration" "high colored urine" "clay colored stool" "optimization" "diagnostic workup" "gall bladder" "pancreas" "lymph nodes" "weight loss")
  "List of frequently used words for autocompletion.")

(defun company-my-frequent-words (command &optional arg &rest ignored)
  "Company backend for frequent words.
Triggers after 2 characters are typed."
  (interactive (list 'interactive))
  (cl-case command
    (prefix (and (>= (length (company-grab-word)) 2)
                 (company-grab-word)))
    (candidates
     (cl-remove-if-not
      (lambda (word) (string-prefix-p arg word))
      my-frequent-words))
    (ignore-case t)
    (annotation (lambda (word) " [freq]"))))

;; Custom face for frequent words
(defface company-frequent-words
  '((t :foreground "#a3be8c" :weight bold))
  "Face for frequent words in Company popup.")

;; Apply the face to frequent words
(defun company-my-frequent-words-annotate (candidate)
  "Add annotation and face to frequent words."
  (propertize " [freq]" 'face 'company-frequent-words))

(advice-add 'company-my-frequent-words :around
            (lambda (orig-fun &rest args)
              (let ((result (apply orig-fun args)))
                (if (eq (car args) 'annotation)
                    (company-my-frequent-words-annotate result)
                  result))))

;; Add current word to frequent words list
(defun add-current-word-to-frequent-words ()
  "Add word at point to frequent words list."
  (interactive)
  (let ((word (thing-at-point 'word)))
    (when word
      (setq word (downcase word))
      (unless (member word my-frequent-words)
        (push word my-frequent-words)
        (message "Added '%s' to frequent words" word)))))

;; Remove word from frequent words list
(defun remove-word-from-frequent-words (word)
  "Remove WORD from frequent words list."
  (interactive
   (list (completing-read "Remove word: " my-frequent-words)))
  (setq my-frequent-words (delete word my-frequent-words))
  (message "Removed '%s' from frequent words" word))

;; Update company backends
(setq company-backends
      '((company-my-frequent-words
         company-dabbrev
         company-yasnippet
         company-capf
         company-files)))

;; Configure trigger behavior
(setq company-transformers
      '(company-sort-by-occurrence
        company-sort-prefer-same-case-prefix))

;; Keybindings
(global-set-key (kbd "C-c +") 'add-current-word-to-frequent-words)
(global-set-key (kbd "C-c -") 'remove-word-from-frequent-words)


;; ====================================
