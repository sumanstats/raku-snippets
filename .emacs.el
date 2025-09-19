;; Disable unnecessary GUI elements
(tool-bar-mode -1)
(tab-bar-mode 1)
(global-visual-line-mode 1)



;; -------------------------------
;; Package setup
;; -------------------------------
(require 'package)
;; Add only MELPA stable
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; -------------------------------
;; Basic settings
;; -------------------------------
(setq default-directory "C:/Users/suman")
(global-display-line-numbers-mode t)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(set-frame-font "Consolas-18" nil t)
(load-theme 'monokai t)
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


(require 'seq)
(require 'cl-lib)

(defvar my/tab-buffer->tabname (make-hash-table :test 'equal)
  "Hash mapping file truename -> tab name where that file was last registered.")

(defun my/register-buffer-in-current-tab (&optional buffer)
  "Record BUFFER's file (if any) as being associated with the current tab.
If BUFFER is nil, use `current-buffer'."
  (let ((buf (or buffer (current-buffer)))
        (tab (tab-bar--current-tab)))
    (when (and (buffer-file-name buf) tab)
      (puthash (file-truename (buffer-file-name buf))
               (alist-get 'name tab)
               my/tab-buffer->tabname))))

;; Keep mapping up-to-date when files are opened / saved
(add-hook 'find-file-hook #'my/register-buffer-in-current-tab)
(add-hook 'after-save-hook #'my/register-buffer-in-current-tab)

(defun my/remove-mappings-for-tabname (tabname)
  "Remove all hash entries that point to TABNAME."
  (maphash (lambda (k v) (when (string= v tabname) (remhash k my/tab-buffer->tabname)))
           my/tab-buffer->tabname))

;; When a tab is closed, remove mappings that pointed to that tab.
(advice-add 'tab-bar-close-tab :around
            (lambda (orig &rest args)
              (let ((closed-name (alist-get 'name (tab-bar--current-tab))))
                (apply orig args)
                (my/remove-mappings-for-tabname closed-name))))

;; If a tab is renamed, update our mappings.
(advice-add 'tab-bar-rename-tab :around
            (lambda (orig &rest args)
              (let ((old-name (alist-get 'name (tab-bar--current-tab))))
                (apply orig args)
                (let ((new-name (alist-get 'name (tab-bar--current-tab))))
                  (maphash (lambda (k v)
                             (when (string= v old-name)
                               (puthash k new-name my/tab-buffer->tabname)))
                           my/tab-buffer->tabname)))))

;; Remove mapping when buffer is killed
(add-hook 'kill-buffer-hook
          (lambda ()
            (when buffer-file-name
              (remhash (file-truename buffer-file-name) my/tab-buffer->tabname))))

;;;###autoload
(defun my-find-file-in-new-tab (filename &optional wildcards)
  "Open FILENAME in a new tab to the right, or jump to the tab recorded for it.

Fast path: if we have a recorded tab name for FILENAME, jump to that tab (no flicker).
Fallback: scan tabs (temporarily selecting them) to find a tab that shows the buffer.
Interactive uses `nil` for the require-match argument so new files can be created."
  (interactive (find-file-read-args "Find file in new tab: " nil))
  (cl-block nil
    (let* ((truename (and filename (file-truename filename)))
           (tabname  (and truename (gethash truename my/tab-buffer->tabname)))
           (existing-buffer (and truename (find-buffer-visiting truename)))
           (tabs (tab-bar-tabs)))
      ;; Fast-path: hash hit -> find tab index by name (cheap)
      (when tabname
        (let ((idx (seq-position tabs tabname
                                 (lambda (tab name)
                                   (string= (alist-get 'name tab) name)))))
          (when idx
            (tab-bar-select-tab (1+ idx))
            (if existing-buffer
                (switch-to-buffer existing-buffer)
              (find-file filename wildcards))
            (my/register-buffer-in-current-tab (current-buffer))
            (cl-return t))))
      ;; Fallback: scan tabs by temporarily selecting each (only if buffer exists)
      (let* ((orig-index (alist-get 'index (tab-bar--current-tab)))
             (n (length tabs))
             (found nil))
        (unwind-protect
            (progn
              (when existing-buffer
                (dotimes (i n)
                  (let ((idx (1+ i)))
                    (tab-bar-select-tab idx)
                    (when (get-buffer-window existing-buffer (selected-frame))
                      (setq found idx)
                      (cl-return)))))
              (if found
                  (progn
                    (tab-bar-select-tab found)
                    (switch-to-buffer existing-buffer)
                    (my/register-buffer-in-current-tab existing-buffer)
                    (cl-return t))
                ;; not found anywhere -> open new tab and visit
                (tab-bar-select-tab orig-index)
                (tab-bar-new-tab)
                (delete-other-windows)
                (find-file filename wildcards)
                (my/register-buffer-in-current-tab (current-buffer))
                (cl-return t)))
          ;; Restore original tab if needed on exit / error
          (when (and orig-index
                     (not (eq (alist-get 'index (tab-bar--current-tab)) orig-index)))
            (tab-bar-select-tab orig-index)))))))

;; Rebind if you like:
(global-set-key (kbd "C-x C-f") #'my-find-file-in-new-tab)








;; -------------------------------
;; UTF-8 and Markdown
;; -------------------------------
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)




;; -------------------------------
;; Company + Yasnippet + Frequent Words Setup (corrected)
;; -------------------------------
;; -------------------------------
;; Company + Yasnippet + Frequent Words Setup
;; -------------------------------

(require 'company)
(require 'yasnippet)
(require 'cl-lib)

;; -------------------------------
;; Enable modes
;; -------------------------------
(yas-global-mode 1)
(global-company-mode 1)
(yas-reload-all)

;; -------------------------------
;; Suspend company popups during snippet fields (robust + efficient)
;; -------------------------------
(defun my/company-suspend-in-yas-field ()
  "Temporarily suspend company popup when in active yasnippet field."
  (setq-local company-idle-delay
              (if (and (boundp 'yas--active-field-overlay)
                       yas--active-field-overlay
                       (overlay-buffer yas--active-field-overlay))
                  nil
                0.1)))

(add-hook 'post-command-hook #'my/company-suspend-in-yas-field)

;; -------------------------------
;; Company backends
;; -------------------------------
(defvar my-frequent-words
  '("Braun's JJ" "heterogeneously" "enhancing" "irregular" "SUVmax"
    "thickness" "blood loss" "duration" "high colored urine" "clay colored stool"
    "optimization" "diagnostic workup" "gall bladder" "pancreas" "lymph nodes" "weight loss")
  "List of frequently used words for autocompletion.")

(defun company-my-frequent-words (command &optional arg &rest ignored)
  "Company backend for frequent words. Trigger after 2 characters."
  (cl-case command
    (prefix (and (>= (length (company-grab-word)) 2)
                 (company-grab-word)))
    (candidates
     (cl-remove-if-not (lambda (w) (string-prefix-p arg w)) my-frequent-words))
    (annotation (lambda (w) " [freq]"))
    (ignore-case t)))

;; Annotate frequent words in popup
(defface company-frequent-words
  '((t :foreground "#a3be8c" :weight bold))
  "Face for frequent words in Company popup.")

(defun company-my-frequent-words-annotate (candidate)
  "Add annotation and face to frequent words."
  (propertize " [freq]" 'face 'company-frequent-words))

(advice-add 'company-my-frequent-words :around
            (lambda (orig-fun &rest args)
              (let ((res (apply orig-fun args)))
                (if (eq (car args) 'annotation)
                    (company-my-frequent-words-annotate res)
                  res))))

;; Add/remove frequent words
(defun add-current-word-to-frequent-words ()
  "Add word at point to frequent words list."
  (interactive)
  (let ((word (thing-at-point 'word t)))
    (when (and word (not (member word my-frequent-words)))
      (push word my-frequent-words)
      (message "Added '%s' to frequent words" word))))

(defun remove-word-from-frequent-words (word)
  "Remove WORD from frequent words list."
  (interactive
   (list (completing-read "Remove word: " my-frequent-words)))
  (setq my-frequent-words (delete word my-frequent-words))
  (message "Removed '%s' from frequent words" word))

(global-set-key (kbd "C-c +") 'add-current-word-to-frequent-words)
(global-set-key (kbd "C-c -") 'remove-word-from-frequent-words)

;; Configure company backends
(setq company-backends
      '((company-my-frequent-words
         company-dabbrev
         company-yasnippet
         company-capf
         company-files)))

;; Sort completions
(setq company-transformers
      '(company-sort-by-occurrence
        company-sort-prefer-same-case-prefix))

;; Keybindings for company
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "RET") 'company-complete-selection)

;; -------------------------------
;; Markdown snippets
;; -------------------------------

(add-to-list 'load-path "C:/Users/suman/") ; directory containing markdown-snippets.el
(require 'markdown-snippets)


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



;; ====================================



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cobalt company-statistics ess markdown-mode monokai-theme
	    solarized-theme yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


(defun toggle-window-split ()
  "Toggle between horizontal and vertical split for two windows."
  (interactive)
  (when (= (count-windows) 2)
    (let* ((w1 (selected-window))
           (w2 (next-window))
           (b1 (window-buffer w1))
           (b2 (window-buffer w2))
           (edges1 (window-edges w1))
           (edges2 (window-edges w2))
           (split-vertically-p (< (cadr edges1) (cadr edges2))))
      (delete-other-windows)
      (if split-vertically-p
          (split-window-right)
        (split-window-below))
      (set-window-buffer (selected-window) b1)
      (set-window-buffer (next-window) b2))))

(global-set-key (kbd "C-x |") 'toggle-window-split)

(require 'anzu)
(global-anzu-mode +1)
