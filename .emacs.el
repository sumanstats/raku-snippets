;; Disable unnecessary GUI elements
(tool-bar-mode -1)
(tab-bar-mode 1)
(global-visual-line-mode 1)
;; 1)
(setq inhibit-startup-screen t)

;; 2)
(delete-selection-mode t)

;; 3)
(show-paren-mode 1)
(electric-pair-mode 1)

;; 5)
(setq use-short-answers t)


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
(setq default-directory "C:/Users/suman/Desktop/Audit/")
(global-display-line-numbers-mode t)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(set-frame-font "Fira Code-18" nil t)
(load-theme 'material t)
(setq inferior-R-program-name "D:/R-4.5.2/bin/R.exe")
(setq make-backup-files nil)
(setq create-lockfiles nil)

;; -------------------------------
;; Keep auto-save, but send files to the system temp directory (recommended, so recovery still works, but don't clutter
;;--------------------------------
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))


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
(set-keyboard-coding-system 'utf-8)

;; Use pandoc for markdown conversion
(setq markdown-command "pandoc -f gfm -t html5 --standalone")


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


(setq company-minimum-prefix-length 2
      company-idle-delay 0.05
      company-selection-wrap-around t)

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
    "thickness" "blood loss" "duration" "high colored urine" "clay colored stool" "chloramphenicol" "tigecycline" "doxycycline" "teicoplanin" "vancomycin" "colistin" "levofloxacin" "meropenem" "gentamicin" "amikacin" "cholelithiasis" "choledocholithiasis" "optimization" "diagnostic workup" "gall bladder" "pancreas" "lymph nodes" "weight loss" "supportive care" "pigtail" "tolerating" "hepatocellular carcinoma" "resection" "idiopathic thrombocytopenic purpura" "mycophenolate mofetil" "hemihepatectomy" "extubated and shifted to" "Pfannenstiel" "adenocarcinoma" "generalised" "pruritus" "fluctuating" "nonreactive" "comorbidities" "asymmetrical" "circumferential" "thickening" "Frey's procedure" "serology" "triglyceride"
    "discharged" "Acinetobacter" "enhancement" "progressive" "periampullary" "differentiated")
  "List of frequently used words for autocompletion.")

(defun company-my-frequent-words (command &optional arg &rest ignored)
  "Company backend for frequent words. Trigger after 3 characters."
  (cl-case command
    (prefix (and (>= (length (company-grab-word)) 3)
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
      '((company-dabbrev
         company-my-frequent-words
         company-capf
         company-files
         company-yasnippet)))

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

(load "markdown-snippets" t)
(load "new-snippets" t)
(load "meld" t)


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
   '(anzu auctex color-theme-sanityinc-tomorrow ess ess-R-data-view
	  ess-view highlight-indent-guides htmlize jupyter magit
	  markdown-mode material-theme monokai-theme move-text
	  multiple-cursors poly-R poly-org quarto-mode smartparens
	  solarized-theme yaml yaml-mode yasnippet)))
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


;; -------------------------------
;; Org-mode Babel Language Support
;; -------------------------------

;; This line tells Org-babel which programming languages to enable.
;; The 't' means "enable" for that language.

(setq org-babel-python-command "D:/miniconda3/python.exe")
(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (python . t)    ; Enable Python execution
   (R . t)     
   (emacs-lisp . t)
   ))
(setq org-html-inline-images t)
(add-hook 'org-mode-hook 'org-display-inline-images)
(setq org-confirm-babel-evaluate nil)
(setq org-export-babel-evaluate t)

(require 'htmlize)
(setq isearch-wrap-pause nil)  
(setq isearch-allow-scroll t) 

(require 'anzu)
(global-anzu-mode +1)

(move-text-default-bindings)

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


;; Get file name and one folder above with keyboard shortcut 
;; C-c f
(defun copy-parent-folder-and-file ()
  "Copy 'parent-folder/filename' to clipboard."
  (interactive)
  (when buffer-file-name
    (let* ((dir (file-name-directory buffer-file-name))
           (parent (file-name-nondirectory (directory-file-name dir)))
           (file (file-name-nondirectory buffer-file-name))
           (result (concat parent "/" file)))
      (kill-new result)
      (message result))))

(global-set-key (kbd "C-c f") 'copy-parent-folder-and-file)

;; Refresh emacs file with init

(defun reload-init-file ()
  "Reload Emacs init file."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(global-set-key (kbd "C-c r") 'reload-init-file)



(add-hook 'markdown-mode-hook
          (lambda ()
            (setq-local yas-indent-line 'fixed)))

;; If you use quarto-mode specifically:
(add-hook 'quarto-mode-hook
          (lambda ()
            (setq-local yas-indent-line 'fixed)))



;; Make C-a, C-e, backspace work 

(defun my/beginning-of-line-smart ()
  "Use move-beginning-of-line always (works with multiple-cursors)."
  (interactive)
  (move-beginning-of-line nil))

(defun my/end-of-line-smart ()
  "Use move-end-of-line always (works with multiple-cursors)."
  (interactive)
  (move-end-of-line nil))

(defun my/backspace-smart ()
  "Backspace that works reliably with multiple-cursors."
  (interactive)
  (backward-delete-char-untabify 1))

(use-package markdown-mode
  :config
  (setq markdown-special-ctrl-a/e nil)
  (define-key markdown-mode-map (kbd "C-a") 'my/beginning-of-line-smart)
  (define-key markdown-mode-map (kbd "C-e") 'my/end-of-line-smart)
  (define-key markdown-mode-map (kbd "<backspace>") 'my/backspace-smart))

(use-package multiple-cursors
  :ensure t
  :demand t
  :bind* (("C-S-c C-S-c" . mc/edit-lines)
          ("C-;"          . mc/mark-next-like-this)
          ("C-c <"        . mc/mark-previous-like-this)
          ("C-c C->"      . mc/skip-to-next-like-this)
          ("C-c C-<"      . mc/skip-to-previous-like-this)
          ("C-c m a"      . mc/mark-all-like-this)
          ("M-<mouse-1>"  . mc/add-cursor-on-click))
  :config
  (global-unset-key (kbd "M-<down-mouse-1>")))

(with-eval-after-load 'multiple-cursors
  (dolist (cmd '(backward-delete-char-untabify
                 delete-backward-char
                 delete-forward-char
                 delete-char
                 backward-kill-word
                 kill-word
                 move-beginning-of-line
                 move-end-of-line
                 my/beginning-of-line-smart
                 my/end-of-line-smart        ;; <-- added
                 my/backspace-smart
                 markdown-outdent-or-delete
                 markdown-electric-backspace
                 markdown-beginning-of-line
                 markdown-end-of-line))      ;; <-- added
    (add-to-list 'mc/cmds-to-run-for-all cmd)))
