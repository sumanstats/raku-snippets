;; Disable unnecessary GUI elements
(tool-bar-mode -1)
;; (menu-bar-mode -1)			


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
(load-theme 'solarized-dark t)
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
  (tab-bar-new-tab-to 'right)     ;; new tab to the right side
  (find-file filename wildcards))

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
(yas-global-mode 1)

;; Company basic settings
(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-align-annotations t)
(setq company-dabbrev-ignore-case nil)
(setq company-dabbrev-downcase nil)
(setq dabbrev-case-replace nil)
(setq company-selection-wrap-around t)
(setq company-dabbrev-char-regexp "\\sw\\|\\s_")

;; Appearance (Nord-inspired)
(set-face-attribute 'company-tooltip nil
  :background "#3b4252"
  :foreground "#d8dee9"
  :height 1.15
  :box '(:line-width 1 :color "#4c566a"))
(set-face-attribute 'company-tooltip-selection nil
  :background "#5e81ac"
  :foreground "#eceff4"
  :weight 'bold)
(set-face-attribute 'company-tooltip-common nil
  :foreground "#88c0d0"
  :weight 'bold)
(set-face-attribute 'company-tooltip-annotation nil
  :foreground "#81a1c1"
  :slant 'italic
  :height 0.9)

;; Add dabbrev backend
(setq company-backends '(company-dabbrev company-capf company-files))

;; Wrapper to include yasnippet without overriding other backends
(defun company-backend-with-yas (backend)
  "Add `company-yasnippet` to BACKEND so both appear in popup."
  (if (and (listp backend) (member 'company-yasnippet backend))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends
      (mapcar #'company-backend-with-yas company-backends))

;; Label snippets in popup
(defun company-format-margin-with-label (selected)
  "Show label [snippets] for yasnippet candidates."
  (let ((backend (company-call-backend 'prefix)))
    (cond
     ((eq backend 'company-yasnippet) " [snippets] ")
     (t " "))))
(setq company-format-margin-function #'company-format-margin-with-label)

;; Keybindings for company
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "RET") 'company-complete-selection)

;; Global company mode
(global-company-mode 1)

;; Yasnippet backend in company
(add-to-list 'company-backends 'company-yasnippet)

;; -------------------------------
;; Load markdown snippets
;; -------------------------------
(load-file "./markdown-snippets.el")

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
