(require 'package)
(setq package-archives nil)
;; Add only MELPA to the package archives
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(setq default-directory "C:/Users/suman")
(global-display-line-numbers-mode t)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(set-frame-font "Consolas-18" nil t)
(load-theme 'solarized-light t)
(setq inferior-R-program-name "D:/R-4.5.1/bin/R.exe")

(setq make-backup-files nil)
(setq create-lockfiles nil)
(defun title-case (start end)
  "Convert region to title case, capitalizing only the first letter of each word."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (re-search-forward "\\b\\w" nil t)
        (replace-match (upcase (match-string 0)) t t)))))
;; Load company mode
(require 'company)

;; Enable company-dabbrev (completes from buffer words)
(add-to-list 'company-backends 'company-dabbrev)

;; Optional: also complete from other buffers

(setq company-dabbrev-char-regexp "\\sw\\|\\s_")
;; Show completion popup automatically as you type (after 2 chars)
(setq company-idle-delay 0.1)   ; show after 0.1s pause in typing
(setq company-minimum-prefix-length 2) ; start after 2 chars typed

;; Enable company-mode globally (or just in text modes)
(global-company-mode 1)

;; Optional: make TAB select suggestion, RET insert and newline
(define-key company-active-map (kbd "TAB") 'company-complete-selection)
(define-key company-active-map (kbd "<tab>") 'company-complete-selection)
(define-key company-active-map (kbd "RET") 'company-complete-selection)
(setq company-tooltip-align-annotations t)


;; Preserve original case when completing with dabbrev/company-dabbrev
(setq dabbrev-case-replace nil)        ; Preserve case of original word
(setq dabbrev-case-fold-search t)      ; Allow "wo" to match "World"
(setq company-dabbrev-ignore-case nil)   ; Make company-dabbrev case-sensitive in search

;; Use pandoc for exporting
(setq markdown-command "pandoc")
;; Set the default coding system to UTF-8
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; Nord-like company-mode popup
(set-face-attribute 'company-tooltip nil
  :background "#3b4252"          ; Nord Polar Night
  :foreground "#d8dee9"          ; Nord Snow Storm
  :height 1.15
  :box '(:line-width 1 :color "#4c566a"))

(set-face-attribute 'company-tooltip-selection nil
  :background "#5e81ac"          ; Nord Aurora Blue
  :foreground "#eceff4"
  :weight 'bold)

(set-face-attribute 'company-tooltip-common nil
  :foreground "#88c0d0"          ; Nord Light Blue (prefix)
  :weight 'bold)

(set-face-attribute 'company-tooltip-annotation nil
  :foreground "#81a1c1"          ; Nord Darker Blue
  :slant 'italic
  :height 0.9)

