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
(message "Configuration file loaded.")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(company ess markdown-mode solarized-theme yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
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
(custom-set-faces
 '(company-tooltip ((t (:background "#2e3440" :foreground "#d8dee9"))))
 '(company-tooltip-selection ((t (:background "#4c566a" :foreground "#eceff4" :weight bold))))
 '(company-tooltip-common ((t (:foreground "#88c0d0" :weight bold))))
 '(company-tooltip-annotation ((t (:foreground "#ebcb8b"))))
)
;; (add-hook 'text-mode-hook 'turn-on-visual-line-mode)
;; Preserve original case when completing with dabbrev/company-dabbrev
(setq dabbrev-case-replace nil)        ; Preserve case of original word
(setq dabbrev-case-fold-search t)      ; Allow "wo" to match "World"
(setq company-dabbrev-ignore-case t)   ; Make company-dabbrev case-insensitive in search
(setq-default fill-column 80) ; Set default width to 80 characters
;; Use pandoc for exporting
(setq markdown-command "pandoc")
;; Set the default coding system to UTF-8
(prefer-coding-system 'utf-8)
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)
