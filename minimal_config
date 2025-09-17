;; Disable unnecessary GUI elements
(tool-bar-mode -1)
(tab-bar-mode 1)
(global-visual-line-mode 1)



;; -------------------------------
;; Package setup
;; -------------------------------
(require 'package)
(setq package-archives nil)
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

;; -------------------------------
;; Company + Yasnippet setup
;; -------------------------------
(require 'yasnippet)


;; Enable modes
(yas-global-mode 1)
(setq yas-snippet-dirs '("./snippets/")) ;; path to snippets

(yas-reload-all)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(cobalt ess monokai-theme solarized-theme yasnippet)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
