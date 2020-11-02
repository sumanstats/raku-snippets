### My emacs initialisation file for running R in Emacs with ESS



For this install `company` mode, ess, solarised theme and lsp-mode.

```
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(lsp-mode company solarized-theme ess)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(setq inferior-ess-r-program "R")
(load-theme 'solarized-dark t)
(add-hook 'after-init-hook 'global-company-mode)
(set-face-attribute 'default nil :family "Consolas" :height 140)
(add-to-list 'default-frame-alist '(height . 35))
(add-to-list 'default-frame-alist '(width . 90))
(require 'lsp-mode)
(lsp-mode 1)
```
