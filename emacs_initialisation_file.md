### My emacs initialisation file for running R in Emacs with ESS



For this install `company` mode, `ess`, `solarised theme`, `elpy`, `yasnippet-snippets`, `smartparens` and `lsp-mode`. Put this configuration in `init.el` inside `.emacs.d` directory.

```
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)
(setq default-directory "c:/Users/suman/")
(setq create-lockfiles nil)
(setq inhibit-startup-message t)
(setq inferior-ess-r-program "R")
(setq ess-tab-complete-in-script 1)
(setq ess-ask-for-ess-directory t)
(load-theme 'solarized-light t)
;;(elpy-enable)
(add-hook 'after-init-hook 'global-company-mode)
(set-face-attribute 'default nil :font "JetBrains Mono-18")
;;(add-to-list 'default-frame-alist '(height . 30))
;;(add-to-list 'default-frame-alist '(width . 115))
;(require 'lsp-mode)
;(lsp-mode 1) 
(elpy-enable)
(require 'yasnippet)
(yas-global-mode 1)
(require 'yasnippet-snippets)
(add-hook 'yas-minor-mode-hook (lambda ()
				 (yas-activate-extra-mode 'fundamental-mode)))
(require 'smartparens-config)
(smartparens-global-mode t)
```


To enable `markdown-mode`, install as in their docs.
