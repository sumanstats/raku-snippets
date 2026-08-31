;;; -*- lexical-binding: t; -*-

;; =====================================================================
;; STARTUP PERFORMANCE: do this FIRST, before anything else loads
;; =====================================================================

;; `file-name-handler-alist' is consulted on every `require'/`load' to check
;; for remote files, compressed files, etc. During init we don't need any of
;; that, so unsetting it cuts a surprising amount of time off startup.
(defvar my/original-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Raise the GC threshold high during init so Emacs isn't stopping to
;; garbage-collect while it loads dozens of packages.
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore both, but to a *higher* steady-state than the Emacs
            ;; default (800kb), which GCs far too often once company/ivy/etc.
            ;; are active. ~16MB is a good balance of latency vs. pause time.
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist my/original-file-name-handler-alist)
            (message "Emacs ready in %s with %d garbage collections."
                     (emacs-init-time) gcs-done)))

;; =====================================================================
;; Cross-platform path helpers
;; =====================================================================
;; The old file hardcoded "C:/Users/skhanal/...", "D:/R-4.6.1/...", and
;; "D:/miniconda3/..." which only ever worked on one specific Windows
;; machine. Everything below resolves relative to the actual home
;; directory and/or searches PATH, so the same init.el works on Windows,
;; macOS, and Linux without edits.

;; On Windows, `~' resolves via the HOME env var, and falls back to
;; APPDATA (AppData\Roaming) when HOME isn't set -- which is almost never
;; what you want. USERPROFILE is the variable Windows itself always sets
;; to the real profile root (C:\Users\suman), so prefer that on Windows
;; and fall back to HOME/`~' everywhere else.
(defconst my/home-dir
  (file-name-as-directory
   (or (and (eq system-type 'windows-nt) (getenv "USERPROFILE"))
       (getenv "HOME")
       (expand-file-name "~/")))
  "The current user's home directory, resolved per-OS.")

(defconst my/lisp-dir (expand-file-name "myemacs/" my/home-dir)
  "Directory holding personal .el files (markdown-snippets, quarto, etc).")

(defun my/find-program (&rest candidates)
  "Return the first of CANDIDATES found on PATH via `executable-find'.
Each candidate can be a bare command name (\"R\", \"python\") so this
works regardless of OS or install location, instead of a hardcoded
drive-letter path."
  (seq-some #'executable-find candidates))

;; =====================================================================
;; UI / basic settings
;; =====================================================================
(tool-bar-mode -1)
(menu-bar-mode -1)
(horizontal-scroll-bar-mode 1)
(tab-bar-mode 1)
(global-visual-line-mode 1)

(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq coding-system-for-read 'utf-8
      coding-system-for-write 'utf-8)
(set-keyboard-coding-system 'utf-8)

(setq inhibit-startup-screen t
      initial-scratch-message nil
      use-short-answers t
      dired-listing-switches "-alv"
      make-backup-files nil
      create-lockfiles nil
      ;; Send auto-saves to the system temp dir instead of littering
      ;; every project directory with #files#.
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))
      default-directory my/home-dir
      ;; Larger read chunks -> noticeably snappier LSP/company output.
      read-process-output-max (* 1024 1024))

(delete-selection-mode t)
(which-function-mode 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(global-display-line-numbers-mode t)

(add-hook 'emacs-startup-hook #'toggle-frame-maximized)
(when (member "Consolas" (font-family-list))
  (set-frame-font "Consolas-18" nil t))

;; Keep Custom's noise out of this file entirely.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

;; =====================================================================
;; Package setup
;; =====================================================================
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
;; Package initialize is invoked automatically at startup as of recent
;; Emacs unless package-enable-at-startup is nil in early-init.el; calling
;; it again here is a harmless no-op safeguard for older setups.
(package-initialize)

;; Bootstrapping use-package needs a working internet connection the
;; very first time (to fetch archive contents). Wrap it so a flaky
;; network, VPN, or corporate proxy doesn't take down your entire init
;; file with a cryptic backtrace -- you just get a message instead, and
;; everything below that doesn't depend on use-package still loads.
(unless (package-installed-p 'use-package)
  (condition-case err
      (progn
        (package-refresh-contents)
        (package-install 'use-package))
    (error
     (display-warning
      'init
      (format "Could not install use-package (%s). \
Check your network/proxy, then M-x package-install RET use-package RET."
              (error-message-string err))
      :error))))

(require 'use-package nil :noerror)
;; Defer everything by default; each package below opts back into eager
;; loading (:demand t) only where that's actually needed. This is the
;; single biggest lever for startup time in the original file, where
;; company/yasnippet/ivy/swiper/rainbow-delimiters/multiple-cursors/
;; all-the-icons-dired were all `require'd or start unconditionally.
(setq use-package-always-defer t
      use-package-always-ensure t
      ;; Quiets async native-comp warning popups that can steal focus.
      native-comp-async-report-warnings-errors 'silent)

;; =====================================================================
;; Utility functions (unchanged from your original)
;; =====================================================================
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

;; ---------------------------------------------------------------------
;; Tab <-> buffer tracking
;; ---------------------------------------------------------------------
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

(add-hook 'find-file-hook #'my/register-buffer-in-current-tab)
(add-hook 'after-save-hook #'my/register-buffer-in-current-tab)

(defun my/remove-mappings-for-tabname (tabname)
  "Remove all hash entries that point to TABNAME."
  (maphash (lambda (k v) (when (string= v tabname) (remhash k my/tab-buffer->tabname)))
           my/tab-buffer->tabname))

(advice-add 'tab-bar-close-tab :around
            (lambda (orig &rest args)
              (let ((closed-name (alist-get 'name (tab-bar--current-tab))))
                (apply orig args)
                (my/remove-mappings-for-tabname closed-name))))

(advice-add 'tab-bar-rename-tab :around
            (lambda (orig &rest args)
              (let ((old-name (alist-get 'name (tab-bar--current-tab))))
                (apply orig args)
                (let ((new-name (alist-get 'name (tab-bar--current-tab))))
                  (maphash (lambda (k v)
                             (when (string= v old-name)
                               (puthash k new-name my/tab-buffer->tabname)))
                           my/tab-buffer->tabname)))))

(add-hook 'kill-buffer-hook
          (lambda ()
            (when buffer-file-name
              (remhash (file-truename buffer-file-name) my/tab-buffer->tabname))))

;;;###autoload
(defun my-find-file-in-new-tab (filename &optional wildcards)
  "Open FILENAME in a new tab to the right, or jump to the tab recorded for it."
  (interactive
   (let ((completing-read-function #'completing-read-default)
         (read-file-name-function nil))
     (ivy-mode -1)
     (unwind-protect
         (find-file-read-args "Find file in new tab: " nil)
       (ivy-mode 1))))
  (cl-block nil
    (let* ((truename (and filename (file-truename filename)))
           (tabname  (and truename (gethash truename my/tab-buffer->tabname)))
           (existing-buffer (and truename (find-buffer-visiting truename)))
           (tabs (tab-bar-tabs)))
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
                (tab-bar-select-tab orig-index)
                (tab-bar-new-tab)
                (delete-other-windows)
                (find-file filename wildcards)
                (my/register-buffer-in-current-tab (current-buffer))
                (cl-return t)))
          (when (and orig-index
                     (not (eq (alist-get 'index (tab-bar--current-tab)) orig-index)))
            (tab-bar-select-tab orig-index)))))))

(global-set-key (kbd "C-x C-f") #'my-find-file-in-new-tab)

;; =====================================================================
;; Theme / fonts
;; =====================================================================
(use-package monokai-theme
  :demand t
  :config (load-theme 'monokai t))

;; =====================================================================
;; R / Python (cross-platform: search PATH instead of hardcoding a drive)
;; =====================================================================

(defvar my/r-program-path "D:/R-4.6.1/bin/R.exe"
  "Explicit path to the R executable, e.g. \"D:/R-4.6.1/bin/R.exe\".
Leave nil to auto-detect via PATH.")

(defvar my/python-program-path nil
  "Explicit path to the python executable.
Leave nil to auto-detect via PATH.")


(use-package ess
  :defer t
  :init
  (setq inferior-R-program-name
      (or my/r-program-path
          (my/find-program "R" "R.exe")
          "R")))

(setq org-babel-python-command
      (or my/python-program-path
          (my/find-program "python3" "python" "python.exe")
          "python3"))

;; =====================================================================
;; Markdown / pandoc
;; =====================================================================
(setq markdown-command "pandoc -f gfm -t html5 --standalone")

(use-package markdown-mode
  :defer t
  :hook (markdown-mode . (lambda () (setq-local yas-indent-line 'fixed)))
  :config
  (setq markdown-special-ctrl-a/e nil)
  (define-key markdown-mode-map (kbd "C-a") 'my/beginning-of-line-smart)
  (define-key markdown-mode-map (kbd "C-e") 'my/end-of-line-smart)
  (define-key markdown-mode-map (kbd "<backspace>") 'my/backspace-smart))

(use-package quarto-mode
  :defer t
  :hook (quarto-mode . (lambda () (setq-local yas-indent-line 'fixed))))

;; Personal .el files. Emacs 31 added a `user-lisp/' directory under
;; `user-emacs-directory' that is auto-byte-compiled and added to
;; load-path for you -- worth migrating markdown-snippets.el, quarto.el,
;; etc. into ~/.emacs.d/user-lisp/ over time so you can drop this block
;; entirely. Until then, this stays cross-platform via `my/lisp-dir'
;; instead of a hardcoded C:/Users/skhanal path.
(when (file-directory-p my/lisp-dir)
  ;; markdown-snippets.el calls `yas-define-snippets' at top level, so
  ;; yasnippet must actually be loaded (not just deferred) before these
  ;; files are read in.
  (require 'yasnippet)
  (add-to-list 'load-path my/lisp-dir)
  (dolist (feature '("markdown-snippets" "quarto" "distal_panc" "meld" "mch"))
    (load feature t)))

;; =====================================================================
;; Company + Yasnippet + frequent words
;; =====================================================================
(defvar my-frequent-words nil
  "List of frequently used words for autocompletion.")

(defconst my-frequent-words-dir
  (expand-file-name "myemacs/frequent_words/" my/home-dir))

(defun my/load-frequent-words-from-dir ()
  "Load words from all .txt files in `my-frequent-words-dir'."
  (interactive)
  (let ((words '()))
    (when (file-exists-p my-frequent-words-dir)
      (dolist (file (directory-files my-frequent-words-dir t "\\.txt\\'"))
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (while (not (eobp))
            (let ((line (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
              (unless (string-empty-p (string-trim line))
                (push (string-trim line) words)))
            (forward-line 1)))))
    (setq my-frequent-words (delete-dups (nreverse words)))))

;; This used to run synchronously at top-level, on every startup, before
;; you'd typed a single character -- reading every .txt file in the
;; directory off disk. Loading it on a short idle timer instead means
;; Emacs finishes starting up first, and the word list is ready a moment
;; later without blocking anything.
(run-with-idle-timer 1 nil #'my/load-frequent-words-from-dir)

(defun company-my-frequent-words (command &optional arg &rest ignored)
  "Company backend for frequent words. Trigger after 3 characters."
  (cl-case command
    (prefix (and (>= (length (company-grab-word)) 3)
                 (company-grab-word)))
    (candidates
     (cl-remove-if-not (lambda (w) (string-prefix-p arg w)) my-frequent-words))
    (annotation (propertize " [freq]" 'face 'company-frequent-words))
    (ignore-case t)))

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

(defun my/company-suspend-in-yas-field ()
  "Temporarily suspend company popup when in active yasnippet field."
  (setq-local company-idle-delay
              (if (and (boundp 'yas--active-field-overlay)
                       yas--active-field-overlay
                       (overlay-buffer yas--active-field-overlay))
                  nil
                0.05)))

(use-package yasnippet
  :hook ((prog-mode text-mode) . yas-minor-mode)
  :config
  ;; yas-global-mode already lazily discovers/loads snippets per-mode;
  ;; the explicit (yas-reload-all) in the original file re-scanned every
  ;; snippet directory again immediately at startup, which is redundant
  ;; startup cost. Removed.
  (add-hook 'post-command-hook #'my/company-suspend-in-yas-field))

(use-package company
  :hook ((prog-mode text-mode) . company-mode)
  :config
  (setq company-minimum-prefix-length 2
        company-idle-delay 0.05
        company-selection-wrap-around t
        company-backends '((company-dabbrev
                             company-my-frequent-words
                             company-capf
                             company-files
                             company-yasnippet))
        company-transformers '(company-sort-by-occurrence
                                company-sort-prefer-same-case-prefix)
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        dabbrev-abbrev-char-regexp "\\sw\\|\\s_"
        dabbrev-case-fold-search nil)
  (define-key company-active-map (kbd "TAB") 'company-complete-selection)
  (define-key company-active-map (kbd "<tab>") 'company-complete-selection)
  (define-key company-active-map (kbd "RET") 'company-complete-selection)
  (set-face-attribute 'company-tooltip nil
    :background "#2e3440" :foreground "#d8dee9" :height 1.1
    :box '(:line-width 1 :color "#4c566a"))
  (set-face-attribute 'company-tooltip-selection nil
    :background "#5e81ac" :foreground "#eceff4" :weight 'bold)
  (set-face-attribute 'company-tooltip-common nil
    :foreground "#88c0d0" :weight 'bold)
  (set-face-attribute 'company-tooltip-annotation nil
    :foreground "#81a1c1" :slant 'italic))

;; =====================================================================
;; Tab-bar styling
;; =====================================================================
(set-face-attribute 'tab-bar-tab nil
                    :background "#4c7ed9"
                    :foreground "#eceff4"
                    :weight 'bold
                    :box '(:line-width 2 :color "#81a1c1"))
(set-face-attribute 'tab-bar nil
                    :background "#2e3440"
                    :foreground "#d8dee9")

;; =====================================================================
;; Org-mode Babel
;; =====================================================================
(with-eval-after-load 'org
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((python . t)
     (R . t)
     (emacs-lisp . t)))
  (setq org-html-inline-images t
        org-confirm-babel-evaluate nil
        org-export-babel-evaluate t)
  (add-hook 'org-mode-hook 'org-display-inline-images))

;; htmlize is only needed when actually exporting org to highlighted
;; HTML, so autoload it instead of requiring it unconditionally at
;; startup.
(use-package htmlize :defer t)

(use-package move-text
  :demand t
  :config (move-text-default-bindings))

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; =====================================================================
;; File-name utility commands
;; =====================================================================
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

(global-set-key (kbd "C-c M-f") 'copy-parent-folder-and-file)

(defun copy-filename-only ()
  "Copy current buffer's filename (without path) to clipboard."
  (interactive)
  (when buffer-file-name
    (let ((filename (file-name-nondirectory buffer-file-name)))
      (kill-new filename)
      (message filename))))

(global-set-key (kbd "C-c f") 'copy-filename-only)

(defun reload-init-file ()
  "Reload Emacs init file."
  (interactive)
  (load-file user-init-file)
  (message "Config reloaded!"))

(global-set-key (kbd "C-c r") 'reload-init-file)

;; =====================================================================
;; Line editing helpers + multiple-cursors
;; =====================================================================
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

(use-package multiple-cursors
  :bind* (("C-S-c C-S-c" . mc/edit-lines)
          ("C-;"          . mc/mark-next-like-this)
          ("C-c <"        . mc/mark-previous-like-this)
          ("C-c C->"      . mc/skip-to-next-like-this)
          ("C-c C-<"      . mc/skip-to-previous-like-this)
          ("C-c m a"      . mc/mark-all-like-this)
          ("M-<mouse-1>"  . mc/add-cursor-on-click))
  :init
  (global-unset-key (kbd "M-<down-mouse-1>"))
  :config
  (dolist (cmd '(backward-delete-char-untabify
                 delete-backward-char
                 delete-forward-char
                 delete-char
                 backward-kill-word
                 kill-word
                 move-beginning-of-line
                 move-end-of-line
                 my/beginning-of-line-smart
                 my/end-of-line-smart
                 my/backspace-smart
                 markdown-outdent-or-delete
                 markdown-electric-backspace
                 markdown-beginning-of-line
                 markdown-end-of-line))
    (add-to-list 'mc/cmds-to-run-for-all cmd)))

;; =====================================================================
;; ivy / swiper
;; =====================================================================
(use-package ivy
  :demand t ;; genuinely needs to be ready before your first M-x / C-x C-f
  :config
  (ivy-mode 1)
  (setq ivy-wrap t
        ivy-use-selectable-prompt nil)
  (defun my/ivy-tab-only-partial ()
    (interactive)
    (ivy-partial))
  (define-key ivy-minibuffer-map (kbd "TAB") #'my/ivy-tab-only-partial)
  (define-key ivy-minibuffer-map (kbd "<tab>") #'my/ivy-tab-only-partial)
  (define-key ivy-minibuffer-map (kbd "S-TAB") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-done))

(use-package swiper
  :bind ("C-s" . swiper-isearch))

;; =====================================================================
;; Dired icons
;; =====================================================================
(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

;; =====================================================================
;; Rainbow delimiters
;; =====================================================================
(use-package rainbow-delimiters
  :hook ((markdown-mode quarto-mode) . rainbow-delimiters-mode)
  :config
  (let ((okabe-ito '("#E69F00" "#56B4E9" "#009E73" "#F0E442"
                     "#0072B2" "#D55E00" "#CC79A7" "#999999")))
    (dotimes (i (length okabe-ito))
      (let ((face (intern (format "rainbow-delimiters-depth-%d-face"
                                  (1+ i)))))
        (when (facep face)
          (set-face-attribute
           face nil
           :foreground (elt okabe-ito i)))))))
