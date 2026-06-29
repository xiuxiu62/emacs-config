;;; init.el --- summary
;;; Commentary:
;;; Code:

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))
(setq inhibit-startup-echo-area-message t)
(setq inhibit-scratch-message nil)
(setq ring-bell-function 'ignore)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode t)
(set-face-attribute 'default nil :height 130)
(setq mouse-wheel-scroll-amount '(7 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq xref-search-program 'ripgrep)
(setq compilation-scroll-output t)
(column-number-mode t)
;; (add-hook 'after-init-hook #'toggle-frame-maximized)
(add-to-list 'default-frame-alist '(alpha . (95 . 95)))

(add-to-list 'load-path (expand-file-name "modes" user-emacs-directory))

(let ((git-bin-dir "C:/Program Files/Git/usr/bin"))
  (when (file-directory-p git-bin-dir)
    (add-to-list 'exec-path git-bin-dir)
    (setenv "PATH" (concat git-bin-dir ";" (getenv "PATH")))))

(defun open-init-file ()
  "Open init.el"
  (interactive)
  (find-file user-init-file))

(setq explicit-shell-file-name "powershell")
(setq explicit-powershell-args '("-NoLogo" "-ExecutionPolicy" "Bypass"))
(setq shell-file-name "powershell")
(setq shell-command-switch "-Command")

(defun open-shell-here ()
  (interactive)
  (let ((default-directory (if buffer-file-name
                               (file-name-directory buffer-file-name)
                             default-directory)))
    (shell (generate-new-buffer-name "*powershell*"))))

(defun open-shell-project ()
  (interactive)
  (let ((default-directory (or (projectile-project-root)
                                 default-directory)))
    (shell (generate-new-buffer-name "*powershell*"))))

(defvar resize-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "h") (lambda () (interactive) (evil-window-increase-width 10)))
    (define-key map (kbd "l") (lambda () (interactive) (evil-window-decrease-width 10)))
    (define-key map (kbd "k") (lambda () (interactive) (evil-window-increase-height 5)))
    (define-key map (kbd "j") (lambda () (interactive) (evil-window-decrease-height 5)))
    (define-key map (kbd "q") 'exit-resize-mode)
    (define-key map (kbd "ESC") 'exit-resize-mode)
    map)
  "Keymap for window resizing mode.")

(defun enter-resize-mode ()
  "Enter a transient mode where h j k l resize the current window.
Press q or ESC to exit."
  (interactive)
  (message "Resize mode: j/k for height, h/l for width, q/ESC to exit")
  (set-transient-map resize-mode-map t
                     (lambda ()
                       (message "Resize mode exited"))))

(defun exit-resize-mode ()
  "Exit resize mode."
  (interactive)
  (message "Resize mode exited"))

;; ── Auto-close pairs ─────────────────────────────────────────────────
(electric-pair-mode 1)
(setq electric-pair-inhibit-predicate
      (lambda (c)
        (if (char-equal c ?<) t (electric-pair-default-inhibit c))))

;; ── Package bootstrap ────────────────────────────────────────────────
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'use-package)
(setq use-package-always-ensure t)
(unless package-archive-contents
  (package-refresh-contents))

;; ── Theme ────────────────────────────────────────────────────────────
(use-package doom-themes
  :config
  (load-theme 'doom-dracula t)
  (set-face-attribute 'default nil :background "#1e1f29")
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; ── Modeline ─────────────────────────────────────────────────────────
(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-height 28)
  (doom-modeline-bar-width 4)
  (doom-modeline-icon nil)               ; set to t if you have nerd-fonts installed
  (doom-modeline-major-mode-icon nil)
  (doom-modeline-buffer-encoding nil)    ; hide encoding (usually utf-8, not useful)
  (doom-modeline-checker-simple-format t) ; compact error/warning counts
  (doom-modeline-lsp t)                  ; show LSP status
  (doom-modeline-column-zero-based nil)  ; 1-based column numbers
  (doom-modeline-percent-position nil)   ; hide file % position
  (doom-modeline-buffer-file-name-style 'relative-from-project)) ; show path from project root

;; ── Evil ─────────────────────────────────────────────────────────────
(use-package evil
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil
        evil-want-C-u-scroll nil)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd "SPC"))

  (evil-define-key '(normal visual) 'global (kbd "U") 'evil-redo)

  ;; Comment toggle
  (evil-define-key 'normal 'global (kbd "C-/")
    (lambda () (interactive)
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))))
  (evil-define-key 'visual 'global (kbd "C-/")
    (lambda () (interactive)
      (let ((start (save-excursion (goto-char (region-beginning)) (line-beginning-position)))
            (end   (save-excursion (goto-char (region-end))       (line-end-position))))
        (comment-or-uncomment-region start end))))

  ;; Leader bindings
  (evil-define-key '(normal visual) 'global (kbd "<leader>a")  'eglot-code-actions)
  (evil-define-key 'normal 'global (kbd "<leader>f") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>s") 'consult-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>b") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>r") 'eglot-rename)
  (evil-define-key 'normal 'global (kbd "<leader>c") 'compile)
  (evil-define-key 'normal 'global (kbd "<leader>,") 'open-init-file)
  (evil-define-key 'normal 'global (kbd "<leader>k") 'eldoc-box-help-at-point)

  ;; Project
  (evil-define-key 'normal 'global (kbd "<leader>p") 'projectile-switch-project)
  ;; (evil-define-key 'normal 'global (kbd "<leader>pn") 'reponotes-menu)

  ;; Terminal
  (evil-define-key 'normal 'global (kbd "<leader>tt") 
    (lambda () (interactive) (shell (generate-new-buffer-name "*powershell*"))))
  (evil-define-key 'normal 'global (kbd "<leader>tp") 'open-shell-project)
  (evil-define-key 'normal 'global (kbd "<leader>th") 'open-shell-here)

  ;; Window management
  (evil-define-key 'normal 'global (kbd "<leader>wh") 'evil-window-left)
  (evil-define-key 'normal 'global (kbd "<leader>wj") 'evil-window-down)
  (evil-define-key 'normal 'global (kbd "<leader>wk") 'evil-window-up)
  (evil-define-key 'normal 'global (kbd "<leader>wl") 'evil-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>wv") 'evil-window-vsplit)
  (evil-define-key 'normal 'global (kbd "<leader>ws") 'evil-window-split)
  (evil-define-key 'normal 'global (kbd "<leader>wq") 'evil-window-delete)
  (evil-define-key 'normal 'global (kbd "<leader>wr") 'enter-resize-mode) 

  ;; Error navigation
  (evil-define-key 'normal 'global (kbd "]e") 'flymake-goto-next-error)
  (evil-define-key 'normal 'global (kbd "[e") 'flymake-goto-prev-error)
  (evil-define-key 'normal 'global (kbd "<leader>el") 'flymake-show-buffer-diagnostics)
  (evil-define-key 'normal 'global (kbd "<leader>eL") 'flymake-show-project-diagnostics)

  (evil-define-key 'normal 'global (kbd "<leader>n")
    (lambda () (interactive)
      (let ((default-directory (if buffer-file-name
                                   (file-name-directory buffer-file-name)
                                 default-directory)))
        (call-interactively #'find-file)))))

(use-package evil-collection
  :after evil
  :init
  (setq evil-collection-want-unimpaired-p nil)
  :config
  (evil-collection-init))

;; ── Completion UI ────────────────────────────────────────────────────
(use-package posframe :ensure t)

(use-package vertico
  :init (vertico-mode 1))

;; vertico-multiform lets us send specific commands to posframe vs buffer display
(use-package vertico-multiform
  :ensure nil  ; ships with vertico
  :after vertico
  :config
  (vertico-multiform-mode 1)
  :custom
  (vertico-multiform-commands
   '((consult-buffer     buffer)   ; full-width buffer switcher
     (consult-ripgrep    buffer)   ; full-width search results
     (consult-line       buffer)   ; full-width line search
     (consult-goto-line  buffer)
     (consult-recent-file buffer)
     (t                  posframe)))) ; everything else (M-x, code actions, etc.) → popup

(use-package vertico-posframe
  :after (vertico posframe)
  :config (vertico-posframe-mode 1)
  :custom
  (vertico-posframe-width 90)
  (vertico-posframe-height 15)
  (vertico-posframe-poshandler #'posframe-poshandler-point-bottom-left-corner)
  (vertico-posframe-border-width 2)
  (vertico-posframe-parameters '((left-fringe . 8) (right-fringe . 8))))

(use-package marginalia
  :init (marginalia-mode 1))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles orderless basic)))))

(use-package corfu
  :custom
  (corfu-cycle t)
  (corfu-auto t)
  (corfu-separator ?\s)
  (corfu-quit-at-boundary t)
  :bind (:map corfu-map
              ("RET"       . corfu-insert)
              ("TAB"       . corfu-next)
              ("<backtab>" . corfu-previous)
              ("C-n"       . corfu-next)
              ("C-p"       . corfu-previous))
  :init (global-corfu-mode 1))

(use-package consult
  :custom
  (consult-ripgrep-args
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --line-number")
  :bind (("C-s"     . consult-line)
         ("M-y"     . consult-yank-pop)
         ("C-x b"   . consult-buffer)
         ("C-x C-r" . consult-recent-file)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)))

(use-package which-key
  :config (which-key-mode 1)
  :custom (which-key-idle-delay 0.3))

;; ── Errors ───────────────────────────────────────────────────────────
(use-package flymake
  :ensure nil
  :custom
  (flymake-fringe-indicator-position 'left-fringe)
  (flymake-show-diagnostics-at-end-of-line f))

;; ── Projects ─────────────────────────────────────────────────────────
;; (defun projectile-get-submodule-command (dir)
;;   "Return a shell command to list submodules for DIR.
;; Uses `git submodule status` and extracts paths without requiring 'tr'."
;;   (format "git -C \"%s\" submodule status --cached --recursive | ForEach-Object { $_.Split()[1] }" dir))

(use-package projectile
  :ensure t
  :after vertico
  :init
  (setq projectile-mode-line-prefix " Proj")
  (setq projectile-indexing-method 'hybrid)       ; faster, uses external commands
  (setq projectile-generic-command "rg --files --hidden --follow --no-ignore-vcs")
  (setq projectile-git-submodule-command nil)
  :config
  (projectile-mode +1))

(require 'reponotes)

;; ── LSP (eglot) ──────────────────────────────────────────────────────
(use-package eglot
  :ensure nil
  :hook ((c-mode c++-mode jai-mode glsl-mode) . eglot-ensure)
  :config
  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode) . ("clangd"
                                      "--background-index"
                                      "--clang-tidy"
                                      "--completion-style=detailed"
                                      "--header-insertion=never")))
  (add-to-list 'eglot-server-programs
               '(jai-mode . ("jails.exe")))
  (add-to-list 'eglot-server-programs
               '(glsl-mode . ("glsl_analyzer" "--stdio")))
  (setq eglot-inlay-hints-mode -1)
  (setq eglot-autoshutdown t))

;; Hover popups (for <leader>k)
(use-package eldoc-box
  :after eglot
  :hook (eglot-managed-mode . eldoc-box-hover-mode))

;; ── Diagnostics toggle (hints & error messages) ─────────────────────
(defvar flux-diagnostics-enabled nil
  "Whether diagnostics (flymake) and inlay hints (eglot) are shown.")

(defun flux-disable-diagnostics ()
  "Turn off inline type hints and diagnostic overlays."
  (interactive)
  (setq flux-diagnostics-enabled nil)
  ;; Disable eglot inlay hints in the current buffer
  (when (and (boundp 'eglot--managed-mode) eglot--managed-mode)
    (eglot-inlay-hints-mode -1))
  ;; Hide flymake diagnostics globally
  (setq flymake-show-diagnostics-at-end-of-line nil)
  (setq flymake-fringe-indicator-position nil)
  (message "Diagnostics and hints disabled"))

(defun flux-enable-diagnostics ()
  "Turn on inline type hints and diagnostic overlays."
  (interactive)
  (setq flux-diagnostics-enabled t)
  ;; Enable eglot inlay hints in the current buffer
  (when (boundp 'eglot--managed-mode)
    (eglot-inlay-hints-mode 1))
  ;; Show flymake diagnostics globally
  (setq flymake-show-diagnostics-at-end-of-line t)
  (setq flymake-fringe-indicator-position 'left-fringe)
  (message "Diagnostics and hints enabled"))

(defun flux-toggle-diagnostics ()
  "Toggle all diagnostics and inline type hints."
  (interactive)
  (if flux-diagnostics-enabled
      (flux-disable-diagnostics)
    (flux-enable-diagnostics)))

;; Start with everything disabled
(flux-disable-diagnostics)

;; Bind <leader>d to the toggle function
(evil-define-key 'normal 'global (kbd "<leader>d") 'flux-toggle-diagnostics)

;; ── C/C++ ────────────────────────────────────────────────────────────
(use-package cc-mode
  :ensure nil
  :hook (c++-mode . (lambda ()
                      (setq indent-tabs-mode nil)
                      (setq c-basic-offset 4)
                      (setq tab-width 4)
                      (font-lock-add-keywords nil
                        '(("\\<\\(module\\|import\\|export\\)\\>" . font-lock-keyword-face)))
                      (add-hook 'before-save-hook
                                (lambda ()
                                  (when (eglot-managed-p)
                                    (eglot-format-buffer)))
                                nil t)))
  :mode (("\\.h\\'"    . c++-mode)
         ("\\.cpp\\'"  . c++-mode)
         ("\\.cppm\\'" . c++-mode)
         ("\\.hpp\\'"  . c++-mode)))

(use-package cmake-mode
  :mode (("CMakeLists\\.txt'" . cmake-mode)
	 ("\\.cmake\\'"       . cmake-mode))
  :hook (cmake-mode . (lambda ()
			(setq indent-tabs-mode nil)
			(setq cmake-tab-width 4)
			(setq tab-width 4))))

;; ── PATH (Windows) ───────────────────────────────────────────────────
(setenv "PATH" (concat "C:\\jai-beta-2-029\\bin;" (getenv "PATH")))
(add-to-list 'exec-path "C:\\jai-beta-2-029\\bin")

;; ── Jai ──────────────────────────────────────────────────────────────
(require 'jai-mode)

(add-hook 'jai-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (local-set-key (kbd "TAB") 'indent-for-tab-command)))

;; ── Slang ────────────────────────────────────────────────────────────
(require 'slang-mode)

(add-hook 'slang-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)))

;; ── GLSL ─────────────────────────────────────────────────────────────
(use-package glsl-mode
  :mode (("\\.glsl\\'" . glsl-mode)
         ("\\.vert\\'" . glsl-mode)
         ("\\.frag\\'" . glsl-mode)
         ("\\.geom\\'" . glsl-mode)
         ("\\.comp\\'" . glsl-mode)
         ("\\.tesc\\'" . glsl-mode)
         ("\\.tese\\'" . glsl-mode)
         ("\\.mesh\\'" . glsl-mode)
         ("\\.task\\'" . glsl-mode)
         ("\\.rgen\\'"  . glsl-mode)
         ("\\.rint\\'"  . glsl-mode)
         ("\\.rahit\\'" . glsl-mode)
         ("\\.rchit\\'" . glsl-mode)
         ("\\.rmiss\\'" . glsl-mode)
         ("\\.rcall\\'" . glsl-mode))
  :hook (glsl-mode . (lambda ()
                       (setq indent-tabs-mode nil)
                       (setq c-basic-offset 4)
                       (setq tab-width 4))))


;; ── Cleanup ──────────────────────────────────────────────────────────
(setq read-extended-command-predicate
      (lambda (sym _buffer)
        (not (eq sym 'run-php))))
