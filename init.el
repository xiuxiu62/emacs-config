;;; Package --- Summary
;;; Commentary:
;;; Xiu-Macs
;;; Code:

(setq inhibit-startup-message t           ;; Disable splash screen
      visible-bell t                      ;; Disable bell sound
      make-backup-files nil               ;; Disable backup files
      auto-save-default nil               ;; Disable auto save files
      history-length 25                   ;; Set minibuffer history size
      mouse-wheel-scroll-amount '(3 ((shift) . 1))  ;; Scroll slower when shift is held
      mouse-wheel-progressive-speed nil)

(defvar minibuffer-eldef-shorten-default t) ;; Remember the last minibuffer input
(defvar recentf-max-menu-items 25)
(defvar recentf-max-saved-items 25)

;; Save minibuffer history
(savehist-mode 1) ;; TODO: maybe remove

;; Keep track of recently opened files
(recentf-mode 1) ;; TODO: maybe remove
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Disable default ui
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-display-line-numbers-mode 1)

(load-theme 'deeper-blue t)

;; Font options
(set-face-attribute 'default nil :height 115)

;; Window options
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(90 . 80))
(add-to-list 'default-frame-alist '(alpha . (90 . 80)))

;; Package manager bootstrap
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
    (straight-use-package 'use-package)

;; temporary until config is good enough to turn this off
(defvar init-file-path
  (pcase system-type
    ('windows-nt "C:/Users/jacre/AppData/Roaming/.emacs.d/init.el")
    ('gnu/linux "/home/xiuxiu/.config/.emacs.d/init.el")
    (_ "")))
(add-hook 'emacs-startup-hook (lambda () (find-file init-file-path)))

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package evil
  :straight t
  :ensure t
  :init
  (setq evil-want-integration t
	evil-want-keybinding nil
	evil-vsplit-window-right t
	evil-split-window-below t)
  (evil-mode))

(use-package evil-collection
  :straight t
  :after evil
  :ensure t
  :config (evil-collection-init))

(use-package evil-surround
  :straight t
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-commentary
  :straight t
  :ensure t
  :ensure t
  :config (evil-commentary-mode))

(use-package evil-leader
  :straight t
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "f" 'find-file
    "b" 'switch-to-buffer
    "k" 'kill-buffer
    "w" 'save-buffer
    "e" 'eval-last-sexp
    ";" 'evil-commentary-line))

(use-package vertico
  :straight t
  :init (vertico-mode)
  :custom (vertico-cycle t))

(use-package marginalia
  :straight t
  :after vertico
  :custom (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init (marginalia-mode))

(use-package consult
  :straight t
  :bind (("C-s" . consule-line))
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  (setq register-preview-delay 0.1
	register-preview-function #'consult-register-format))

(use-package embark
  :straight t
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
		 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :straight t
  :ensure t
  :init (icomplete-mode)
  :custom
  (completion-styles '(orderless basic))
  (completion--category-override '((file (styles . (partial-completion))))))

;; Company for auto-completion
(use-package company
  :straight t
  :hook (emacs-lisp-mode . company-mode)
  :bind ("<tab>" . company-indent-or-complete-common)
  :config
  (setq company-idle-delay 0.1
        company-minimum-prefix-length 1))

(use-package eldoc
  :straight t
  :diminish eldoc-mode
  :config (global-eldoc-mode 1))

(use-package helpful
  :straight t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-h x" . helpful-command)
   ("C-h C-d" . helpful-at-point)
   ("C-h F" . helpful-function)))

(use-package highlight-defined
  :straight t
  :hook (emacs-lisp-mode . highlight-defined-mode))

(use-package flycheck
  :straight t
  :init (global-flycheck-mode))

(use-package aggressive-indent
  :straight t
  :hook (emacs-lisp-mode . aggressive-indent-mode))

(use-package suggest
  :straight t)

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :hook ((python-mode . lsp)
         (rust-mode . lsp)
         (js-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :straight t
  :commands lsp-ui-mode)

;; Code formatting
(use-package apheleia
  :straight t
  :config
  (apheleia-global-mode +1))

;; Evaluate buffer and reload init file
(defun reload-init-file ()
  "Evaluate the current buffer (assuming it's the init file) and reload init file."
  (interactive)
  (save-buffer)
  (eval-buffer)
  (load-file user-init-file))
(global-set-key (kbd "C-c r") 'reload-init-file)

(provide 'init)
