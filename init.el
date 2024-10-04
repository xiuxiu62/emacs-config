;;; Package --- Summary
;;; Commentary:
;;; Xiu-Macs
;;; Code:

;; temporary until config is good enough to turn this off
(defvar init-file-path
  (pcase system-type
    ('windows-nt "C:/Users/jacre/AppData/Roaming/.emacs.d/init.el")
    ('gnu/linux "/home/xiuxiu/.emacs.d/init.el")
    (_ "")))
(add-hook 'emacs-startup-hook (lambda () (find-file init-file-path)))

;; Disable default ui
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(column-number-mode 1)
(electric-pair-mode 1) ;; auto close parens
(show-paren-mode 1)    ;; show matching paren
(fido-vertical-mode 1) ;; better minibuffer completions

(global-display-line-numbers-mode)

;; Save minibuffer history
(savehist-mode 1) ;; TODO: maybe remove

(setq inhibit-startup-message t           ;; Disable splash screen
      visible-bell t                      ;; Disable bell sound
      make-backup-files nil               ;; Disable backup files
      auto-save-default nil               ;; Disable auto save files
      history-length 25                   ;; Set minibuffer history size
      display-line-numbers-type 'relative
      mouse-wheel-scroll-amount '(3 ((shift) . 1))  ;; Scroll slower when shift is held
      mouse-wheel-progressive-speed nil
      display-line-numbers 'relative
      custom-file (make-temp-file "emacs-custom"))

(add-hook 'prog-mode-hook 'flymake-mode)

(defvar minibuffer-eldef-shorten-default t) ;; Remember the last minibuffer input
;; (defvar recentf-max-menu-items 25)
;; (defvar recentf-max-saved-items 25)

;; Keep track of recently opened files
;; (recentf-mode 1) ;; TODO: maybe remove
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

(load-theme 'modus-vivendi t)

;; Font
(set-face-attribute 'default nil :height 110)

;; Window
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(80 . 70))
(add-to-list 'default-frame-alist '(alpha . (80 . 70)))

(setq-default mode-line-format
      '("%e"
	mode-line-front-space
	mode-line-buffer-identification
	mode-line-modified
	"  "
	mode-line-position
	vc-mode
	"  "
 	mode-line-modes
	mode-line-misc-info
	mode-line-end-spaces))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defmacro install (&rest packages)
  "Install a package if it isn't already installed."
  `(mapc (lambda (package)
	   (unless (package-installed-p package)
	     (package-install package)))
	 ',packages))

(defmacro config (package &rest body)
  "Configure a package after it's loaded."
  `(with-eval-after-load 'package ,@body))

(install which-key
	 general
	 evil
	 evil-collection
	 evil-commentary
	 magit)

(config which-key
	(which-key-mode)
	(setq which-key-idle-delay 0
	      which-key-idle-secondary-delay 0))

(config evil
	(setq evil-want-integration t
	      evil-want-keybinding nil)
	(evil-mode 1))

(config evil-commentary
	(evil-commentary-mode))

(config general
	(general-auto-unbind-keys))
	;; (general-evil-setup)

(general-create-definer leader
  :prefix "SPC")

(leader
  :keymaps 'normal
  ";" 'evil-commentary-line
  :keymaps 'visual
  ";" 'evil-commentary-line
  :keymaps 'emacs
  "e" '(:ignore t :wk "eval")
  "eb" '(eval-buffer :wk "buffer")
  "ee" '(eval-last-sexp :wk "last")
  "f" '(:ignore t :wk "file")
  "ff" '(find-file :wk "find")
  "x" '(execute-extended-command :wk "commands")
  "w" '(:ignore t :wk "window")
  "wh" '(windmove-left :wk "left")
  "wj" '(windmove-down :wk "down")
  "wk" '(windmove-up :wk "up")
  "wl" '(windmove-right :wk "right")
  "ws" '(:ignore t :wk "split")
  "wsh" '(split-window-horizontally :wk "horizontal")
  "wsv" '(split-window-vertically :wk "vertical")
  "wd" '(:ignore t :wk "delete")
  "wdd" '(delete-window :wk "active")
  "wdo" '(delete-other-windows :wk "others"))

(config eglot
	(add-hook 'python-mode-hook 'elgot-ensure)
	(add-hook 'rust-mode-hook 'elgot-ensure)
	(setq eglot-autoshutdown 1
	      eglot-confirm-server-initiated-edits nil))

;; Evaluate buffer and reload init file
(defun reload-init-file ()
  "Evaluate the current buffer (assuming it's the init file) and reload init file."
  (interactive)
  (save-buffer)
  (eval-buffer)
  (load-file user-init-file))
(global-set-key (kbd "C-c r") 'reload-init-file)

(provide 'init)
;;; init.el ends here
