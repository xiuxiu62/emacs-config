;;; Package --- Summary
;;; Commentary:
;;; Xiu-Macs
;;; Code:

;; temporary until config is good enough to turn this off
(defvar init-file-path
  (pcase system-type
    ('windows-nt "C:/Users/jacre/AppData/Roaming/.emacs.d/init.el")
    ('gnu/linux "/home/xiuxiu/.config/.emacs.d/init.el")
    (_ "")))
(add-hook 'emacs-startup-hook (lambda () (find-file init-file-path)))

(setq inhibit-startup-message t           ;; Disable splash screen
      visible-bell t                      ;; Disable bell sound
      make-backup-files nil               ;; Disable backup files
      auto-save-default nil               ;; Disable auto save files
      history-length 25                   ;; Set minibuffer history size
      mouse-wheel-scroll-amount '(3 ((shift) . 1))  ;; Scroll slower when shift is held
      mouse-wheel-progressive-speed nil)

(electric-pair-mode 1) ;; auto close parens
(show-paren-mode 1)    ;; show matching paren
(ido-mode 1)           ;; better buffer and file switching
(fido-mode 1)          ;; better minibuffer completions

(add-hook 'prog-mode-hook 'flymake-mode)

(defvar minibuffer-eldef-shorten-default t) ;; Remember the last minibuffer input
;; (defvar recentf-max-menu-items 25)
;; (defvar recentf-max-saved-items 25)

;; Save minibuffer history
(savehist-mode 1) ;; TODO: maybe remove

;; Keep track of recently opened files
;; (recentf-mode 1) ;; TODO: maybe remove
;; (global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Disable default ui
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

(global-display-line-numbers-mode 1)

(load-theme 'modus-vivendi t)

;; Font options
(set-face-attribute 'default nil :height 115)

;; Window options
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(set-frame-parameter (selected-frame) 'alpha '(80 . 70))
(add-to-list 'default-frame-alist '(alpha . (80 . 70)))

(require 'package)
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defun install (package)
  "Install a package if it isn't already installed."
  (unless (package-installed-p package)
    (package-install package)))

(defun install-many (packages)
  "Installs packages they aren't already installed."
  (mapc #'install packages))

(defmacro config (package &rest body)
  "Configure a package after it's loaded."
  `(with-eval-after-load 'package
     ,@body))

(install-many '(which-key
		evil
		evil-collection))

(with-eval-after-load 'which-key
  '(which-key-mode 1))

(config evil
	(setq evil-want-integration t
	      evil-want-keybinding nil)
	(evil-mode 1))

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
