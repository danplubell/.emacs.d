;;; custom.el  --- Summary
;;; Commentary:
;;; Code:
(defadvice load-theme
    (before theme-dont-propagate activate)
  (mapc #'disable-theme custom-enabled-themes))

;;(req-package moe-theme)

;;(req-package moe-theme-switcher
;;    :require moe-theme)
(req-package dash-at-point
  :config
  (global-set-key "\C-cd" 'dash-at-point)
  (global-set-key "\C-ce" 'dash-at-point-with-docset)
  (add-to-list 'dash-at-point-alist '(haskell-mode . "haskell"))
  (add-to-list 'dash-at-point-alist '(haskell-mode . "hackage"))
  )

(req-package exe-path-from-shell
  :config
  (when (memq window-system 'mac ns))
    (exec-path-from-shell-initialize))

(req-package noctilux-theme)
;;(req-package zenburn-theme)

(req-package scroll-bar
  :config
  (scroll-bar-mode -1))

(req-package tool-bar
  :config
  (tool-bar-mode -1))

(req-package menu-bar
  :config
  (menu-bar-mode -1))   

(req-package linum
  :config
  (add-hook 'prog-mode-hook
	    '(lambda () (linum-mode 1))))

;;;(global-linum-mode 1)

(req-package diminish)

(req-package server
             :diminish (server-buffer-clients . ""))

(req-package smartparens-config
  :ensure smartparens
  :diminish (smartparens-mode . "()")
  :init (smartparens-global-mode t))

(req-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(req-package powerline)

(req-package auto-complete-config
  :ensure auto-complete
  :init
  (progn
    (ac-config-default)
    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
    (setq ac-comphist-file "~/.emacs.d/ac-comphist.dat")
    (setq ac-auto-start 3)
    (setq ac-use-fuzzy t))
  :config
  (progn
    (require 'ac-math)
    (require 'auto-complete-auctex)))

(req-package flycheck
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
  :diminish flycheck-mode)

(req-package flycheck-pos-tip
  :require flycheck
  :config
;;  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
;;  (setq flycheck-display-errors-function #'flycheck-display-error-messages-unless-error-list))
  (if (memq window-system '(mac ns))
      (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages)
    (setq flycheck-display-errors-function #'flycheck-display-error-messages))
 )



(req-package helm-flycheck
  :require flycheck
  :commands helm-flycheck
  :config
  (bind-key "C-c ! h"
	    'helm-flycheck
	    flycheck-mode-map))

(req-package haskell-mode
  :require (flycheck flycheck-haskell)
  :commands haskell-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.l?hs$" . haskell-mode))
  (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
  :config
  (progn
    (req-package inf-haskell)
;;    (req-package hs-lint)
    (bind-key "C-x C-d" nil haskell-mode-map)
    (bind-key "C-c C-z" 'haskell-interactive-switch haskell-mode-map)
    (bind-key "C-c C-l" 'haskell-process-load-file haskell-mode-map)
    (bind-key "C-c C-b" 'haskell-interactive-switch haskell-mode-map)
    (bind-key "C-c C-t" 'haskell-process-do-type haskell-mode-map)
    (bind-key "C-c C-i" 'haskell-process-do-info haskell-mode-map)
    (bind-key "C-c M-." nil haskell-mode-map)
    (bind-key "C-c C-d" nil haskell-mode-map)
    (defun my-haskell-hook ()
      (setq mode-name " λ ")
      (turn-on-haskell-doc)
      (diminish 'haskell-doc-mode "")
;;      (capitalized-words-mode)
;;      (diminish 'capitalized-words-mode "")
      (turn-on-eldoc-mode)
      (diminish 'eldoc-mode "")
      (turn-on-haskell-decl-scan)
      (setq evil-auto-indent nil))
    (setq haskell-font-lock-symbols 'unicode)      
    (setq haskell-literate-default 'tex)
    (setq haskell-stylish-on-save t)
    (setq haskell-tags-on-save t)
    (setq haskell-process-log t)
    (setq haskell-process-load-or-reload-prompt t)
    (setq haskell-interactive-mode-hide-multi-line-errors nil)
    (setq haskell-process-type (quote auto))
    ;;    (setq haskell-process-type (quote cabal-repl))
    (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
    (add-hook 'haskell-mode-hook 'my-haskell-hook)
    (defun hc-ac-haskell-candidates (prefix)
      (let ((cs (haskell-process-get-repl-completions (haskell-process)prefix)))
	(remove-if (lambda (c) (string= "" c)) cs)))
    (ac-define-source haskell
      '((candiates . (hc-ac-haskell-candidates ac-prefix))))
    (defun hc-haskell-hook()
      (add-to-list 'ac-sources 'ac-source-haskell))
    (add-hook 'haskell-mode-hook 'hc-haskell-hook)))
		 
(req-package flycheck-haskell
    :config (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(req-package shm
  :require haskell-mode
  :commands structured-haskell-mode
  :init (add-hook 'haskell-mode-hook
		  'structured-haskell-mode))
;;  :config
;;  (eval-after-load 'shm
;;   '(progn
;;     (set-face-background 'shm-current-face "#eee8d5")
;;     (set-face-background 'shm-quarantine-face "lemonchiffon")))

;;(req-package ghc
;;  :init (add-hook 'haskell-mode-hook (lambda () (ghc-init))))

(req-package lisp-mode
  :init
  (add-hook 'emacs-lisp-mode-hook
	    (lambda ()
	      (setq mode-name " ξ "))))
(req-package files
  :init
  (progn
    (setq backup-directory-alist
	  `((".*" . ,temporary-file-directory)))
    (setq auto-save-file-name-transforms
	  `((".*" ,temporary-file-directory t)))))

(req-package ac-haskell-process
  :require (auto-complete haskell-mode)
  :config
  (progn
    (add-hook 'interactive-haskell-mode 'ac-haskell-process-setup)
    (add-hook 'haskell-interactive-mode-hook 'ac-haskell-process-setup)
    (eval-after-load "auto-complete"
      '(add-to-list 'ac-modes 'haskell-interactive-mode))
    )
)
(req-package stack-mode)

(req-package hindent)

(req-package neotree
  :config
  (progn (global-set-key [f8] 'neotree-toggle))
)

(fset 'display-startup-echo-area-message #'ignore)
(fset 'yes-or-no-p #'y-or-n-p)

(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)

;;(setq initial-scratch-message
;;      (concat "\nHello Dan. Today is: "
;;	          (format-time-string "%A %d %B %Y at %T\n")))
(req-package-finish)

;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hindent-style "chris-done")
 '(package-selected-packages
   (quote
    (neotree ac-haskell-process flycheck-haskell helm-flycheck powerline rainbow-delimiters noctilux-theme yasnippet web-mode stack-mode smex smartparens shm req-package projectile prodigy popwin pallet nyan-mode multiple-cursors moe-theme magit idle-highlight-mode htmlize hindent flycheck-pos-tip flycheck-cask expand-region exec-path-from-shell drag-stuff company-ghc))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
