;;package archives
(require 'package)
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

;;turn off backup files and autosave
(setq make-backup-files nil)
(setq auto-save-default nil)

;;functions
(defun package-safe-install (&rest packages)
  (dolist (package packages)
    (unless (package-installed-p package)
      (package-install package))
    (require package)))


;;autocomplete
;;(package-safe-install 'auto-complete)
(add-to-list 'load-path "~/.emacs.d/lisp")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/lisp/ac-dict")
(ac-config-default)
(setq ac-use-fuzzy t)

;;flycheck
;;(package-safe-install 'flycheck)
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;(package-safe-install 'flycheck-haskell)
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;Line numbers
(global-linum-mode 1)

;;remove menu bars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)

;;haskell
(package-safe-install 'haskell-mode)

(package-safe-install 'flymake-easy)
(package-safe-install 'flymake-haskell-multi)
(require 'flymake-haskell-multi) 
(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
(package-safe-install 'flymake-cursor)
(eval-after-load 'flymake '(require 'flymake-cursor))

;;turn off ghc-mod until ghc 7.10 is released because 7.8.3 is not compatible with cabal 1.22
;;(autoload 'ghc-init "ghc" nil t)
;;(autoload 'ghc-debug "ghc" nil t)
;;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)

;;autocomplete source for haskell
(auto-complete-mode)
(defun hc-ac-haskell-candidates (prefix)
  (let ((cs (haskell-process-get-repl-completions (haskell-process) prefix)))
    (remove-if (lambda (c) (string= "" c)) cs)))
(ac-define-source haskell
  '((candidates . (hc-ac-haskell-candidates ac-prefix))))
(defun hc-haskell-hook ()
  (add-to-list 'ac-sources 'ac-source-haskell))
  (add-hook 'haskell-mode-hook 'hc-haskell-hook)

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-ode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(custom-set-variables
 '(haskell-interactive-mode-hide-multi-line-errors nil)
 '(haskell-process-log t)
 '(haskell-process-type (quote cabal-repl))
 )
