;;; init.el --- Emacs configuration of Dan Plubell -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2015 Dan Plubell <danplubell@gmail.com>
;;
;; Author: Dan Plubell <danplubell@gmail.com>
;; URL: https://gihub.com/danplubell/.emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation; either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; GNU Emacs; see the file COPYING.  If not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301,
;; USA.

;;; Commentary:


;;package archives
(require 'package)
;;; Code:
(add-to-list 'package-archives
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
;;(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "http://stable.melpa.org/packages/"))
;;(package-initialize)

(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

;;setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

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
(package-safe-install 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-use-fuzzy t)

;;flycheck
;;(package-safe-install 'flycheck)
;;(add-hook 'after-init-hook #'global-flycheck-mode)

;;(package-safe-install 'flycheck-haskell)
;;(eval-after-load 'flycheck
;;  '(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))

(use-package flycheck                   ; On-the-fly syntax checking
  :ensure t
  :bind (("C-c l e" . list-flycheck-errors)
	 ("C-c T f" . flycheck-mode))
  :init (global-flycheck-mode)
  :config
  (progn
    (setq flycheck-completion-system 'ido)

    ;; Use italic face for checker name
    (set-face-attribute 'flycheck-error-list-checker-name nil :inherit 'italic))
  :diminish flycheck-mode)

(use-package flycheck-pos-tip           ; Show Flycheck messages in popups
  :ensure t
  :defer t
  :init
  (setq flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

(use-package flycheck-haskell           ; Setup Flycheck from Cabal projects
  :ensure t
  :defer t
  :init (add-hook 'flycheck-mode-hook #'flycheck-haskell-setup))


;;Line numbers
(global-linum-mode 1)

;;remove menu bars
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))
(setq inhibit-startup-screen t)

;;haskell
(package-safe-install 'haskell-mode)

;;(package-safe-install 'flymake-easy)
;;(package-safe-install 'flymake-haskell-multi)
;;(require 'flymake-haskell-multi) 
;;(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
;;(package-safe-install 'flymake-cursor)
;;(eval-after-load 'flymake '(require 'flymake-cursor))

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
