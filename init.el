(require 'cask "~/.cask/cask.el")
(cask-initialize)

(require 'req-package)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
