;; prevent start-screen from loading
(setq inhibit-startup-screen t)

;; hide toolbar and scroll bar on gui mode
(if
    (eql window-system 'ns)

    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)
      )
  )

;; select font and font-size
;; brew install --cask font-mononoki
(set-face-attribute 'default nil :family "Mononoki" :height 180)

;; set up zenburn dark theme
(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

;; enable enhanced navigation
(fido-mode 1)

;; take backup files to a different location
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")))
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; disable audible bells, enable visible bells
(setq ring-bell-function 'ignore)
(setq visible-bell t)

;; start emacs server to enable emacs-client
(server-start)
