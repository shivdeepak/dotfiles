(defun get-abs-path (file-name)
  (let* ((current-file (or load-file-name buffer-file-name))
         (current-path (file-name-directory current-file)))
    (expand-file-name file-name current-path)))

(defun load-sub-module (module-name)
  "Load a submodule in the emacs config"
  (load-file (get-abs-path module-name)))

(load-sub-module "defaults.el")
(load-sub-module "editing.el")
(load-sub-module "git.el")

(setq treesit-language-source-alist '())
(load-sub-module "python.el")
(load-sub-module "rust.el")
