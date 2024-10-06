(defun load-sub-module (module-name)
  "Load a submodule in the emacs config"
  (let*
      ((current-file (or load-file-name buffer-file-name))
    (current-path (file-name-directory current-file)))
  (load-file (expand-file-name module-name current-path))))

(load-sub-module "defaults.el")
(load-sub-module "editing.el")
(load-sub-module "magit.el")
