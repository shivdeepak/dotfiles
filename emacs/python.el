(defun ensure-venv ()
  "Check if Python 3 is installed and use it to create a virtual environment.
If the 'venv' directory already exists, skip the creation."
  (interactive)
  (if (file-directory-p "venv")
      (message "Virtual environment already exists.")
    (let* ((python-executable
            (or (executable-find "python3") (executable-find "python")))
           (python-version
            (if python-executable
                (string-trim
                 (shell-command-to-string (concat python-executable " --version"))))))
      (if (and python-executable
               (string-match "^Python 3\." python-version))
          (progn
            (message "Using %s to create virtual environment..." python-executable)
            (shell-command (concat python-executable (concat " -m venv " (get-abs-path "venv"))))
            (message "Virtual environment created successfully."))
        (message "Python 3 is not installed or not available in the system path.")))))

(defun ensure-pip-package-installed (package)
  "Check if a given pip PACKAGE is installed in the venv, if not, install it using the venv's pip."
  (interactive "Enter pip package name: ")
  (let* ((venv-pip (get-abs-path "venv/bin/pip"))
         (python-executable (or (executable-find "python3") (executable-find "python")))
         (check-command (concat venv-pip " show " package))
         (install-command (concat venv-pip " install " package))
         (venv-exists (file-exists-p venv-pip))
         (package-installed (and venv-exists
                                 (string-match-p "Name: " (shell-command-to-string check-command)))))
    (if (not venv-exists)
        (message "Virtual environment 'venv' does not exist. Please create it first.")
      (if package-installed
          (message "Pip package '%s' is already installed in the virtual environment." package)
        (progn
          (message "Installing pip package '%s' in the virtual environment..." package)
          (shell-command install-command)
          (message "Pip package '%s' installed successfully in the virtual environment." package))))))

(ensure-venv)
(ensure-pip-package-installed "ruff")

(setq ruff-exec-path (get-abs-path "venv/bin/ruff"))

(defun run-ruff-on-buffer ()
  "Run `ruff check --fix` on the current buffer."
  (interactive)
  (when (eq major-mode 'python-mode)
    (shell-command (format (concat ruff-exec-path " check --fix %s") (shell-quote-argument (buffer-file-name))))
    (revert-buffer t t t)))

(defun eglot-format-and-ruff ()
  "Run eglot-format and then `ruff check --fix`."
  (interactive)
  (eglot-format)
  (run-ruff-on-buffer))

(use-package eglot
  :ensure t
  :hook (python-mode . eglot-ensure)
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs
		 `(python-mode . (,ruff-exec-path "server")))
    (add-hook 'after-save-hook 'eglot-format-and-ruff)))
