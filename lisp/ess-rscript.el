;;; ess-rscript.el --- Run R scripts using Rscript -*- lexical-binding: t -*-

;; Author: Christophe Gouel <christophe.gouel@inrae.fr>
;; Version: 0.1
;; Package-Requires: ((emacs "24.3") (ess "18.10.2"))
;; Keywords: languages

;;; Commentary:

;; This package provides a function to run R scripts using Rscript.
;; It allows running the current buffer's R script or any specified R script
;; in a specified working directory.

;;; Code:

(require 'ess)

(defgroup ess-rscript nil
  "Run R scripts using Rscript."
  :group 'ess)

(defcustom rscript-command (executable-find "Rscript")
  "Command to call Rscript."
  :type 'string
  :group 'ess-rscript)

;; Internal function that does the heavy lifting
(defun ess--run-rscript (script &optional directory)
  "Run Rscript on SCRIPT in DIRECTORY.
SCRIPT is the path to the R script file.
DIRECTORY is the working directory in which to run the script."
  (let* ((default-directory (or directory default-directory))
         (buffer-name (format "*Rscript-%s*" (file-name-base script)))
         (compilation-buffer-name-function
          (lambda (_mode) buffer-name))
         (compilation-ask-about-save nil))
    (when (get-buffer buffer-name)
      (kill-buffer buffer-name))
    (compile (format "%s %s" (shell-quote-argument rscript-command)
                     (shell-quote-argument (expand-file-name script))))
    (with-current-buffer buffer-name
      (rename-buffer buffer-name))))

;;;###autoload
(defun ess-rscript (&optional arg)
  "Run an R script using Rscript.
This function executes the current R script in the buffer using the
Rscript command.  If a universal prefix argument ARG is provided, or if
the current buffer is not an R buffer, the function will prompt the user
to specify the script file and the working directory."
  (interactive "P")
  (let ((script (buffer-file-name))
        (directory default-directory))
    (if (or arg (not (derived-mode-p 'ess-r-mode)) (not script))
        (progn
          (setq script (read-file-name "R script: "))
          (setq directory (read-directory-name "Working directory: ")))
      (when (buffer-modified-p)
        (save-buffer)))
    (ess--run-rscript script directory)))

(provide 'ess-rscript)

;;; ess-rscript.el ends here
