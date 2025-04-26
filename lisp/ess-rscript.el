;;; ess-rscript.el --- Run R scripts using Rscript -*- lexical-binding: t -*-

;; Author: Christophe Gouel <christophe.gouel@inrae.fr>
;; Version: 0.3
;; Package-Requires: ((emacs "24.3") (ess "18.10.2"))
;; Keywords: languages

;;; Commentary:

;; This package provides functions to run R scripts or evaluate R expressions
;; using Rscript.  It allows running the current buffer's R script or any
;; specified R script in a specified working directory, and supports passing
;; additional command-line arguments to Rscript.

;;; Code:

(require 'ess)

(defgroup ess-rscript nil
  "Run R scripts using Rscript."
  :group 'ess)

(defcustom ess-rscript-command "Rscript"
  "Progam name for calling Rscript."
  :type '(choice (string) file)
  :group 'ess-rscript)

;; Internal function that does the heavy lifting
(defun ess--run-rscript (script directory args expression)
  "Run Rscript on SCRIPT or evaluate EXPRESSION in DIRECTORY with ARGS.
SCRIPT is the path to the R script file.
EXPRESSION is an R expression to evaluate.
DIRECTORY is the working directory in which to run the script.
ARGS is a list of additional arguments to Rscript."
  (let* ((default-directory
          (or directory
              (when-let ((project (ess-r-project)))
                (cdr project)) ;; Use cdr to extract the project folder from the cons cell
              default-directory)) ; Fall back to `default-directory` if no directory or project is set
         (buffer-name (format "*Rscript-%s*"
                              (if script
                                  (file-name-base script)
                                "expression")))
         (compilation-buffer-name-function
          (lambda (_mode) buffer-name))
         (compilation-ask-about-save nil)
         (cmd (concat (shell-quote-argument ess-rscript-command)
                      " "
                      (when args
                        (concat (mapconcat #'shell-quote-argument args " ") " "))
                      (if expression
                          (concat "-e " (shell-quote-argument expression))
                        (concat " " (shell-quote-argument (expand-file-name script))))))
         (compilation-buffer (compile cmd)))
    (when (buffer-live-p compilation-buffer)
      (with-current-buffer compilation-buffer
        (rename-buffer buffer-name t)))))

;;;###autoload
(defun ess-rscript (&optional arg script directory args expression)
  "Run an R script or expression using Rscript.

This function executes the R script specified by SCRIPT in the directory
specified by DIRECTORY, with additional arguments ARGS.

Alternatively, if EXPRESSION is provided, it evaluates the R expression using
`Rscript -e`.

If called interactively with a universal prefix argument ARG, or if the
current buffer is not an R buffer, or if SCRIPT is nil, the function will
prompt the user to specify the script file or expression, the working
directory, and additional arguments.

When called from Lisp code, SCRIPT, DIRECTORY, ARGS, and EXPRESSION can be
provided to specify the script file or expression, the working directory, and
additional arguments.  If SCRIPT or EXPRESSION is provided when called from
Lisp code, it will not prompt for input."
  (interactive
   (let (script directory args expression)
     (if (or current-prefix-arg
             (not (derived-mode-p 'ess-r-mode))
          (not (buffer-file-name)))
         (progn
           ;; Prompt for script or expression
           (let ((choice (completing-read "Run (s)cript or (e)xpression? "
                                          '("script" "expression") nil t nil nil "script")))
             (if (string-equal choice "expression")
                 (setq expression (read-string "R expression: "))
               (setq script (read-file-name "R script: "))))
           ;; Use ess-r-project if available, fall back to default-directory
           (setq directory (read-directory-name
                            "Working directory: "
                            (or (when-let ((project (ess-r-project)))
                                  (cdr project)) ;; Extract the project folder
                                default-directory) ;; Fall back if no project detected
                            nil t))
           (let ((args-string (read-string "Additional arguments: ")))
         (unless (string-empty-p args-string)
               (setq args (split-string-and-unquote args-string)))))
       ;; Default behavior: use current buffer's script
       (setq script (buffer-file-name))
       (setq directory (or (when-let ((project (ess-r-project)))
                             (cdr project))
                           default-directory)) ;; Falls backto default-directory
       (when (buffer-modified-p)
         (save-buffer)))
     (list nil script directory args expression)))
  ;; Run the script or expression
  (ess--run-rscript script directory args expression))

(provide 'ess-rscript)

;;; ess-rscript.el ends here
