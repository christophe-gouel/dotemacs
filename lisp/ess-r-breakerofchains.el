;;; ess-r-breakerofchains.el --- Break R magrittr / base‑pipe chains and evaluate -*- lexical-binding: t; -*-

;; Author: Christophe Gouel <christophe.gouel@inrae.fr>
;; Version: 0.1
;; Keywords: languages, R, ess
;; Package-Requires: ((emacs "27.1") (ess "18.10.2"))

;;; Commentary:

;; The command `ess-r-breakerofchains-run-to-point` imitates the behaviour of the
;; RStudio add‑in “Break chain and run to cursor” supplied by the
;; {breakerofchains} package.  With point anywhere inside a multi‑line
;; pipe chain (magrittr %>%, base |>, or any custom %op%), invoke the
;; command to:
;;
;;   1. Locate the chain’s start (moving upward until a line *not*
;;      continuing the chain is found),
;;   2. Trim any trailing pipe at the cursor line so the expression is
;;      syntactically complete,
;;   3. Strip an initial assignment (`x <-` or `x =`) so the fragment
;;      is executed *for its value*,
;;   4. Evaluate the fragment through ESS, print the result, and,
;;      unless disabled, copy it to `.chain` in the R global
;;      environment for later inspection.
;;
;; The implementation is 100 % Emacs Lisp; no R helper code is used.

;;; Code:

(require 'ess-inf)
(require 'ess-r-mode)
(require 'subr-x)

(defgroup ess-r-breakerofchains nil
  "Break R chains and evaluate through ESS."
  :group 'ess
  :prefix "ess-r-breakerofchains-")

(defcustom ess-r-breakerofchains-store-result t
  "If non‑nil, assign the value just evaluated to a global `.chain` in R."
  :type 'boolean
  :group 'ess-r-breakerofchains)

(defcustom ess-r-breakerofchains-debug nil
  "When non‑nil, echo extra information about the region that will be run."
  :type 'boolean
  :group 'ess-r-breakerofchains)

(defcustom ess-r-breakerofchains-print-code nil
  "When non‑nil, print the R code that is run."
  :type 'boolean
  :group 'ess-r-breakerofchains)

;;;; Helper functions

(defun ess-r-breakerofchains--trim-right (s)
  "Trim whitespace from the right of string S."
  (replace-regexp-in-string "[ \t\n\r]+\\'" "" s))

(defun ess-r-breakerofchains--pipe-regexp ()
  "Return a regexp that matches common R pipe / infix operators at EOL."
  "\\(%[^%[:space:]]+%\\|%>%\\|[|]>\\|\\+\\)\\s-*\\(?:#.*\\)?$")

(defun ess-r-breakerofchains--line-continues-p ()
  "Return non‑nil if the current line *continues* a chain.
This is the case when the logical end of code on the line is a pipe/infix
operator, *or* the line ends within an open bracket context."
  (let* ((eol (line-end-position))
         (syntax (syntax-ppss eol)))
    (or (save-excursion
          (goto-char eol)
          (re-search-backward (ess-r-breakerofchains--pipe-regexp)
                              (line-beginning-position) t))
        (> (nth 0 syntax) 0))))

(defun ess-r-breakerofchains--find-chain-start ()
  "Return buffer position (point) of the first line of the chain."
  (save-excursion
    (let ((continue t))
      (while (and (not (bobp)) continue)
        (forward-line -1)
        (setq continue (ess-r-breakerofchains--line-continues-p)))
      ;; If we broke because current line does *not* continue, move one down.
      (unless (ess-r-breakerofchains--line-continues-p)
        (forward-line +1))
      (line-beginning-position))))

(defun ess-r-breakerofchains--strip-leading-assignment (code)
  "Remove an initial \"name <-\" or \"name =\" from CODE."
  (let ((re "^\\s-*\\([.A-Za-z][.A-Za-z0-9_]*\\)\\s-*\\(<-\\|=\\)\\s-*"))
    (if (string-match re code)
        (replace-match "" nil nil code)
      code)))

(defun ess-r-breakerofchains--strip-trailing-pipe (code)
  "Remove a trailing pipe/infix operator from CODE *on the last line only*."
  (replace-regexp-in-string
   ;;              ┌─ common pipes ─┐  optional spaces & comment  end‑of‑string
   "\\(%[^%[:space:]]+%\\|%>%\\|[|]>\\|\\+\\)\\s-*\\(?:#.*\\)?\\'"
   ""
   code))

;;;; Main interactive command

;;;###autoload
(defun ess-r-breakerofchains-run-to-point ()
  "Break the infix/pipe chain at point and evaluate up to that point."
  (interactive)
  (unless (derived-mode-p 'ess-r-mode)
    (user-error "Current buffer is not an ESS R buffer"))
  (save-excursion
    (let* ((chain-start (ess-r-breakerofchains--find-chain-start))
           (chain-end   (line-end-position))
           (raw-code    (buffer-substring-no-properties chain-start chain-end))
           (code        (ess-r-breakerofchains--strip-leading-assignment
                         (ess-r-breakerofchains--strip-trailing-pipe
                          (ess-r-breakerofchains--trim-right raw-code)))))
      (when ess-r-breakerofchains-debug
        (message "[ess-r-breakerofchains] region %d..%d: %s"
                 chain-start chain-end (string-replace "\n" "\\n" code)))
      (ess-force-buffer-current) ; ensure ESS knows which process
      ;; Evaluate and print
      (ess-send-string (ess-get-process) code ess-r-breakerofchains-print-code)
      ;; Optionally assign to .chain
      (when ess-r-breakerofchains-store-result
        (ess-send-string
         (ess-get-process)
         "assign('.chain', .Last.value, envir = .GlobalEnv)" nil)))))

(provide 'ess-r-breakerofchains)

;;; ess-r-breakerofchains.el ends here
