
;; To install manually:
;; - LSP servers
;; ```{bash}
;;   pip3 install --user python-lsp-server[all] jupyter[all]
;;   Rscript -e "install.packages('languageserver')"
;;   Curl --output %HOME%/.local/bin/digestif.cmd https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif.cmd
;; ```
;; - Languagetool: to download from <https://languagetool.org/download/>
;; - Autocompletion in Python on Windows.
;; ```{bash}
;;   pip3 install --user pyreadline3
;; ```
;; - M-x jedi:install-server
;; - Download and install fonts
;;   - JetBrains from <https://www.nerdfonts.com/font-downloads>
;;   - <https://github.com/aliftype/xits>
;; - fd command for a fast find: <https://github.com/sharkdp/fd>

(defconst is-mswindows (equal window-system 'w32)
  "Boolean indicating whether Emacs is excuted within MS Windows.")

;; Define a file in which any customization is saved
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; ==================
;;;  Packages manager
;;; ==================
;; (require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package setup
;; Bootstrap use-package for emacs version below 29
(when (< emacs-major-version 29)
    (unless (package-installed-p 'use-package)
      (package-refresh-contents) ; update archives
      (package-install 'use-package)) ; grab the newest use-package
    (require 'use-package)
    )

(use-package use-package
  :ensure nil
  :custom
  ;; Always download packages if not available
  (use-package-always-ensure t)
  )

;; To keep GPG keys up to date
(use-package gnu-elpa-keyring-update)

;; Quelpa
(use-package quelpa
  :custom
  (quelpa-update-melpa-p nil) ; Prevent update at all startup
  )
(use-package quelpa-use-package)

;;; =======
;;;  Shell
;;; =======
(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

(if is-mswindows    ;; MS Windows clipboard is UTF-16LE
    (defun bash ()
      (interactive)
      (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe" ))
	(shell "*bash*"))
      ))
(setq explicit-bash.exe-args '("--login" "-i"))

(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;;; ===============
;;;  Others
;;; ===============
;;; Activate lower- and upper-case commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(if (display-graphic-p)
    (server-start)
  (xterm-mouse-mode))

;;; ============
;;;  nerd-icons
;;; ============
(use-package nerd-icons
  :if (display-graphic-p)
  :custom
  (nerd-icons-font-family "JetBrainsMonoNL NF")
  )
(use-package nerd-icons-dired
  :if (display-graphic-p)
  :hook
  (dired-mode . nerd-icons-dired-mode)
  )
(use-package nerd-icons-ivy-rich
  :if (display-graphic-p)
  :after counsel
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1)
  )
(use-package nerd-icons-ibuffer
  :if (display-graphic-p)
  :hook
  (ibuffer-mode . nerd-icons-ibuffer-mode)
  )
(use-package nerd-icons-completion
  :if (display-graphic-p)
  :config
  (nerd-icons-completion-mode)
  )

;;; ======================
;;;  Dashboard
;;; ======================
(use-package dashboard
  :if (display-graphic-p)
  :config
  ;; On active la prise en charge des projets avec projectile
  (setq dashboard-projects-backend 'projectile
	;; On ajoute les raccourcis de rubrique
	dashboard-set-navigator t
	;; On centre le contenu
	dashboard-center-content t
	;; On configure ce qu'on veut voir apparaître
	dashboard-items '((recents  . 5)
                          (projects . 5)
                          (bookmarks . 5))
	;; On met des icônes
	dashboard-set-heading-icons t
	dashboard-set-file-icons t
	;; On vire le footer (je ne le lis pas)
	dashboard-set-footer nil)
  ;; On démarre dashboard par défaut
  (dashboard-setup-startup-hook)
  )

;;; ========
;;;  proced
;;; ========
(use-package proced
  :ensure nil
  :custom
  (proced-enable-color-flag t)
)

;;; ===========
;;;  which-key
;;; ===========
(use-package which-key
  :diminish which-key-mode
  :init
  (setq which-key-sort-uppercase-first nil
		max-mini-window-height 15)
  ;; On va utiliser une fenêtre dédiée plutôt que le minibuffer
  (which-key-setup-side-window-bottom)
  ;; On l'active partout, tout le temps
  (which-key-mode t)
  )

;;; =======
;;;  dired
;;; =======
(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom
  (dired-listing-switches "-agho --group-directories-first")
  (auto-revert-verbose nil)
  :hook
  (dired-mode . (lambda ()
		  (dired-hide-details-mode)))
  (dired-mode . auto-revert-mode)
  )

(use-package diredfl
  :hook
  (dired-mode . diredfl-mode)
  )

;;; =========
;;;  flymake
;;; =========
(use-package flymake
  :ensure nil
  :custom
  (flymake-no-changes-timeout nil)
  :config
  (setq ess-use-flymake nil) ; Deactivate linter in ess because it does not seem to work well
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  :bind
  ("M-n" . flymake-goto-next-error)
  ("M-p" . flymake-goto-prev-error)
  )

;;; =======
;;;  imenu
;;; =======
(use-package imenu
  :ensure nil
  :custom
  (imenu-auto-rescan t)
  )

(use-package imenu-list
  :bind
  (("C-c =" . imenu-list-smart-toggle)
   :map imenu-list-major-mode-map
	 ("C-<return>" . cg/imenu-list-goto-entry))
  :custom
  (imenu-list-focus-after-activation t)
  (imenu-list-position 'right)
  :config
  (defun cg/imenu-list-goto-entry ()
    (interactive)
    (imenu-list-goto-entry)
    (imenu-list-smart-toggle))
  )

;;; ======================
;;;  greek-unicode-insert
;;; ======================
(use-package greek-unicode-insert
  :quelpa (greek-unicode-insert
	   :fetcher github
	   :repo "Malabarba/greek-unicode-insert")
  :bind ("²" . greek-unicode-insert-map))

;;; ===============================
;;;  font-lock for hex color codes
;;; ===============================
(use-package rainbow-mode
  :quelpa (rainbow-mode
	   :fetcher url
	   :url "https://raw.githubusercontent.com/emacsmirror/rainbow-mode/master/rainbow-mode.el")
  :hook (prog-mode . rainbow-mode))

;;; ==================================
;;;  ado-mode for editing Stata files
;;; ==================================
(use-package ado-mode)

;;; =============
;;;  Python mode
;;; =============
(use-package python
  :ensure nil
  :config
  (setq python-shell-interpreter "ipython3"
	python-shell-interpreter-args "-i --simple-prompt --InteractiveShell.display_page=True"
	python-shell-prompt-detect-failure-warning nil)
;; Set encoding to utf-8 to allows utf-8 characters in Python REPL (from https://stackoverflow.com/questions/14172576/why-unicodeencodeerror-raised-only-in-emacss-python-shell?utm_source=pocket_reader)
  (setenv "PYTHONIOENCODING" "utf-8")
  (defun my/python-mode-hook ()
    (add-to-list 'company-backends 'company-jedi))
  :hook
  (python-mode . my/python-mode-hook)
  (python-mode . flymake-mode)
  )

(use-package conda
  :if is-mswindows
  :config
  (setq-default mode-line-format
		(cons '(:exec conda-env-current-name) mode-line-format))
  )

(use-package poetry)

(use-package pyvenv
  :custom
  (pyvenv-virtualenvwrapper-supported "ipython3")
  :config
  (if is-mswindows
      ;; Default virtualenv cache directory for poetry on Microsoft Windows
      (setenv "WORKON_HOME"
	      (substitute-in-file-name "${LOCALAPPDATA}/pypoetry/Cache/virtualenvs"))
    ;; Default virtualenv cache directory for poetry on *nix
    (setenv "WORKON_HOME" "~/.cache/pypoetry/virtualenvs"))
  )

(use-package pydoc)

(use-package numpydoc
  :bind (:map python-mode-map
              ("C-c C-n" . numpydoc-generate)))

;;; =========
;;;  chatGPT
;;; =========
(use-package chatgpt-shell
  :custom
  (chatgpt-shell-openai-key
      (auth-source-pick-first-password :host "api.openai.com"))
  )

(use-package gptel
  :custom
  (gptel-use-curl nil)
  )

;;; ==================
;;;  GitHub - Copilot
;;; ==================
;; Configuration from <https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/>
(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "zerolfx/copilot.el"
                   :branch "main"
                   :files ("dist" "*.el"))
  :custom
  (copilot-indent-warning-suppress t)
  :config
  (defun cg/copilot-complete-or-accept ()
    "Command that either triggers a completion or accepts one if one is available."
    (interactive)
    ;; Check if the Copilot overlay is visible
    (if (copilot--overlay-visible)
	(progn
	  ;; Accept the completion
          (copilot-accept-completion)
          ;; ;; Open a new line
          ;; (open-line 1)
          ;; ;; Move to the next line
          ;; (next-line)
	  )
      ;; If the Copilot overlay is not visible, trigger completion
      (copilot-complete)))
  
  (defvar cg/copilot-manual-mode nil
    "When `t' will only show completions when manually triggered, e.g. via M-C-<return>.")

  (defun cg/copilot-disable-predicate ()
    "When copilot should not automatically show completions."
    cg/copilot-manual-mode)

  (defun cg/copilot-change-activation ()
    "Switch between three activation modes:
       - automatic: copilot will automatically overlay completions
       - manual: you need to press a key (M-C-<return>) to trigger completions
       - off: copilot is completely disabled."
    (interactive)
    (if (and copilot-mode cg/copilot-manual-mode)
	(progn
          (message "deactivating copilot")
          (copilot-mode -1)
          (setq cg/copilot-manual-mode nil))
      (if copilot-mode
          (progn
            (message "activating copilot manual mode")
            (setq cg/copilot-manual-mode t))
	(message "activating copilot mode")
	(copilot-mode))))

  (add-to-list 'copilot-disable-predicates #'cg/copilot-disable-predicate)
  :hook (prog-mode . (lambda() (setq cg/copilot-manual-mode t)))
  :bind
  (
   ("C-M-c"         . cg/copilot-change-activation)
   :map copilot-mode-map
   (("M-C-<next>"   . copilot-next-completion)
    ("M-C-<prior>"  . copilot-previous-completion)
    ("M-C-<right>"  . copilot-accept-completion-by-word)
    ("M-C-<down>"   . copilot-accept-completion-by-line)
    ("M-C-<return>" . cg/copilot-complete-or-accept)
    ("M-C-g"        . copilot-clear-overlay))
   )
  )

;;; ==========
;;;  Org mode
;;; ==========
(use-package org
  :ensure nil
  :mode ("\\.org\\'" . org-mode)
  :custom
  (org-hide-leading-stars t)
  (org-export-with-LaTeX-fragments t)       ; Export LaTeX fragment to HTML
  (org-todo-keywords '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)")))
  (org-tag-alist '(("OFFICE" . ?o) ("COMPUTER" . ?c) ("HOME" . ?h) ("PROJECT" . ?p) ("CALL" . ?a) ("ERRANDS" . ?e) ("TASK" . ?t)))
  (org-hide-leading-stars t)
  :config
  ;; Integration of RefTeX in org
  (defun org-mode-reftex-setup ()
    (load-library "reftex")
    (and (buffer-file-name)
	 (file-exists-p (buffer-file-name))
         (global-auto-revert-mode t)
	 (reftex-parse-all))
    (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
    )
  :hook (org-mode . org-mode-reftex-setup)
)

;;; ========================================
;;;  Look, feel, and general emacs behavior
;;; ========================================
(use-package emacs
  :custom
  (pixel-scroll-precision-mode t)
  (blink-cursor-blinks 0)  ; curseur clignote indéfiniment
  (cursor-type 'bar)  ; curseur étroit
  (show-paren-mode t)  ; coupler les parenthèses
  (auth-sources '("~/.authinfo"))  ; Define file that stores secrets
  (backup-directory-alist '(("." . "~/.emacs.d/backup")))
  (display-time-24hr-format t)  ; Affichage de l'heure format 24h
  (default-major-mode 'text-mode)  ; mode par défaut
  ;; Options to make lsp usable in emacs (from <https://emacs-lsp.github.io/lsp-mode/page/performance/>)
  (gc-cons-threshold (* 10 800000))
  (read-process-output-max (* 1024 1024))
  (prettify-symbols-unprettify-at-point t)
  (column-number-mode t)              ; affichage du numéro de la colonne
  :config
  (set-face-background 'cursor "#CC0000")  ; curseur rouge foncé
  (tool-bar-mode 0)
  (when (display-graphic-p)
    (global-hl-line-mode +1)  ; Highlight the current line
    ;; Fonts and unicode characters
    (add-to-list 'default-frame-alist
		 '(font . "JetBrainsMonoNL NF-10"))
    (set-fontset-font t 'unicode (font-spec :name "XITS Math") nil 'prepend)
    ;; To list all available fonts, use
    ;; (dolist (font (x-list-fonts "*"))
    ;;   (insert (format "%s\n" font)))
    (add-hook 'text-mode-hook 'prettify-symbols-mode)
    )
  :bind(("C-x C-b" . ibuffer)
	("<f5>" . revert-buffer)
	(:map compilation-mode-map
	      ("r" . recompile)))
  :hook
  (prog-mode . display-fill-column-indicator-mode)
  )
(setq jit-lock-chunk-size 50000)

(setq large-file-warning-threshold 100000000) ; set large file threshold at 100 megabytes

(delete-selection-mode t)                ; entrée efface texte sélectionné
(setq-default mouse-yank-at-point t)     ; coller avec la souris
(setq-default case-fold-search t)        ; recherche sans égard à la casse

(fset 'yes-or-no-p 'y-or-n-p)            ; Replace yes or no with y or n


(setq comment-column 0) ; Prevent indentation of lines starting with one comment

;;; set up unicode
(set-language-environment "UTF-8")
(prefer-coding-system       'utf-8)
;; (setq locale-coding-system 'utf-8) ; Mess up dired buffer under windows
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8-unix)
(setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))
(if is-mswindows    ;; MS Windows clipboard is UTF-16LE
    (set-clipboard-coding-system 'utf-16le-dos))

;;; Sent deleted files to trash
(setq delete-by-moving-to-trash t)

;;; ====================
;;;  rainbow-delimiters
;;; ====================
(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'yaml-mode-hook 'rainbow-delimiters-mode))
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
  (rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
  (rainbow-delimiters-depth-3-face ((t (:foreground "cyan"))))
  (rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
  (rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
  (rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
  (rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
  (rainbow-delimiters-depth-8-face ((t (:foreground "black"))))
  (rainbow-delimiters-unmatched-face ((t (:background "yellow")))))

;;; =============
;;;  smartparens
;;; =============
(use-package smartparens-config
  :ensure smartparens
  :init
  (progn
    (add-hook 'prog-mode-hook 'smartparens-mode)
    (add-hook 'markdown-mode-hook 'smartparens-mode)
    (add-hook 'yaml-mode-hook 'smartparens-mode))
  :config (progn (show-smartparens-global-mode t)))

;;; =============
;;;  fill-Unfill
;;; =============
(defun unfill-paragraph ()
  "Unfill paragraph."
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(defun unfill-region (start end)
  "Unfill region."
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

(setq-default fill-column 80)

;; Package to visually (not really) indent the filled lines following the first
;; lines
(use-package adaptive-wrap)

;; Use visual fill column for text mode
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 100)
  :config
  (defun my-visual-fill ()
    "Toggle visual fill column and visual line mode."
    (interactive)
    (visual-line-mode 'toggle)
    (visual-fill-column-mode 'toggle)
    (adaptive-wrap-prefix-mode 'toggle))
  
  (defun center-text ()
    "Center text in visual fill column."
    (interactive)
    (setq-local visual-fill-column-center-text t))
  
  (defun uncenter-text ()
    "Uncenter text in visual fill column."
    (interactive)
    (setq-local visual-fill-column-center-text nil))
  :bind ("C-c v" . my-visual-fill)
  :hook
  (TeX-mode    . my-visual-fill)
  (bibtex-mode . my-visual-fill)
  )

;;; =======
;;;  Pager
;;; =======
(use-package pager
  :bind
  (("\C-v" . pager-page-down)
   ([next] . pager-page-down)
   ("\ev" . pager-page-up)
   ([prior] . pager-page-up)
   ([M-up] . pager-row-up)
   ([M-kp-8] . pager-row-up)
   ([M-down] . pager-row-down)
   ([M-kp-2] . pager-row-down))
  )

;;; ==============
;;;  Recent Files
;;; ==============
(use-package recentf)

;;; ========
;;;  Docker
;;; ========
(use-package docker
  :bind ("C-c d" . docker))

;;; ========
;;;  Ispell
;;; ========
(use-package flyspell
  :hook (text-mode . flyspell-mode)
  :config
  (setq ispell-program-name (executable-find "hunspell")
	flyspell-issue-welcome-flag nil
	ispell-really-hunspell t
	ispell-dictionary "en_US"
	ispell-local-dictionary "en_US"
	ispell-local-dictionary-alist
	'(("en_US"
	   "[[:alpha:]]"
	   "[^[:alpha:]]"
	   "[']"
	   nil
	   ("-d" "en_US")
	   nil
	   utf-8))
	ispell-hunspell-dictionary-alist ispell-local-dictionary-alist)
  )

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
		  ("M-$" . flyspell-correct-at-point))
  )

(use-package flyspell-correct-ivy
  :demand t
  :after flyspell-correct
  )

;;; ==============
;;;  languagetool
;;; ==============
(use-package flymake-languagetool
  :if is-mswindows
  :hook ((text-mode       . flymake-languagetool-load)
         (latex-mode      . flymake-languagetool-load)
         (org-mode        . flymake-languagetool-load)
         (markdown-mode   . flymake-languagetool-load)
	 )
  :custom
  (flymake-languagetool-server-jar
   (substitute-in-file-name "${HOME}/Documents/LanguageTool-6.2/languagetool-server.jar"))
  (flymake-languagetool-check-spelling t)
  )

;;; ==========
;;;  Polymode
;;; ==========
(use-package poly-R)
(use-package poly-markdown)
(use-package quarto-mode)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;;; ======
;;;  GAMS 
;;; ======
(use-package gams-mode
  ;; :load-path "c:/Users/Gouel/Documents/git_projects/code/gams-mode"
  :mode ("\\.gms\\'" "\\.inc\\'")
  ;; I don't know why but despite gams-mode being a prog-mode, it does not load
  ;; automatically some default minor modes for prog-mode.
  :hook ((gams-mode . rainbow-delimiters-mode)
	 (gams-mode . smartparens-mode)
	 (gams-mode . display-fill-column-indicator-mode))
  :custom
  (gams-process-command-option "ll=0 lo=3 pw=153 ps=9999")
  (gams-statement-upcase t)
  (gams-fill-column 100)
  (gams-recenter-font-lock t)
  (gams-statement-name "Parameter")
  (gams-dollar-control-name "exit")
  (gams-default-pop-window-height 20)
  ;; Remove the handling of parentheses by gams-mode to use smartparens instead
  (gams-close-paren-always nil)
  (gams-close-double-quotation-always nil)
  (gams-close-single-quotation-always nil)
  ;; Indent
  (gams-indent-on t)
  (gams-indent-number 2)
  (gams-indent-number-loop 2)
  (gams-indent-number-mpsge 2)
  (gams-indent-number-equation 2)
  :config
  (defun find-in-gams-files (string)
    "Find a regular expression in GAMS files"
    (interactive "sRegular expression to find: ")
    (grep (concat "grep -nHI -i -r -e " string " --include=\*.{gms,inc} *" )))
  (if is-mswindows
      (setq gams-system-directory "C:/GAMS/Last/"
	    gams-docs-directory "C:/GAMS/Last/docs")
    (setq gams-system-directory "/opt/gams/gamsLast_linux_x64_64_sfx"
	  gams-docs-directory "/opt/gams/gamsLast_linux_x64_64_sfx/docs"))
  :bind (:map gams-mode-map
	      ("C-c f" . find-in-gams-files))
  )

; Polymode for gams
(define-hostmode poly-gams-hostmode
  :mode 'gams-mode)

(define-innermode poly-gams-yaml-innermode
  :mode 'yaml-mode
  :head-matcher ".?o?n?embeddedcode.* connect:$"
  :tail-matcher ".*embeddedcode.*$"
  :head-mode 'host
  :tail-mode 'host)

(define-innermode poly-gams-python-innermode
  :mode 'python-mode
  :head-matcher ".?o?n?embeddedcode.* python:$"
  :tail-matcher ".*embeddedcode.*$"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-gams-mode
  :hostmode 'poly-gams-hostmode
  :innermodes '(poly-gams-yaml-innermode
		poly-gams-python-innermode))

;;; ===========
;;;  Yaml mode
;;; ===========
(use-package yaml-mode
  :mode ("\\.yml$" "\\.dvc" "dvc.lock")
  :bind (:map yaml-mode-map
	      ("C-m" . newline-and-indent)))

;;; ==============
;;;  Custom theme
;;; ==============
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; ===============
;;;  doom-modeline
;;; ===============
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (if (not (display-graphic-p))
      (setq doom-modeline-icon nil))
  )

;;; =======
;;;  eglot
;;; =======
(use-package eglot
  :ensure nil
  :config
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))  ; Prevent eglot from reformatting code automatically
  :bind
  ("C-c l" . eglot)
  )
  
;;; =======
;;;  eldoc
;;; =======
; Prevent eldoc from showing the function doc in the minibuffer when the cursor is on the function
(setq eldoc-echo-area-use-multiline-p nil)

;;; ============
;;;  format-all
;;; ============
(use-package format-all
  :config
  (setq-default format-all-formatters
		'(("LaTeX"
		   (latexindent "-m" "--yaml=modifyLineBreaks:textWrapOptions:columns:-1,defaultIndent:'  ',indentAfterItems:itemize:0;enumerate:0;description:0"))))
  )

;;; ============
;;;  LaTeX-mode
;;; ============
(use-package tex
  :ensure auctex
  :hook
  (TeX-mode . latex-math-mode)
  (TeX-mode . imenu-add-menubar-index)
  (TeX-mode . turn-on-reftex)
  (TeX-mode . TeX-fold-buffer)
  :hook
  (TeX-mode . TeX-fold-mode)
  ;; Custom functions to compile, preview, and view documents
  (TeX-mode . (lambda ()
		(define-key TeX-mode-map (kbd "<f9>")
		  (lambda ()
                    (interactive)
                    (save-buffer)
                    (TeX-command-menu "latex")))
		(define-key TeX-mode-map (kbd "<f10>")
		  (lambda ()
                    (interactive)
                    (preview-at-point)))
		(define-key TeX-mode-map (kbd "<f12>")
		  (lambda ()
                    (interactive)
                    (TeX-view)
                    [return]))))
  :custom
  (setq-default TeX-auto-parse-length 200)
  (setq-default TeX-master nil)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (LaTeX-item-indent 0)
  (LaTeX-default-options "12pt")
  (LaTeX-math-abbrev-prefix "²")
  (TeX-source-specials-mode 1)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method (quote synctex))
  (TeX-source-correlate-start-server (quote ask))
  (TeX-PDF-mode t)
  (TeX-electric-sub-and-superscript 1)
  (LaTeX-math-list
   '(
     (?\) "right)")
   (?\( "left(")
     (?/ "frac{}{}")
     ))

  ;; Preview
  (preview-auto-cache-preamble t)
  (preview-default-option-list '("displaymath" "graphics" "textmath"))

  ;; Fold-mode
  
  ;; Personalize the list of commands to be folded
  (TeX-fold-macro-spec-list
   '(("[f]"
      ("footnote" "marginpar"))
     ("[c]"
      ("citeyear" "citeauthor" "citep" "citet" "cite"))
     ("[l]"
      ("label"))
     ("[r]"
      ("ref" "pageref" "eqref" "footref" "fref" "Fref"))
     ("[i]"
      ("index" "glossary"))
     ("[1]:||*"
      ("item"))
     ("..."
      ("dots"))
     ("(C)"
      ("copyright"))
     ("(R)"
      ("textregistered"))
     ("TM"
      ("texttrademark"))
     (1
      ("part" "chapter" "section" "subsection" "subsubsection" "paragraph" "subparagraph" "part*" "chapter*" "section*" "subsection*" "subsubsection*" "paragraph*" "subparagraph*" "emph" "textit" "textsl" "textmd" "textrm" "textsf" "texttt" "textbf" "textsc" "textup"))))
  ;; Prevent folding of math to let prettify-symbols do the job
  (TeX-fold-math-spec-list-internal nil)
  (TeX-fold-math-spec-list nil)
  (LaTeX-fold-math-spec-list nil)
  :config
  (if is-mswindows
      (setq preview-gs-command "C:\\Program Files\\gs\\gs10.01.1\\bin\\gswin64c.exe")
    (setq preview-gs-command "gs"))
  :bind (:map TeX-mode-map
	      ("C-c e" . TeX-next-error)
	      ("M-RET" . latex-insert-item))
  )

(use-package reftex
  :custom
  (reftex-bibpath-environment-variables (quote ("BIBINPUTS")))
  (reftex-default-bibliography '("References.bib"))
  (reftex-cite-format (quote natbib))
  (reftex-sort-bibtex-matches (quote author))
  (reftex-plug-into-AUCTeX t)
  (reftex-label-alist '(AMSTeX)) ; Use \eqref by default instead of \ref
  ;; Increase reftex speed (especially on Windows)
  (reftex-enable-partial-scans t)
  (reftex-save-parse-info t)
  (reftex-use-multiple-selection-buffers t)
  :bind (:map reftex-mode-map 
	      ("C-c f" . reftex-fancyref-fref)
	      ("C-c F" . reftex-fancyref-Fref))
  )

;; Beamer
(defun tex-frame ()
  "Run pdflatex on current frame.  Frame must be declared as an environment."
  (interactive)
  (let (beg)
    (save-excursion
      (search-backward "\\begin{frame}")
      (setq beg (point))
      (forward-char 1)
      (LaTeX-find-matching-end)
      (TeX-pin-region beg (point))
      (letf (( (symbol-function 'TeX-command-query) (lambda (x) "LaTeX")))
	(TeX-command-region)))))
(add-hook 'TeX-mode-hook
	  #'(lambda()
	     (local-set-key [(shift return)] 'tex-frame)))

(use-package cdlatex
  :hook
  (LaTeX-mode . turn-on-cdlatex)
  ; Slow down company for a better use of cdlatex
  (LaTeX-mode . (lambda ()
		  (make-local-variable 'company-idle-delay)
		  (setq company-idle-delay 0.3)))
  :config
  ;; Prevent cdlatex from defining LaTeX math subscript everywhere
  (define-key cdlatex-mode-map "_" nil)
  :custom
  (cdlatex-command-alist
	'(("equ*" "Insert equation* env"   "" cdlatex-environment ("equation*") t nil)))
  )
;; Allow tab to be used to indent when the cursor is at the beginning of the line
(add-hook 'cdlatex-tab-hook
          (defun cdlatex-indent-maybe ()
            (when (or (bolp) (looking-back "^[ \t]+"))
              (LaTeX-indent-line))))

;; texfrag to have preview of LaTeX fragment outside LaTeX buffers
(use-package texfrag
  :hook
  (markdown-mode . texfrag-mode)
  (eww-mode . texfrag-mode)
  )

;;; ==========
;;;  doc-view
;;; ==========
(use-package doc-view
  :if is-mswindows
  :config
  (setq doc-view-ghostscript-program "C:\\Program Files\\gs\\gs10.01.1\\bin\\gswin64c.exe"))

;;; ==================
;;;  Auto-compression
;;; ==================
(auto-compression-mode t)

;;; ===============================================
;;;  Matlab - http://matlab-emacs.sourceforge.net/
;;; ===============================================
(use-package  matlab
  :ensure matlab-mode)

;; Matlab mode
;;; Set up matlab-mode to load on .m files
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))

;;; Customization:
(matlab-cedet-setup)
(setq matlab-indent-function t)	; if you want function bodies indented
(setq matlab-verify-on-save-flag nil) ; turn off auto-verify on save
(setq matlab-indent-level 2)
(setq matlab-comment-region-s "% ")
(defun my-matlab-mode-hook ()
  (setq matlab-show-mlint-warnings t)   ; Activate mlint
  (mlint-minor-mode))                   ; Activate mlint minor mode
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

(defun find-in-m-files (string)
  "Find a regular expression in m files."
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nHI -i -r -e " string " --include=*.m *" )))
(define-key matlab-mode-map "\C-cf" 'find-in-m-files)

;; mlint
(if is-mswindows
    (setq mlint-programs (quote ("C:/Program Files/MATLAB/RLast/bin/win64/mlint.exe")))
  (setq mlint-programs (quote ("/usr/local/MATLAB/RLast/bin/glnxa64/mlint"))))

;; Matlab shell
(autoload 'matlab-shell "matlab" "Interactive MATLAB mode." t)
(defun my-matlab-shell-mode-hook ()
  '())
(add-hook 'matlab-shell-mode-hook 'my-matlab-shell-mode-hook)
(setq matlab-shell-command-switches '("-nodesktop -nosplash"))

;;; ============
;;;  projectile
;;; ============
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom
  (projectile-completion-system 'ivy)
  (projectile-use-git-grep t)
  (projectile-switch-project-action #'projectile-dired)
  (projectile-enable-caching nil)
  (projectile-indexing-method 'alien)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/git_projects")
    (setq projectile-project-search-path '("~/Documents/git_projects")))
  )

(use-package ripgrep)

;;; ============================================
;;;  Company - Modern auto-completion framework
;;; ============================================
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq
   ;; Number the candidates (use M-1, M-2 etc to select completions).
   company-show-numbers t
   company-idle-delay 0)
  ;; company configuation from <https://github.com/radian-software/radian/blob/develop/emacs/radian.el>
  :bind (;; Replace `completion-at-point' and `complete-symbol' with
         ;; `company-manual-begin'. You might think this could be put
         ;; in the `:bind*' declaration below, but it seems that
         ;; `bind-key*' does not work with remappings.
         ([remap completion-at-point] . company-manual-begin)
         ([remap complete-symbol] . company-manual-begin)
	 
         ;; The following are keybindings that take effect whenever
         ;; the completions menu is visible, even if the user has not
         ;; explicitly interacted with Company.
	 
         :map company-active-map
	 
         ;; Make TAB always complete the current selection. Note that
         ;; <tab> is for windowed Emacs and TAB is for terminal Emacs.
         ("<tab>" . company-complete-selection)
         ("TAB" . company-complete-selection)
	 
         ;; Prevent SPC from ever triggering a completion.
         ("SPC" . nil)
	 
         ;; The following are keybindings that only take effect if the
         ;; user has explicitly interacted with Company.
	 
         :map company-active-map
         :filter (company-explicit-action-p)
	 
         ;; Make RET trigger a completion if and only if the user has
         ;; explicitly interacted with Company. Note that <return> is
         ;; for windowed Emacs and RET is for terminal Emacs.
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)
	 )
  
  :bind* (;; The default keybinding for `completion-at-point' and
          ;; `complete-symbol' is M-TAB or equivalently C-M-i. Here we
          ;; make sure that no minor modes override this keybinding.
          ("M-TAB" . company-manual-begin))
  )

(use-package company-bibtex)
(use-package company-math)
(use-package company-reftex)
(use-package company-jedi)

(setq company-backends
      (append '((:separate company-bibtex
			   ;; company-reftex-labels
                           company-reftex-citations
			   company-math-symbols-latex
			   company-math-symbols-unicode
			   company-latex-commands))
              company-backends))

(use-package company-quickhelp
  :if (display-graphic-p)
  :custom
  (company-quickhelp-delay 0.1)
  :config
  (eval-after-load 'company
    '(define-key company-active-map (kbd "C-c h") #'company-quickhelp-manual-begin))
)

;;; ===========
;;;  Yasnippet
;;; ===========
(use-package yasnippet)
(yas-global-mode 1)

;;; =====
;;;  ESS
;;; =====
(use-package ess
  :bind (:map ess-r-mode-map
	 ;; Shortcut for pipe |>
         ("C-S-m" . " |>")
	 ;; Shortcut for pipe %>%
	 ("C-%" . " %>%")
	 ;; Shortcut for assign <-
	 ("M--" . ess-insert-assign)
	 ("<f9>" . run-r-script-on-current-buffer-file)
         :map inferior-ess-r-mode-map
         ("C-S-m" . " |>")
         ("C-%" . " %>%")
	 ("M--" . ess-insert-assign))
  :custom
  (ess-roxy-str "#'")
  (ess-roxy-template-alist
   '(("description" . ".. content for \\description{} (no empty lines) ..")
     ("details" . ".. content for \\details{} ..")
     ("param" . "")
     ("return" . "")))
  :config
  (setq ess-assign-list '(" <-" " <<- " " = " " -> " " ->> ")
	ess-style 'RStudio  ; Set code indentation
	ess-ask-for-ess-directory nil  ; Do not ask what is the project directory
	comint-scroll-to-bottom-on-input 'this
	comint-scroll-to-bottom-on-output t
	comint-move-point-for-output t)
  ;; Following the "source is real" philosophy put forward by ESS, one should
  ;; not need the command history and should not save the workspace at the end
  ;; of an R session. Hence, both options are disabled here.
  (setq-default inferior-R-args "--no-restore-history --no-save ")
  ;; Background jobs for R as in RStudio
  (defun run-r-script (arg title)
    (let* ((is-file (file-exists-p arg))
	   (working-directory (if is-file default-directory (file-name-directory arg)))
	   (combuf-name (format "*Rscript-%s*" title)) ; Generate a unique compilation buffer name
           (combuf (get-buffer combuf-name)) ; Get the existing compilation buffer, if any
           (compilation-buffer-name-function (lambda (_) combuf-name)) ; Set the compilation buffer name function
           (compilation-ask-about-save nil) ; Automatically save modified buffers without asking
	   )
      (when combuf
	(kill-buffer combuf)) ; Kill the existing compilation buffer
      (setq combuf (get-buffer-create combuf-name)) ; Create a new compilation buffer
      (with-current-buffer combuf
	(setq default-directory working-directory) ; Set the default directory of the compilation buffer
	(delete-region (point-min) (point-max)) ; Delete any existing content in the compilation buffer
	(compilation-mode)) ; Enable compilation mode in the buffer
      (compile (format "Rscript %s" arg)) ; Execute the R script using Rscript
      (with-current-buffer combuf
	(rename-buffer combuf-name)))) ; Rename the compilation buffer to its final name

  (defun run-r-script-on-current-buffer-file ()
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
	(run-r-script filename (file-name-base filename)))))

  (defun run-r-script-on-file ()
    (interactive)
    (let ((filename (read-file-name "R script: ")))
      (run-r-script filename (file-name-base filename))))
  )

(define-key inferior-ess-mode-map [home] 'comint-bol)

(use-package rutils) ; To interact easily with renv

(defun find-in-R-files (string)
  "Find a regular expression in R files."
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nHI -i -r -e " string " --include=\*.{R,Rmd,qmd} *" )))
(define-key ess-mode-map "\C-cf" 'find-in-R-files)

(add-hook 'ess-mode-hook
	  #'(lambda ()
	      (outline-minor-mode)
	      (setq outline-regexp "^# .*----")
	      (defun outline-level ()
		(cond (looking-at "^# .*----") 1)
		(cond (looking-at "^## .*----") 2)
		(cond (looking-at "^### .*----") 3)
		(cond (looking-at "^#### .*----") 4)
		((looking-at "^[a-zA-Z0-9_\.]+ ?<- ?function(.*{") 5)
		(t 1000)
		)))

(defun my-inferior-ess-init ()
  "Workaround for https://github.com/emacs-ess/ESS/issues/1193"
  (add-hook 'comint-preoutput-filter-functions #'xterm-color-filter -90 t)
  (setq-local ansi-color-for-comint-mode nil)
  (smartparens-mode 1)
  )
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

;;; ===================================
;;;  Définition de touches perso global
;;; ===================================
;; (define-key global-map [(f5)] 'revert-buffer)

;;; ===============
;;;  Markdown mode
;;; ===============
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :custom
  (markdown-command
   (concat "pandoc"
	   " --from=markdown --to=html"
	   " --standalone --mathjax"
	   ;; " --citeproc --bibliography="
	   ;; (shell-quote-argument (substitute-in-file-name "${BIBINPUTS}\\References.bib"))
	   ))
  (markdown-enable-math t)
  (markdown-enable-prefix-prompts nil)
  (markdown-header-scaling nil)
  (markdown-hide-markup nil)
  (markdown-fontify-code-blocks-natively t)
  (markdown-enable-highlighting-syntax t)
  :config
  ;; Code to import screenshots in markdown files
  ;; from <https://www.nistara.net/post/2022-11-14-emacs-markdown-screenshots> and
  ;; <https://stackoverflow.com/questions/17435995/paste-an-image-on-clipboard-to-emacs-org-mode-file-without-saving-it/31868530#31868530>
  (defun markdown-screenshot ()
    "Copy a screenshot into a time stamped unique-named file in the
same directory as the working and insert a link to this file."
    (interactive)
    (setq filename
          (concat
           (make-temp-name
            (concat (file-name-nondirectory (buffer-file-name))
                    "_screenshots/"
                    (format-time-string "%Y-%m-%d_%a_%kh%Mm_")) ) ".png"))
    (unless (file-exists-p (file-name-directory filename))
      (make-directory (file-name-directory filename)))
					; copy the screenshot to file
    (shell-command (concat "powershell -command \"Add-Type -AssemblyName System.Windows.Forms;if ($([System.Windows.Forms.Clipboard]::ContainsImage())) {$image = [System.Windows.Forms.Clipboard]::GetImage();[System.Drawing.Bitmap]$image.Save('" filename "',[System.Drawing.Imaging.ImageFormat]::Png); Write-Output 'clipboard content saved as file'} else {Write-Output 'clipboard does not contain image data'}\""))
					; insert into file if correctly taken
    (if (file-exists-p filename)
	(insert (concat "![](" filename ")")))
    (markdown-display-inline-images)
    (newline)
    )
  ;; Code to use RefTeX to input references in markdown
  ;; from <https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a>
  ;; define markdown citation formats
  (defvar markdown-cite-format)
  (setq markdown-cite-format
	'(
          (?\C-m . "[@%l]")
          (?p . "[@%l]")
          (?t . "@%l")
          ))
  ;; wrap reftex-citation with local variables for markdown format
  (defun markdown-reftex-citation ()
    (interactive)
    (let ((reftex-cite-format markdown-cite-format)
          (reftex-cite-key-separator "; @"))
      (reftex-citation)))
  :hook
  (markdown-mode . imenu-add-menubar-index)
  :bind (:map markdown-mode-map
	      ("C-c [" . markdown-reftex-citation))
  )

(use-package pandoc-mode
  :hook
  (markdown-mode . pandoc-mode)
  (pandoc-mode . pandoc-load-default-settings)
  )

;;; =======
;;;  Magit
;;; =======
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom
  (magit-diff-refine-hunk (quote all))
  :config
  (remove-hook 'server-switch-hook 'magit-commit-diff)  ; Do not diff when committing
  (remove-hook 'with-editor-filter-visit-hook 'magit-commit-diff))  ; Do not diff when committing

(use-package magit-delta
  :hook (magit-mode . magit-delta-mode))

;;; =========
;;;  diff-hl
;;; =========
(use-package diff-hl
  :defer t
  :after magit
  :hook
  (prog-mode . diff-hl-mode)
  (latex-mode . diff-hl-mode)
  (dired-mode . diff-hl-dired-mode)
  (magit-post-refresh . diff-hl-magit-post-refresh)
  )

;;; =================
;;;  ivy and friends
;;; =================
(use-package counsel
  :config (counsel-mode)
  )

(use-package ivy
  :demand
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config (ivy-mode)
  )

(use-package swiper)

;; swiper is slow for large files so it is replaced by isearch for large files
(defun search-method-according-to-numlines ()
  "Determine the number of lines of current buffer and chooses a search method accordingly."
  (interactive)
  (if (< (count-lines (point-min) (point-max)) 20000)
      (swiper)
    (isearch-forward)
    )
  )
(global-set-key "\C-s" 'search-method-according-to-numlines)

(use-package ivy-xref
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs)
  )

(use-package ivy-prescient
  :after counsel
  :config
  (ivy-prescient-mode)
  )

(use-package ivy-rich
  :after nerd-icons-ivy-rich
  :init (ivy-rich-mode +1)
  )

;;; ============
;;;  ivy-bibtex
;;; ============
(use-package ivy-bibtex
  :if is-mswindows
  :custom
  (bibtex-completion-bibliography
   (substitute-in-file-name "${BIBINPUTS}/References.bib"))
  (bibtex-completion-library-path
   (substitute-in-file-name "${HOME}/Dropbox (Inrae EcoPub)/Bibliography/Papers"))
  (bibtex-completion-pdf-symbol "⌘")
  )

;;; =======
;;;  Julia
;;; =======
(use-package julia-mode)

;;; ==================
;;;  View Large Files
;;; ==================
(use-package vlf)

;;; ===============
;;;  expand-region
;;; ===============
(use-package expand-region
  :bind ("C-!" . er/expand-region))

;;; =======
;;;  Tramp
;;; =======
(if is-mswindows
    (setq tramp-default-method "plink"))

;;; ===========
;;;  pdf-tools
;;; ===========
(use-package pdf-tools
  :init
  (pdf-tools-install)  ; Standard activation command
  (pdf-loader-install) ; On demand loading, leads to faster startup time
  :config
  (setq TeX-view-program-selection '((output-pdf "PDF Tools"))
	TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
	TeX-source-correlate-start-server t)
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
  :bind (:map pdf-view-mode-map
	      ("C-s" . isearch-forward))
  )

;;; ==========
;;;  obsidian
;;; ==========
(use-package obsidian
  :demand t
  :config
  (obsidian-specify-path "~/Dropbox (Inrae EcoPub)/obsidian")
  (global-obsidian-mode t)
  :custom
  ;; This directory will be used for `obsidian-capture' if set.
  (obsidian-inbox-directory "Inbox")
  :bind (:map obsidian-mode-map
	      ;; Replace C-c C-o with Obsidian.el's implementation. It's ok to use another key binding.
	      ("C-c C-o" . obsidian-follow-link-at-point)
	      ;; Jump to backlinks
	      ("C-c C-b" . obsidian-backlink-jump)
	      ;; If you prefer you can use `obsidian-insert-link'
	      ("C-c C-l" . obsidian-insert-wikilink))
  )
  
;;; init.el ends here
