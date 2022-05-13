;; (byte-recompile-directory "~/.emacs.d/site-lisp" 1)

(defconst mswindows (equal window-system 'w32))

;;; My location for external packages.
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(setq backup-directory-alist
     	  '(("." . "~/.emacs.d/backup")))
;;; =======
;;;  Shell
;;; =======

(global-set-key [f1] 'shell)
(global-set-key [f2] 'eshell)

(if mswindows    ;; MS Windows clipboard is UTF-16LE
    (defun bash ()
      (interactive)
      (let ((shell-file-name "C:\\Program Files\\Git\\bin\\bash.exe" ))
	(shell "*bash*"))
      ))
(setq explicit-bash.exe-args '("--login" "-i"))

(add-hook 'shell-mode-hook
      (lambda ()
        (face-remap-set-base 'comint-highlight-prompt :inherit nil)))

;;; ==================
;;;  Packages manager
;;; ==================
;; use-package setup
(require 'package)
(setq package-enable-at-startup nil) ; dont do it immediately
(setq package-archives '(("gnu_elpa"  . "https://elpa.gnu.org/packages/")
			 ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package

;; Define packages
(require 'use-package)

;; Always download if not available
(setq use-package-always-ensure t)

;;; ===============
;;;  Others
;;; ===============
;;; Activate lower- and upper-case commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(if (display-graphic-p)
    (server-start)
  (xterm-mouse-mode))

;;; ===============
;;;  all-the-icons
;;; ===============
(use-package all-the-icons
  :if (display-graphic-p))
(use-package all-the-icons-dired
  :init (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))
(use-package all-the-icons-ivy
  :init (add-hook 'after-init-hook 'all-the-icons-ivy-setup))

;;; ======================
;;;  find-file-in-project
;;; ======================
(use-package find-file-in-project)

;;; =========
;;;  neotree
;;; =========
(use-package neotree
  :demand
  :bind ([f3] . neotree-project-dir)
  :custom
  (neo-theme (if (display-graphic-p) 'icons 'arrow))
  (neo-show-hidden-files t))

(defun neotree-project-dir ()
  "Open NeoTree using the git root."
  (interactive)
  (if (neo-global--window-exists-p)
      (neotree-hide)
    (let ((project-dir (ffip-project-root))
        (file-name (buffer-file-name)))
    (if project-dir
        (progn
          (neotree-dir project-dir)
          (neotree-find file-name))
      (message "Could not find git project root.")))))

;;; ==========
;;;  ado-mode
;;; ==========
(use-package ado-mode)

;;; =============
;;;  Python mode
;;; =============
(use-package elpy
  :ensure t
  :init
  (elpy-enable))
;; (use-package conda)
;; (use-package py-autopep8)
;; (use-package blacken)

;; (setq python-shell-interpreter "jupyter"
;;       python-shell-interpreter-args "console --simple-prompt"
;;       python-shell-prompt-detect-failure-warning nil)
;; (add-to-list 'python-shell-completion-native-disabled-interpreters
;;              "jupyter")

;; (use-package ein) ;; Emacs IPython Notebook

;;; ==========
;;;  Org mode
;;; ==========
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-hide-leading-stars t)
(setq org-export-with-LaTeX-fragments t)       ; Export LaTeX fragment to HTML
(setq org-todo-keywords '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)")))
(setq org-tag-alist '(("OFFICE" . ?o) ("COMPUTER" . ?c) ("HOME" . ?h) ("PROJECT" . ?p) ("CALL" . ?a) ("ERRANDS" . ?e) ("TASK" . ?t)))
(setq org-hide-leading-stars t)
;; (setq org-directory "c:/Users/Christophe/Dropbox/Org")

;; Integration of RefTeX in org
(defun org-mode-reftex-setup ()
  (load-library "reftex")
  (and (buffer-file-name)
       (file-exists-p (buffer-file-name))
        (global-auto-revert-mode t)
       (reftex-parse-all))
  (define-key org-mode-map (kbd "C-c )") 'reftex-citation)
  )
(add-hook 'org-mode-hook 'org-mode-reftex-setup)

;;; ===============
;;;  Look and feel
;;; ===============
(setq blink-cursor-blinks 0)             ; curseur clignote indéfiniment
(global-hl-line-mode +1)                 ; Highlight the current line
(setq-default cursor-type 'bar)          ; curseur étroit
(set-face-background 'cursor "#CC0000")  ; curseur rouge foncé
(setq jit-lock-chunk-size 50000)

(setq large-file-warning-threshold 100000000) ; set large file threshold at 100 megabytes

(require 'font-lock)
(global-font-lock-mode t)                ; colorisation du texte
;; (transient-mark-mode t)                  ; mode de sélection "normal"
(delete-selection-mode t)                ; entrée efface texte sélectionné
(setq-default mouse-yank-at-point t)     ; coller avec la souris
(show-paren-mode t)                      ; coupler les parenthèses
(setq-default case-fold-search t)        ; recherche sans égard à la casse

(setq display-time-24hr-format t)        ; Affichage de l'heure format 24h
(display-time)                           ; Affichage de l'errur dans le bandeau

(fset 'yes-or-no-p 'y-or-n-p)            ; Replace yes or no with y or n

(add-hook 'write-file-hooks 'time-stamp) ; Time-stamp

(setq default-major-mode 'text-mode)     ; mode par défaut
(setq text-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'text-mode-hook 'turn-on-auto-fill) ; wrap long lines in text mode

(setq column-number-mode t)              ; affichage du numéro de la colonne

(setq frame-title-format '(buffer-file-name "Emacs: %b (%f)" "Emacs: %b")) ; Affiche le chemin complet du fichier dans la barre de titre d'Emacs

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
(if mswindows    ;; MS Windows clipboard is UTF-16LE
    (set-clipboard-coding-system 'utf-16le-dos))

;;; Police Consolas
;; (if mswindows
;;   (progn
;;     (set-face-font 'default "-outline-Consolas-normal-r-normal-normal-*-*-96-96-c-*-iso8859-1")
;;     (set-face-font 'bold "-outline-Consolas-bold-r-normal-normal-*-*-96-96-c-*-iso8859-1")
;;     (set-face-font 'italic "-outline-Consolas-normal-i-normal-normal-*-*-96-96-c-*-iso8859-1")
;;     (set-face-font 'bold-italic "-outline-Consolas-bold-i-normal-normal-*-*-96-96-c-*-iso8859-1"))
  ;; (set-default-font "DejaVu Sans Mono 10"))

;;; Remove menu bar in terminal mode
(if (display-graphic-p)
    ()
  (progn (menu-bar-mode -1)
   (global-hl-line-mode 0)))

;;; Remove toolbar
(if window-system
    (tool-bar-mode 0))

(setq inhibit-startup-screen t)

;;; ====================
;;;  rainbow-delimiters
;;; ====================
(use-package rainbow-delimiters
  :init
  (progn
    (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
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
  :config (progn (show-smartparens-global-mode t)))
(add-hook 'prog-mode-hook 'smartparens-mode)
(add-hook 'markdown-mode-hook 'smartparens-mode)
(setq markdown-enable-math t)

;;; ========
;;;  Unfill
;;; ========
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

;;; ============================================
;;;  Pager - From ELPA
;;; ============================================
(use-package pager
  :bind
  (("\C-v" . pager-page-down)
   ([next] . pager-page-down)
   ("\ev" . pager-page-up)
   ([prior] . pager-page-up)
   ([M-up] . pager-row-up)
   ([M-kp-8] . pager-row-up)
   ([M-down] . pager-row-down)
   ([M-kp-2] . pager-row-down)))

;;; ==============
;;;  Recent Files
;;; ==============
(use-package recentf)

;;; ========
;;;  Docker
;;; ========
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

;;; ========
;;;  Ispell
;;; ========
(use-package ispell
  :config
  (setq ispell-list-command "list")
  (setq flyspell-issue-welcome-flag nil)
  (setq ispell-local-dictionary "en_US")
  (setq ispell-local-dictionary-alist
	'(("en_US" "[[:alpha:]]" "[^[:alpha:]]" "[']" nil ("-d" "en_US") nil utf-8)))
  (setq ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))
(if mswindows
    (setq-default ispell-program-name "C:/ProgramData/chocolatey/lib/hunspell.portable/tools/hunspell.exe")
  (setq-default ispell-program-name "aspell"))

;;; ========================================================
;;;  Gams - http://shirotakeda.org/en/gams/gams-mode/
;;; ========================================================
(use-package gams-mode
  :mode ("\\.gms\\'" "\\.inc\\'")
  :init
  (progn
    (add-hook 'gams-mode-hook 'rainbow-delimiters-mode)
    (add-hook 'gams-mode-hook 'smartparens-mode))
  :custom
  (gams-process-command-option "ll=0 lo=3 pw=153 ps=9999")
  (gams-statement-upcase t)
  (gams-fill-column 80)
  (gams-recenter-font-lock t)
  (gams-statement-name "Parameter")
  (gams-dollar-control-name "exit")
  (gams-default-pop-window-height 20)
  ;; (gams-inlinecom-symbol-start-default "{")
  ;; (gams-inlinecom-symbol-end-default "}")
  ;; Remove the handling of parenthèses by gams-mode to use smartparens instead
  (gams-close-paren-always nil)
  (gams-close-double-quotation-always nil)
  (gams-close-single-quotation-always nil)
  ;; Indent
  (gams-indent-on t)
  (gams-indent-number 2)
  (gams-indent-number-loop 2)
  (gams-indent-number-mpsge 2)
  (gams-indent-number-equation 2)
  (font-lock-support-mode
   '((gams-mode . nil)
     (t . jit-lock-mode)))
  :config
  (defun find-in-gms-files (string)
    "Find a regular expression in gms files"
    (interactive "sRegular expression to find: ")
    (grep (concat "grep -nHI -i -r -e " string " --include=*.gms *" )))
  :bind ("\C-cf" . find-in-gms-files))

(if mswindows
  (progn
    (setq gams-process-command-name "C:/GAMS/Last/gams.exe")
    (setq gams-system-directory "C:/GAMS/Last/")
    (setq gams-docs-directory "C:/GAMS/Last/docs")
    (setq gams-docs-view-program "C:/Program Files (x86)/Foxit Software/Foxit Reader/FoxitReader.exe")
    (setq load-path
	  (cons "C:/GAMS/Last/" ;; Set the installed directory!
		load-path)))
  (progn
    (setq gams-docs-directory "/opt/gams/gamsLast_linux_x64_64_sfx/docs")
    (setq gams-docs-view-program "qpdfview")))

;;; =========================================================================
;;;  Lancer une recherche d'article sous IDEAS ou google-search depuis Emacs
;;; =========================================================================
(defun ideas (command)
;"Uses `browse-url' to submit keywords to IDEAS and open result in an external browser defined in `browse-url-browser-function'."
  (interactive "sCommand:")
  (browse-url
   (concat "http://ideas.repec.org/cgi-bin/htsearch?words="command)))

(defun google-scholar (command)
  (interactive "sCommand:")
  (browse-url
   (concat "http://scholar.google.com/scholar?q="command)))

;;; ==================================================
;;;  Yaml mode - https://github.com/yoshiki/yaml-mode
;;; ==================================================
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode)
  :bind ("\C-m" . newline-and-indent))

;;; ==============
;;;  Custom theme
;;; ==============
(load-theme 'material t)  ;; Load material theme

;;; ============
;;;  LaTeX-mode
;;; ============
(use-package tex
  :ensure auctex
  :init
  (add-hook 'TeX-mode-hook 'flyspell-mode)
  (add-hook 'TeX-mode-hook 'auto-fill-mode)
  (add-hook 'TeX-mode-hook 'latex-math-mode)
  (add-hook 'TeX-mode-hook 'imenu-add-menubar-index)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (add-hook 'TeX-mode-hook 'TeX-fold-mode 1)
  (add-hook 'TeX-mode-hook
            '(lambda ()
               (define-key TeX-mode-map (kbd "<f9>")
		 (lambda ()
                   (interactive)
                   (save-buffer)
                   (TeX-command-menu "Latex")))
               (define-key TeX-mode-map (kbd "<f10>")
		 (lambda ()
                   (interactive)
		   ;; (TeX-fold-buffer)
                   (preview-at-point)))
               (define-key TeX-mode-map (kbd "<f12>")
		 (lambda ()
                   (interactive)
                   (TeX-view)
                   [return]))))
  :custom
  (reftex-bibpath-environment-variables (quote ("BIBINPUTS")))
  (setq-default TeX-auto-parse-length 200)
  (setq-default TeX-master nil)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (LaTeX-default-options "12pt")
  (reftex-cite-format (quote natbib))
  (reftex-sort-bibtex-matches (quote author))
  (LaTeX-math-abbrev-prefix "²")
  (TeX-source-specials-mode 1)
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method (quote synctex))
  (TeX-source-correlate-start-server (quote ask))
  (TeX-PDF-mode t)
  (fill-column 80)
  (reftex-plug-into-AUCTeX t)
  (TeX-electric-sub-and-superscript 1)
  (LaTeX-math-list
   '(
     (?) "right)")
   (?( "left(")
     (?/ "frac{}{}")
     ))
  :bind
  ("\C-ce" . TeX-next-error)
  ("\C-cf" . reftex-fancyref-fref)
  ("\C-cF" . reftex-fancyref-Fref))

(if mswindows
    ()
  (add-hook 'TeX-mode-hook 'TeX-fold-buffer t))

(if mswindows
    (progn
      (require 'tex-mik)
      (eval-after-load "tex"
	'(add-to-list 'TeX-command-list
		      '("htlatex" "htlatex %s" TeX-run-command t t :help "Run htlatex") t))
      (eval-after-load "tex"
	'(add-to-list 'TeX-command-list
		      '("htlatexword" "htlatexword %s" TeX-run-command nil t :help "Run htlatex with Word options") t))
      (eval-after-load "tex"
	'(add-to-list 'TeX-command-list
		      '("PDFViewerClose" "PDFXCview-close.bat %s" TeX-run-command nil t :help "Close PDF open in PDF-XChange Viewer") t))))

(if mswindows
    (progn
      (setq TeX-view-program-list (quote (("Sumatra PDF" ("\"SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n") " %o")))))
      (setq TeX-view-program-selection (quote ((output-pdf "Sumatra PDF")
					       (output-dvi "Yap")))))
    (progn
      (setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"%o#src:%b:%n:0\"")))
      (setq TeX-view-program-selection '((output-pdf "qpdfview")
					 (output-dvi "xdvi")))))

;; Preview
(setq preview-scale-function 1.5)      ; Higher preview images in TeX buffers
(setq preview-auto-cache-preamble t)
(if mswindows
    (setq preview-gs-command "c:/Program Files/gs/gs9.55.0/bin/gswin64c.exe")
  (setq preview-gs-command "gs"))

;; Beamer
(defun tex-frame ()
  "Run pdflatex on current frame. Frame must be declared as an environment."
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
	  '(lambda()
	     (local-set-key [(shift return)] 'tex-frame)))

;;; =====
;;;  PDF
;;; =====
(use-package doc-view
  :if mswindows
  :config
  (setq doc-view-ghostscript-program "C:\\Program Files\\gs\\gs9.50\\bin\\gswin64c.exe"))

;;; ==================
;;;  Auto-compression
;;; ==================
(auto-compression-mode t)

;;; ================
;;;  Ibuffer
;;; ================
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffers." t)

;;; ===============================================
;;;  Matlab - http://matlab-emacs.sourceforge.net/
;;; ===============================================
(require 'matlab)

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
  (setq fill-column 80) 	        ; where auto-fill should wrap
  (setq matlab-show-mlint-warnings t)   ; Activate mlint
  (mlint-minor-mode))                   ; Activate mlint minor mode
(add-hook 'matlab-mode-hook 'my-matlab-mode-hook)

(defun find-in-m-files (string)
  "Find a regular expression in m files"
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nHI -i -r -e " string " --include=*.m *" )))
(define-key matlab-mode-map "\C-cf" 'find-in-m-files)

;; mlint
(if mswindows
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
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/Documents/git_projects")
    (setq projectile-project-search-path '("~/Documents/git_projects")))
  (setq projectile-switch-project-action #'projectile-dired))

;;; =====================================
;;;  Activation du clic droit comme aide
;;; =====================================
(define-key global-map [(mouse-3)] 'mouse-me)

;;; =========
;;;  Company
;;; =========
(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

;;; =====
;;;  ESS
;;; =====
(require 'ess-site)

(use-package ess
  :bind (;; Shortcut for pipe |>
	 :map ess-r-mode-map
         ("C-S-m" . " |> ")
         :map inferior-ess-r-mode-map
         ("C-S-m" . " |> ")
	 ;; Shortcut for pipe %>%
	 :map ess-r-mode-map
	 ("C-%" . " %>% ")
         :map inferior-ess-r-mode-map
         ("C-%" . " %>% ")
	 ;; Shortcut for assign <-
	 :map ess-r-mode-map
	 ("M--" . ess-insert-assign)
         :map inferior-ess-r-mode-map
	 ("M--" . ess-insert-assign)))

;; Following the "source is real" philosophy put forward by ESS, one
;; should not need the command history and should not save the
;; workspace at the end of an R session. Hence, both options are
;; disabled here.
(setq-default inferior-R-args "--no-restore-history --no-save ")

(setq comment-column 0) ; Prevent indentation of lines starting with one #
(setq ess-style 'RStudio) ; Set code indentation

;; Add a vertical line at 80 columns
(setq ess-mode-hook '(lambda () (setq fill-column 80)))
(add-hook 'ess-mode-hook (lambda ()
			   (display-fill-column-indicator-mode)))

(define-key inferior-ess-mode-map [home] 'comint-bol)
(define-key ess-mode-map (kbd "C-;") 'comment-region)
(define-key ess-mode-map [(control ?c) (?:)] 'uncomment-region)


(setq ess-ask-for-ess-directory nil) ; Do not ask what is the project directory
;; (setq ansi-color-for-comint-mode 'filter)
;; (setq comint-prompt-read-only t) ; Prevent the ess prompt (>) to be writable
;; (setq comint-scroll-to-bottom-on-input t) ; Move the cursor to > when typing in iESS buffer
(setq comint-scroll-to-bottom-on-input 'this)
(setq comint-scroll-to-bottom-on-output t)
(setq comint-move-point-for-output t)

;; (defun my-ess-start-R ()
;;   (interactive)
;;   (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
;;     (progn
;;       (delete-other-windows)
;;       (setq w1 (selected-window))
;;       (setq w1name (buffer-name))
;;       (setq w2 (split-window w1))
;;       (R)
;;       (set-window-buffer w2 "*R*")
;;       (set-window-buffer w1 w1name))))

;; (defun my-ess-eval ()
;;   (interactive)
;;   (my-ess-start-R)
;;   (if (and transient-mark-mode mark-active)
;;     (call-interactively 'ess-eval-region)
;;     (call-interactively 'ess-eval-line-and-step)))

; Call imenu with \C-c =
(define-key ess-mode-map "\C-c=" 'imenu)

(defun find-in-R-files (string)
  "Find a regular expression in R files"
  (interactive "sRegular expression to find: ")
  (grep (concat "grep -nHI -i -r -e " string " --include=*.R* *" )))
(define-key ess-mode-map "\C-cf" 'find-in-R-files)

;;; ===================================
;;;  Définition de touches perso global
;;; ===================================
(define-key global-map [(M-f1)] 'bookmark-bmenu-list)
(define-key global-map [(f5)] 'revert-buffer)
;; (define-key global-map "\C-c;" 'comment-region)
;; (define-key global-map "\C-c:" 'uncomment-region)

;;; ===============
;;;  Markdown mode
;;; ===============
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)
(add-hook 'markdown-mode-hook 'flyspell-mode)

;;; ==========
;;;  Polymode
;;; ==========
(require 'poly-R)
(require 'poly-markdown)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;;; =======
;;;  Magit
;;; =======
(use-package magit
  :bind ("C-x g" . magit-status)
  :custom (magit-diff-refine-hunk (quote all)))

;;; ========
;;;  ccrypt
;;; ========
(require 'ps-ccrypt)

;;; =================
;;;  ivy and friends
;;; =================
(use-package ivy
  :demand
  :bind ("\C-s" . swiper)
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config (ivy-mode))

;;; ==========
;;;  flycheck
;;; ==========
(use-package flycheck
  :init (global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not LaTeX-mode latex-mode))
  (setq flycheck-global-modes '(not ess-mode ess-mode)))

;;; Sent deleted files to trash
(setq delete-by-moving-to-trash t)

;;; =======
;;;  Julia
;;; =======
(setq inferior-julia-program-name "c:/Users/gouel/AppData/Local/Julia-0.6.2/bin/julia.exe")

(require 'vlf)

(require 'ein)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("90a6f96a4665a6a56e36dec873a15cbedf761c51ec08dd993d6604e32dd45940" "f149d9986497e8877e0bd1981d1bef8c8a6d35be7d82cba193ad7e46f0989f6a" "14de8f58ad656af5be374086ae7ab663811633fc1483a02add92f7a1ff1a8455" "bf390ecb203806cbe351b966a88fc3036f3ff68cd2547db6ee3676e87327b311" default))
 '(ess-R-font-lock-keywords
   '((ess-R-fl-keyword:modifiers . t)
     (ess-R-fl-keyword:fun-defs . t)
     (ess-R-fl-keyword:keywords . t)
     (ess-R-fl-keyword:assign-ops . t)
     (ess-R-fl-keyword:constants . t)
     (ess-fl-keyword:fun-calls . t)
     (ess-fl-keyword:numbers . t)
     (ess-fl-keyword:operators . t)
     (ess-fl-keyword:delimiters . t)
     (ess-fl-keyword:= . t)
     (ess-R-fl-keyword:F&T . t)))
 '(latex-preview-pane-multifile-mode 'auctex)
 '(package-selected-packages
   '(material-theme yaml-mode vlf visual-fill-column use-package smartparens rainbow-delimiters projectile poly-R pandoc-mode pager neotree matlab-mode magit latex-preview-pane julia-shell julia-repl htmlize gams-ac flycheck-julia find-file-in-project ess espresso-theme ein counsel company auctex all-the-icons-ivy all-the-icons-dired ado-mode)))

(setenv "CYGWIN" "nodosfilewarning")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "orange"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "cyan"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "violet"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "purple"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "black"))))
 '(rainbow-delimiters-unmatched-face ((t (:background "yellow")))))
