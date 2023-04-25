;; To install manually:
;; - LSP servers
;; ```{bash}
;;   pip3 install --user python-lsp-server[all] jupyter[all]
;;   Rscript -e "install.packages('languageserver')"
;;   Curl --output %HOME%/.local/bin/digestif.cmd https://raw.githubusercontent.com/astoff/digestif/master/scripts/digestif.cmd
;; ```
;; - M-x all-the-icons-install-fonts
;; - Download and install fonts
;;   - <https://fonts.google.com/specimen/Fira+Code>
;;   - <https://candyfonts.com/font/symbola.htm>

(defconst mswindows (equal window-system 'w32))

(setq backup-directory-alist
     	  '(("." . "~/.emacs.d/backup")))

;; Define a file in which any customization is saved
(setq custom-file (concat user-emacs-directory "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;;; =======
;;;  Shell
;;; =======

(global-set-key [f1] 'shell)
(global-set-key [f2] 'eshell)

(use-package eshell-git-prompt
  :config
  (eshell-git-prompt-use-theme 'powerline))

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
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package setup
;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents) ; update archives
  (package-install 'use-package)) ; grab the newest use-package
(require 'use-package)
;; Always download packages if not available
(setq use-package-always-ensure t)

;; Quelpa
(use-package quelpa)
(use-package quelpa-use-package)

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
  :if (display-graphic-p)
  :hook (dired-mode . all-the-icons-dired-mode))
(use-package all-the-icons-ivy
  :if (display-graphic-p)
  :hook (after-init . all-the-icons-ivy-setup))

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
  :init
  (add-hook 'dired-mode-hook (lambda ()
			       (dired-hide-details-mode)))
  (add-hook 'dired-mode-hook 'auto-revert-mode)
  :custom
  ((dired-listing-switches "-agho --group-directories-first"))
  )

;;; ======================
;;;  greek-unicode-insert
;;; ======================
(use-package greek-unicode-insert
  :quelpa (greek-unicode-insert :fetcher git :url "https://github.com/Malabarba/greek-unicode-insert.git")
  :bind ("²" . greek-unicode-insert-map))

;;; ======================
;;;  treemacs
;;; ======================
(use-package treemacs
  :defer t
  :after (treemacs-all-the-icons)
  :hook (treemacs-mode . no_code_mode)
  :config
  (setq treemacs-width 20
	treemacs-indentation '(4 px)
	treemacs-is-never-other-window t
	treemacs-width-is-initially-locked nil
	treemacs-space-between-root-nodes nil
	treemacs-collapse-dirs 4
	treemacs-text-scale -1)
  (treemacs-resize-icons 14)
  (treemacs-follow-mode t)
  (treemacs-tag-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (treemacs-hide-gitignored-files-mode nil)
  (treemacs-load-theme "all-the-icons")
  )

(setq treemacs-python-executable "python")

(global-set-key [f3] 'treemacs-select-window)

(use-package treemacs-magit
  :after (treemacs magit)
  )

;;; ===============================
;;;  font-lock for hex color codes
;;; ===============================
 (defvar hexcolour-keywords
   '(("#[abcdef[:digit:]]\\{6\\}"
      (0 (put-text-property (match-beginning 0)
                            (match-end 0)
			    'face (list :background
				        (match-string-no-properties 0)))))))
(defun hexcolour-add-to-font-lock ()
  (font-lock-add-keywords nil hexcolour-keywords))

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
	python-shell-interpreter-args "-i --simple-prompt"
	python-shell-prompt-detect-failure-warning nil)
;; Set encoding to utf-8 to allows utf-8 characters in Python REPL (from https://stackoverflow.com/questions/14172576/why-unicodeencodeerror-raised-only-in-emacss-python-shell?utm_source=pocket_reader)
  (setenv "PYTHONIOENCODING" "utf-8")
  )
(use-package conda
  :config
  (setq-default mode-line-format (cons '(:exec conda-env-current-name) mode-line-format))
  )
(use-package poetry)

;;; ==========
;;;  Org mode
;;; ==========
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-hide-leading-stars t)
(setq org-export-with-LaTeX-fragments t)       ; Export LaTeX fragment to HTML
(setq org-todo-keywords '((type "TODO(t)" "STARTED(s)" "WAITING(w)" "|" "DONE(d)")))
(setq org-tag-alist '(("OFFICE" . ?o) ("COMPUTER" . ?c) ("HOME" . ?h) ("PROJECT" . ?p) ("CALL" . ?a) ("ERRANDS" . ?e) ("TASK" . ?t)))
(setq org-hide-leading-stars t)

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

;;; ========================================
;;;  Look, feel, and general emacs behavior
;;; ========================================
(setq blink-cursor-blinks 0)             ; curseur clignote indéfiniment
(global-hl-line-mode +1)                 ; Highlight the current line
(setq-default cursor-type 'bar)          ; curseur étroit
(set-face-background 'cursor "#CC0000")  ; curseur rouge foncé
(setq jit-lock-chunk-size 50000)

(setq large-file-warning-threshold 100000000) ; set large file threshold at 100 megabytes

(if (display-graphic-p)
    (add-to-list 'default-frame-alist
             '(font . "Fira Code Regular-10"))
  )
(set-fontset-font "fontset-default" 'symbol "Symbola")

(require 'font-lock)
(global-font-lock-mode t)                ; colorisation du texte
;; (transient-mark-mode t)                  ; mode de sélection "normal"
(delete-selection-mode t)                ; entrée efface texte sélectionné
(setq-default mouse-yank-at-point t)     ; coller avec la souris
(show-paren-mode t)                      ; coupler les parenthèses
(setq-default case-fold-search t)        ; recherche sans égard à la casse

(setq display-time-24hr-format t)        ; Affichage de l'heure format 24h

(fset 'yes-or-no-p 'y-or-n-p)            ; Replace yes or no with y or n

(setq default-major-mode 'text-mode)     ; mode par défaut

(setq column-number-mode t)              ; affichage du numéro de la colonne

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
(if mswindows    ;; MS Windows clipboard is UTF-16LE
    (set-clipboard-coding-system 'utf-16le-dos))

;;; Sent deleted files to trash
(setq delete-by-moving-to-trash t)


;;; Remove menu bar in terminal mode
(if (display-graphic-p)
    ()
  (progn (menu-bar-mode -1)
   (global-hl-line-mode 0)))

;;; Remove toolbar
(if window-system
    (tool-bar-mode 0))

(setq inhibit-startup-screen t)

;;; ==================
;;;  prettify-symbols
;;; ==================
;; From <https://emacsredux.com/blog/2014/08/25/a-peek-at-emacs-24-dot-4-prettify-symbols-mode/>
;; To check if this is a good idea or if one should rather activate it by mode
(global-prettify-symbols-mode +1)
(setq prettify-symbols-unprettify-at-point t)

(quelpa
 '(prettify-utils :fetcher url :url "https://raw.githubusercontent.com/Ilazki/prettify-utils.el/master/prettify-utils.el"))

(defun prettify-set ()
  (setq prettify-symbols-alist
		(prettify-utils-generate
		 ("lambda"	"λ")
		 ("|>"		"▷")
		 ("<|"		"◁")
		 ("%>%"         "▶")
		 ("->>"		"↠")
		 ("->"		"→")
		 ("<-"		"←")
		 ("=>"		"⇒")
		 ("<="		"≤")
		 (">="		"≥")
		 ("[ ]"         "☐")
                 ("[X]"         "☑")
                 ("[-]"         "❍")
		 ("function"    "ƒ")
		 ("%x%"         "⊗")
		 ("%*%"         "×")
		 )))

(defun gams-symbols-list ()
  (setq prettify-symbols-alist
		(prettify-utils-generate
		 ;; Math operator
		 ("sum"  "∑")
		 ("prod" "∏")
		 ("**"   "^")
		 ;; Relations in equations
		 ("=l="  "≤")
		 ("=g="  "≥")
		 ("=e="  "=")
		 ;; Logical operator
		 ("lt"   "<")
		 ("gt"   ">")
		 ("<="   "≤")
		 ("le"   "≤")
		 (">="   "≥")
		 ("ge"   "≥")
		 ("eq"   "=")
		 ("or"   "∨")
		 ("and"  "∧")
		 ("xor"  "⊻")
		 ("imp"  "⇒")
		 ("eqv"  "⇔")
		 ("not"  "¬")
		 ("ne"   "≠")
		 ("<>"   "≠")
		 ("=="   "=")
		 ;; Mathematical constant
		 ("inf"  "∞")
		 ("-inf" "-∞")
		 )))

(add-hook 'gams-mode-hook 'gams-symbols-list)
(add-hook 'prog-mode-hook 'prettify-set)
(add-hook 'gams-mode-hook 'prettify-set)
(add-hook 'inferior-ess-mode-hook 'prettify-set)

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
  (interactive)
  (let ((fill-column (point-max)))
  (fill-paragraph nil)))

(defun unfill-region (start end)
  (interactive "r")
  (let ((fill-column (point-max)))
    (fill-region start end nil)))

(setq-default fill-column 80)

;; Use visual fill column for text mode
(use-package visual-fill-column
  :init
  (setq visual-fill-column-width 100))
(add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
(add-hook 'text-mode-hook 'visual-fill-column-mode)
(add-hook 'text-mode-hook 'visual-line-mode)

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
  :bind ("C-c d" . docker))

;;; ========
;;;  Ispell
;;; ========
(use-package flyspell
  :init
  (add-hook 'text-mode-hook 'flyspell-mode)
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
	ispell-hunspell-dictionary-alist ispell-local-dictionary-alist))

(use-package flyspell-correct
  :after flyspell
  :bind (:map flyspell-mode-map
		  ("M-$" . flyspell-correct-at-point)))

(use-package flyspell-correct-ivy
  :demand t
  :after flyspell-correct)

;;; ==========
;;;  Polymode
;;; ==========
(use-package poly-R)
(use-package poly-markdown)
(use-package quarto-mode)
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;;; ========================================================
;;;  Gams - http://shirotakeda.org/en/gams/gams-mode/
;;; ========================================================
(use-package gams-mode
  :mode ("\\.gms\\'" "\\.inc\\'")
  :hook ((gams-mode . rainbow-delimiters-mode)
	 (gams-mode . smartparens-mode)
	 (gams-mode . (lambda ()
			(display-fill-column-indicator-mode))))
  :custom
  (gams-process-command-option "ll=0 lo=3 pw=153 ps=9999")
  (gams-statement-upcase t)
  (gams-fill-column 100)
  (gams-recenter-font-lock t)
  (gams-statement-name "Parameter")
  (gams-dollar-control-name "exit")
  (gams-default-pop-window-height 20)
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
  :config
  (defun find-in-gams-files (string)
    "Find a regular expression in gams files"
    (interactive "sRegular expression to find: ")
    (grep (concat "grep -nHI -i -r -e " string " --include=\*.{gms,inc} *" )))
  :bind ("\C-cf" . find-in-gams-files))

(use-package gams-ac
  :init
  (gams-ac-after-init-setup))

(if mswindows
  (progn
    (setq gams-process-command-name "C:/GAMS/Last/gams.exe"
	  gams-system-directory "C:/GAMS/Last/"
	  gams-docs-directory "C:/GAMS/Last/docs"
	  gams-docs-view-program "C:/Program Files (x86)/Foxit Software/Foxit Reader/FoxitReader.exe"
	  load-path
	  (cons "C:/GAMS/Last/" ;; Set the installed directory!
		load-path)))
  (progn
    (setq gams-docs-directory "/opt/gams/gamsLast_linux_x64_64_sfx/docs"
	  gams-docs-view-program "qpdfview")))

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

;;; ==================================================
;;;  Yaml mode - https://github.com/yoshiki/yaml-mode
;;; ==================================================
(use-package yaml-mode
  :mode ("\\.yml$" "\\.dvc" "dvc.lock")
  :bind ("\C-m" . newline-and-indent))

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
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;;; ===============
;;;  doom-modeline
;;; ===============
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode))

;;; =======
;;;  eglot
;;; =======
(use-package eglot
  :config
  (setq eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider)))  ; Prevent eglot from reformatting code automatically

;;; =======
;;;  eldoc
;;; =======
; Prevent eldoc from showing the function doc in the minibuffer when the cursor is on the function
(setq eldoc-echo-area-use-multiline-p nil)

;;; ============
;;;  format-all
;;; ============
(use-package format-all)

;;; ============
;;;  LaTeX-mode
;;; ============
(use-package tex
  :ensure auctex
  :init
  (add-hook 'TeX-mode-hook 'latex-math-mode)
  (add-hook 'TeX-mode-hook 'imenu-add-menubar-index)
  (add-hook 'TeX-mode-hook 'turn-on-reftex)
  (add-hook 'TeX-mode-hook 'TeX-fold-mode 1)
  (add-hook 'TeX-mode-hook
            #'(lambda ()
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
  (reftex-default-bibliography '("References.bib"))
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
  (reftex-plug-into-AUCTeX t)
  (TeX-electric-sub-and-superscript 1)
  (LaTeX-math-list
   '(
     (?\) "right)")
   (?\( "left(")
     (?/ "frac{}{}")
     ))
  :bind
  ("\C-ce" . TeX-next-error)
  ("\C-cf" . reftex-fancyref-fref)
  ("\C-cF" . reftex-fancyref-Fref))

;; Increase reftex speed (especially on Windows)
(setq reftex-enable-partial-scans t
      reftex-save-parse-info t
      reftex-use-multiple-selection-buffers t)

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
      ;; (setq TeX-view-program-list (quote (("Sumatra PDF" ("\"SumatraPDF.exe\" -reuse-instance" (mode-io-correlate " -forward-search %b %n") " %o")))))
      ;; (setq TeX-view-program-selection (quote ((output-pdf "Sumatra PDF")
      (setq TeX-view-program-selection (quote ((output-pdf "SumatraPDF")
					       (output-dvi "Yap")))))
    (progn
      (setq TeX-view-program-list '(("qpdfview" "qpdfview --instance emacsauxtex --unique \"%o#src:%b:%n:0\"")))
      (setq TeX-view-program-selection '((output-pdf "qpdfview")
					 (output-dvi "xdvi")))))

;; Preview
(setq preview-scale-function 1.5)      ; Higher preview images in TeX buffers
(setq preview-auto-cache-preamble t)
(if mswindows
    (setq preview-gs-command "C:\\Program Files\\gs\\gs9.56.1\\bin\\gswin64c.exe")
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
	  #'(lambda()
	     (local-set-key [(shift return)] 'tex-frame)))

(use-package cdlatex
  :init
  (add-hook 'LaTeX-mode-hook #'turn-on-cdlatex)
  )
(add-hook 'cdlatex-tab-hook
          (defun cdlatex-indent-maybe ()
            (when (or (bolp) (looking-back "^[ \t]+"))
              (LaTeX-indent-line))))

(add-hook 'LaTeX-mode-hook
      (lambda ()
        (make-local-variable 'company-idle-delay)
        (setq company-idle-delay 0.3)))

;;; =====
;;;  PDF
;;; =====
(use-package doc-view
  :if mswindows
  :config
  (setq doc-view-ghostscript-program "C:\\Program Files\\gs\\gs9.56.1\\bin\\gswin64c.exe"))

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
  ;; (setq fill-column 80) 	        ; where auto-fill should wrap
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
(setq projectile-use-git-grep t)

(use-package ripgrep)

;;; =====================================
;;;  Activation du clic droit comme aide
;;; =====================================
(define-key global-map [(mouse-3)] 'mouse-me)

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

;;; ===========
;;;  Yasnippet
;;; ===========
(use-package yasnippet)
(yas-global-mode 1)

;;; =====
;;;  ESS
;;; =====
(use-package ess
  :bind (;; Shortcut for pipe |>
	 :map ess-r-mode-map
         ("C-S-m" . " |>")
         :map inferior-ess-r-mode-map
         ("C-S-m" . " |>")
	 ;; Shortcut for pipe %>%
	 :map ess-r-mode-map
	 ("C-%" . " %>%")
         :map inferior-ess-r-mode-map
         ("C-%" . " %>%")
	 ;; Shortcut for assign <-
	 :map ess-r-mode-map
	 ("M--" . ess-insert-assign)
         :map inferior-ess-r-mode-map
	 ("M--" . ess-insert-assign))
  :config
  (setq ess-assign-list '(" <-" " <<- " " = " " -> " " ->> ")
	ess-style 'RStudio  ; Set code indentation
	ess-ask-for-ess-directory nil  ; Do not ask what is the project directory
	comint-scroll-to-bottom-on-input 'this
	comint-scroll-to-bottom-on-output t
	comint-move-point-for-output t
	 ess-use-flymake nil)  ; Deactivate linter because it does not seem to work
  ;; Following the "source is real" philosophy put forward by ESS, one should
  ;; not need the command history and should not save the workspace at the end
  ;; of an R session. Hence, both options are disabled here.
  (setq-default inferior-R-args "--no-restore-history --no-save ")
  :init
  (add-hook 'ess-mode-hook 'hexcolour-add-to-font-lock)
  ;; Add a vertical line at 80 columns
  (add-hook 'ess-mode-hook (lambda ()
			     (display-fill-column-indicator-mode)))
  )

(define-key inferior-ess-mode-map [home] 'comint-bol)
; Call imenu with \C-c =
(define-key ess-mode-map "\C-c=" 'imenu)

(use-package rutils) ; To interact easily with renv

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

(defun find-in-R-files (string)
  "Find a regular expression in R files"
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

;; Fix to a bug in iess (see: https://github.com/emacs-ess/ESS/issues/1193)
;; To check with new emacs/ess version if still relevant
(defun my-inferior-ess-init ()
  (setq-local ansi-color-for-comint-mode 'filter)
  (smartparens-mode 1))
(add-hook 'inferior-ess-mode-hook 'my-inferior-ess-init)

;;; ===================================
;;;  Définition de touches perso global
;;; ===================================
(define-key global-map [(f5)] 'revert-buffer)

;;; ===============
;;;  Markdown mode
;;; ===============
(use-package markdown-mode
  :mode ("README\\.md\\'" . gfm-mode)
  :init
  (setq markdown-command "pandoc"
	markdown-enable-math t))
(use-package pandoc-mode)
;; (autoload 'markdown-mode "markdown-mode"
;;   "Major mode for editing Markdown files" t)
(add-hook 'markdown-mode-hook 'pandoc-mode)
(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; Code to use RefTeX to input references in markdown
;; from <https://gist.github.com/kleinschmidt/5ab0d3c423a7ee013a2c01b3919b009a>

;; define markdown citation formats
(defvar markdown-cite-format)
(setq markdown-cite-format
      '(
        (?\C-m . "[@%l]")
        (?p . "[@%l]")
        (?t . "@%l")
        )
      )

;; wrap reftex-citation with local variables for markdown format
(defun markdown-reftex-citation ()
  (interactive)
  (let ((reftex-cite-format markdown-cite-format)
        (reftex-cite-key-separator "; @"))
    (reftex-citation)))

;; bind modified reftex-citation to C-c[, without enabling reftex-mode
;; https://www.gnu.org/software/auctex/manual/reftex/Citations-Outside-LaTeX.html#SEC31
(add-hook
 'markdown-mode-hook
 (lambda ()
   (define-key markdown-mode-map "\C-c[" 'markdown-reftex-citation)))

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

;;; ========
;;;  ccrypt
;;; ========
(use-package ps-ccrypt
  :quelpa (ps-ccrypt :fetcher git :url "https://github.com/isdamir/ps-ccrypt.git"))

;;; =================
;;;  ivy and friends
;;; =================
(use-package ivy
  :demand
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "%d/%d ")
  :config (ivy-mode))

(use-package swiper)

;; swiper is slow for large files so it is replaced by isearch for large files
(defun search-method-according-to-numlines ()
  "Determines the number of lines of current buffer and chooses a search method accordingly"
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

;;; =======
;;;  Julia
;;; =======
(use-package julia-mode)

;; (use-package julia-repl)  ; Does not work on Linux
;; (add-hook 'julia-mode-hook 'julia-repl-mode)

;;; ==================
;;;  View Large Files
;;; ==================
(use-package vlf)

;;; =======
;;;  Tramp
;;; =======
(if mswindows
    (setq tramp-default-method "plink"))

;; ;;; init.el ends here
