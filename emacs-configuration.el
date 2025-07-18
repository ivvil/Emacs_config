(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
						   ("elpa" . "https://elpa.gnu.org/packages/")
						   ("org" . "https://orgmode.org/elpa/")
						   ("nongnu" . "https://elpa.nongnu.org/nongnu/")
						   ))

(package-initialize)
										  ; Fetch the list of packages available 
(unless package-archive-contents (package-refresh-contents))

										  ; Install use-package
(setq package-list '(use-package))
(dolist (package package-list)
	(unless (package-installed-p package) (package-install package)))

;; (use-package exec-path-from-shell :ensure t)
;; (exec-path-from-shell-initialize)

;; Load platform specific variables using specific files. E.g. linux.el. 
;; Make necessary changes as needed
(cond ((eq system-type 'windows-nt) (load (concat EMACS_DIR "windows")))
((eq system-type 'gnu/linux) (load (concat EMACS_DIR "linux")))
((eq system-type 'darwin) (load (concat EMACS_DIR "mac")))
(t (load-library "default")))

;; Disable annoying ring-bell when backspace key is pressed in certain situations
(setq ring-bell-function 'ignore)

;; Disable scrollbar and toolbar
(scroll-bar-mode -1)
(tool-bar-mode -1)

;; Set language environment to UTF-8
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;; Longer whitespace, otherwise syntax highlighting is limited to default column
(setq whitespace-line-column 1000) 

;; Enable soft-wrap
(global-visual-line-mode 1)

;; Maintain a list of recent files opened
(recentf-mode 1)            
(setq recentf-max-saved-items 50)

;; Move all the backup files to specific cache directory
;; This way you won't have annoying temporary files starting with ~(tilde) in each directory
;; Following setting will move temporary files to specific folders inside cache directory in EMACS_DIR

(setq user-cache-directory (concat EMACS_DIR "cache"))
(setq backup-directory-alist `(("." . ,(expand-file-name "backups" user-cache-directory)))
		url-history-file (expand-file-name "url/history" user-cache-directory)
		auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-cache-directory)
		projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-cache-directory))

;; Org-mode issue with src block not expanding
;; This is a fix for bug in org-mode where <s TAB does not expand SRC block
(when (version<= "9.2" (org-version))
	(require 'org-tempo))

;; Coding specific setting

;; Automatically add ending brackets and braces
(electric-pair-mode 1)

;; Make sure tab-width is 4 and not 8
(setq-default tab-width 4)

;; Highlight matching brackets and braces
(show-paren-mode 1)

;; Disable default startup buffer
(setq inhibit-startup-screen t)

;; Enable disabled comands
(setq disabled-command-function nil)

;; Add other elisp files to ~load-path~
(defun add-subdirectories-to-load-path (directory)
	"Add subdirectories of DIRECTORY to the `load-path`."
	(interactive "Directory: ")
	(let ((default-directory (file-name-as-directory directory)))
	  (dolist (subdir (directory-files directory t "^[^.]" 'nosort))
		(when (file-directory-p subdir)
		  (add-to-list 'load-path subdir)))))

(add-subdirectories-to-load-path "~/.emacs.d/elisp")

;; Highlight todos
(use-package hl-todo
	:ensure t
	:init (global-hl-todo-mode))

;; use-package with package.el:
(use-package dashboard
	:ensure t
	:config
	(dashboard-setup-startup-hook)
	(setq dashboard-projects-backend 'projectile))
(setq dashboard-display-icons-p t) ;; display icons on both GUI and terminal
;; (setq dashboard-icon-type 'nerd-icons) ;; use `nerd-icons' package
(setq dashboard-icon-type 'all-the-icons) ;; use `all-the-icons' package
;; (setq dashboard-set-heading-icons t)
;; (setq dashboard-set-file-icons t)

(setq dashboard-startup-banner 'logo)
;; Value can be
;; - nil to display no banner
;; - 'official which displays the official emacs logo
;; - 'logo which displays an alternative emacs logo
;; - 1, 2 or 3 which displays one of the text banners
;; - "path/to/your/image.gif", "path/to/your/image.png", "path/to/your/text.txt" or "path/to/your/image.xbm" which displays whatever gif/image/text/xbm you would prefer
;; - a cons of '("path/to/your/image.png" . "path/to/your/text.txt")

(setq dashboard-items '((recents  . 5)
						  (bookmarks . 5)
						  (projects . 5)
						  (agenda . 5)
						  (registers . 5)))

(setq dashboard-startupify-list '(dashboard-insert-banner
									dashboard-insert-newline
									dashboard-insert-banner-title
									dashboard-insert-newline
									dashboard-insert-navigator
									dashboard-insert-newline
									dashboard-insert-init-info
									dashboard-insert-items
									dashboard-insert-newline
									dashboard-insert-footer))

(use-package page-break-lines :ensure t)

(use-package all-the-icons
	:if (display-graphic-p) :ensure t)
(use-package all-the-icons-dired :ensure t)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; (use-package nerd-icons
;;   ;; :custom
;;   ;; The Nerd Font you want to use in GUI
;;   ;; "Symbols Nerd Font Mono" is the default and is recommended
;;   ;; but you can use any other Nerd Font if you want
;;   ;; (nerd-icons-font-family "Symbols Nerd Font Mono")
;;   :ensure t
;;   )

;; (use-package s
;;   :ensure t)
;; (require 'which-linux-distribution)

;; (set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font-10")
;; (set-frame-font "CaskaydiaCove Nerd Font-10" nil t)
(setq default-frame-alist '((font . "Jetbrains Mono Semibold-12")
							(width . 80)
							(height. 24)))

(use-package doom-themes
  :ensure t 
  :init 
  (load-theme 'doom-palenight t))

(use-package solo-jazz-theme
  :ensure t)

(use-package acme-theme
  :ensure t)

(use-package heaven-and-hell
  :ensure t
  :init
  (setq heaven-and-hell-theme-type 'dark)
  (setq heaven-and-hell-themes
		'((light . acme)
		  (dark . doom-gruvbox)))
  :hook (after-init . heaven-and-hell-init-hook)
  :bind (("C-c <f6>" . heaven-and-hell-load-default-theme)
		 ("<f6>" . heaven-and-hell-toggle-theme)))

(defun my/ansi-colorize-buffer ()
(let ((buffer-read-only nil))
(ansi-color-apply-on-region (point-min) (point-max))))

(use-package ansi-color
:ensure t
:config
(add-hook 'compilation-filter-hook 'my/ansi-colorize-buffer)
)

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("--" "---" "==" "===" "!=" "!==" "=!="
                              "=:=" "=/=" "<=" ">=" "&&" "&&&" "&=" "++" "+++" "***" ";;" "!!"
                              "??" "???" "?:" "?." "?=" "<:" ":<" ":>" ">:" "<:<" "<>" "<<<" ">>>"
                              "<<" ">>" "||" "-|" "_|_" "|-" "||-" "|=" "||=" "##" "###" "####"
                              "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#=" "^=" "<$>" "<$"
                              "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</" "</>" "/>" "<!--" "<#--"
                              "-->" "->" "->>" "<<-" "<-" "<=<" "=<<" "<<=" "<==" "<=>" "<==>"
                              "==>" "=>" "=>>" ">=>" ">>=" ">>-" ">-" "-<" "-<<" ">->" "<-<" "<-|"
                              "<=|" "|=>" "|->" "<->" "<~~" "<~" "<~>" "~~" "~~>" "~>" "~-" "-~"
                              "~@" "[||]" "|]" "[|" "|}" "{|" "[<" ">]" "|>" "<|" "||>" "<||"
                              "|||>" "<|||" "<|>" "..." ".." ".=" "..<" ".?" "::" ":::" ":=" "::="
                              ":?" ":?>" "//" "///" "/*" "*/" "/=" "//=" "/==" "@_" "__" "???"
                              "<:<" ";;;"))
  (global-ligature-mode t))

(use-package doom-modeline
:ensure t
:init (doom-modeline-mode))

(use-package rainbow-delimiters
	:ensure t
	:hook (prog-mode . rainbow-delimiters-mode))

(use-package page-break-lines
	:ensure t
	:init (global-page-break-lines-mode))

;; (use-package solaire-mode
;;   :ensure t
;;   :init (solaire-global-mode +1))

(use-package use-package-chords
	:ensure t
	:init 
	:config (key-chord-mode 1)
	(setq key-chord-two-keys-delay 0.4)
	(setq key-chord-one-key-delay 0.5) ; default 0.2
	)

(use-package projectile 
:ensure t
:init (projectile-mode +1)
:config 
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)

(use-package helm
:ensure t
:init 
(helm-mode 1)
(progn (setq helm-buffers-fuzzy-matching t))
:bind
(("C-c h" . helm-command-prefix))
(("M-x" . helm-M-x))
(("C-x C-f" . helm-find-files))
(("C-x b" . helm-buffers-list))
(("C-c b" . helm-bookmarks))
(("C-c f" . helm-recentf))   ;; Add new key to recentf
(("C-c g" . helm-grep-do-git-grep)))  ;; Search using grep in a git project

(use-package helm-descbinds
:ensure t
:bind ("C-h b" . helm-descbinds))

(use-package helm-swoop 
:ensure t
:chords
("js" . helm-swoop)
("jp" . helm-swoop-back-to-last-point)
:init
(bind-key "M-m" 'helm-swoop-from-isearch isearch-mode-map)

;; If you prefer fuzzy matching
(setq helm-swoop-use-fuzzy-match t)

;; Save buffer when helm-multi-swoop-edit complete
(setq helm-multi-swoop-edit-save t)

;; If this value is t, split window inside the current window
(setq helm-swoop-split-with-multiple-windows nil)

;; Split direction. 'split-window-vertically or 'split-window-horizontally
(setq helm-swoop-split-direction 'split-window-vertically)

;; If nil, you can slightly boost invoke speed in exchange for text color
(setq helm-swoop-speed-or-color nil)

;; ;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

)

(use-package helm-tramp :ensure t)

(use-package avy 
:ensure t
:chords
("jc" . avy-goto-char)
("jw" . avy-goto-word-1)
("jl" . avy-goto-line))

(use-package which-key 
:ensure t 
:init
(which-key-mode)
)

(use-package quickrun 
:ensure t
:bind ("C-c r" . quickrun))

(use-package magit :ensure t)
(use-package magit-todos
  :ensure t
  :after magit
  :config (magit-todos-mode 1))


(setq auth-sources '("~/.authinfo"))


(use-package forge
  :ensure t
  :after magit)
(setenv "TERM" "dumb")

(use-package envrc :ensure t
	:init (envrc-mode))

(use-package vterm :ensure t)
(use-package eshell-vterm :ensure t)

;; (use-package emms
;;   :config
;;   (require 'emms-player-mpd)
;;   (require 'emms-mpris)
;;   (emms-all)
;;   (setq emms-player-list emms-player-mpd)
;;   (setq emms-info-functions 'emms-info-mpd)
;;   (setq emms-change-volume-function 'emms-volume-mpd-change)
;;   (fset emms-browser-covers 'emms-browser-cache-thumbnail)
;;   (add-to-list 'emms-player-list 'emms-player-mpd))

;; (emms-all)
;; (emms-default-players)
;; (emms-mode-line 1)

;; (use-package emms-browser
;;   :config
;;   (setq emms-browser-covers 'emms-browser-cache-thumbnail)
;;   (setq emms-browser-covers-for-first-column 'emms-browser-cache-thumbnail))

;; (use-package emms-player-mpd
;;   :config
;;   (setq emms-player-list '(emms-player-mpd))
;;   (setq emms-info-functions '(emms-info-mpd))
;;   (setq emms-change-volume-function 'emms-volume-mpd-change)
;;   (add-to-list 'emms-player-list 'emms-player-mpd))

(use-package elcord :ensure t)
(elcord-mode)

;; (when (s-contains? "NixOS" (which-linux-distribution)) (fset 'bitlbee-command-line
;; 							     (lambda ()
;; 							   ((concat bitlbee-executable " " bitlbee-options " -d " bitlbee-user-directory)))))
;; (use-package bitlbee :ensure t)

(use-package undo-tree :ensure t :init (global-undo-tree-mode))

(use-package skewer-mode :ensure t)
(use-package skewer-reload-stylesheets :ensure t)

(use-package smart-compile :ensure t)

(use-package prettier :ensure t)

;; (use-package tree-sitter :ensure t)
;; (use-package tree-sitter-langs
;;   :ensure t
;;   :after tree-sitter)

;; (setq treesit-language-source-alist
;; 	  '((astro "https://github.com/virchau13/tree-sitter-astro")
;; 		(css "https://github.com/tree-sitter/tree-sitter-css")
;; 		(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

;; (use-package treesit-auto
;;   :ensure t
;;   :config
;;   (global-treesit-auto-mode))

;; ;; (let ((astro-recipe (make-treesit-auto-recipe
;; ;; 					 :lang 'astro
;; ;; 					 :ts-mode 'astro-ts-mode
;; ;; 					 :url "https://github.com/virchau13/tree-sitter-astro"
;; ;; 					 :revision "master"
;; ;; 					 :source-dir "src")))
;; ;;   (add-to-list 'treesit-auto-recipe-list astro-recipe))

;; (use-package doom-modeline
;;       :ensure t
;;       :init (doom-modeline-mode 1))

;; (use-package minions
;; :ensure t
;; :init (minions-mode))

(use-package org-modern
	:ensure t
	:init (global-org-modern-mode))

(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))
(setq org-latex-listings 'minted)

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

(use-package cdlatex
  :ensure t
  :hook (org-mode-hook . turn-on-org-cdlatex))

;; (require SQLplus)
(setq sqlplus-connect-string "sys/test@//localhost:1521/xe as sysdba")

(use-package multiple-cursors
	:ensure t
	:bind (("C-S-c C-S-c" . mc/edit-lines)
		   ("C->" . mc/mark-next-like-this)
		   ("C-<" . mc/mark-previous-like-this)
		   ("C-c C-<" . mc/mark-all-like-this)))

(use-package platformio-mode
	:ensure t)

(use-package org-roam
	:ensure t
	:custom
	(org-roam-directory (file-truename "~/Documentos/Notas"))
	:bind (("C-c n l" . org-roam-buffer-toggle)
		   ("C-c n f" . org-roam-node-find)
		   ("C-c n g" . org-roam-graph)
		   ("C-c n i" . org-roam-node-insert)
		   ("C-c n c" . org-roam-capture)
		   ;; Dailies
		   ("C-c n j" . org-roam-dailies-capture-today))
	:config
	;; If you're using a vertical completion framework, you might want a more informative completion interface
	(setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
	(org-roam-db-autosync-mode)
	;; If using org-roam-protocol
	(require 'org-roam-protocol))

(use-package eshell-syntax-highlighting
  :after eshell-mode
  :ensure t ;; Install if not already installed.
  :config
  ;; Enable in all Eshell buffers.
  (eshell-syntax-highlighting-global-mode))

(use-package ellama
  :ensure t
  :init
  (setopt ellama-language "English")
  (require 'llm-ollama)
  (setopt ellama-provider
		  (make-llm-ollama
		   :chat-model "mistral-small" :embedding-model "mistral-small")))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package company :ensure t :init (global-company-mode))

(use-package yasnippet :config (yas-global-mode))
(use-package yasnippet-snippets :ensure t)

(use-package flycheck :ensure t :init (global-flycheck-mode))
(setq ispell-default-dicctionary "es")

(use-package dap-mode
  :ensure t
  :after (lsp-mode)
  :functions dap-hydra/nil
  :config
  (require 'dap-java)
  :bind (:map lsp-mode-map
			  ("<f5>" . dap-debug)
			  ("M-<f5>" . dap-hydra))
  :hook ((dap-mode . dap-ui-mode)
		 (dap-session-created . (lambda (&_rest) (dap-hydra)))
		 (dap-terminated . (lambda (&_rest) (dap-hydra/nil)))))

(use-package dap-java :ensure nil)
(require 'dap-gdb)

(with-eval-after-load 'lsp-rust
  (require 'dap-cpptools))

(with-eval-after-load 'dap-cpptools
  ;; Add a template specific for debugging Rust programs.
  ;; It is used for new projects, where I can M-x dap-edit-debug-template
  (dap-register-debug-template "Rust::CppTools Run Configuration"
                               (list :type "cppdbg"
                                     :request "launch"
                                     :name "Rust::Run"
                                     :MIMode "gdb"
                                     :miDebuggerPath "rust-gdb"
                                     :environment []
                                     :program "${workspaceFolder}/target/debug/hello / replace with binary"
                                     :cwd "${workspaceFolder}"
                                     :console "external"
                                     :dap-compilation "cargo build"
                                     :dap-compilation-dir "${workspaceFolder}")))

(with-eval-after-load 'dap-mode
  (setq dap-default-terminal-kind "integrated") ;; Make sure that terminal programs open a term for I/O in an Emacs buffer
  (dap-auto-configure-mode +1))

(use-package lsp-treemacs
	:after (lsp-mode treemacs)
	:ensure t
	:commands lsp-treemacs-errors-list
	:init (lsp-treemacs-sync-mode 1)
	:bind (:map lsp-mode-map
		   ("M-9" . lsp-treemacs-errors-list)))
(use-package treemacs-projectile
	:ensure t
	:after (treemacs))

(use-package treemacs-magit
	:ensure t
	:after (treemacs))

(use-package treemacs-all-the-icons
	:ensure t
	:after (treemacs))

(use-package treemacs
	:ensure t
	:commands (treemacs)
	:after (lsp-mode))

(use-package lsp-ui
:ensure t
:after (lsp-mode)
:bind (:map lsp-ui-mode-map
         ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
         ([remap xref-find-references] . lsp-ui-peek-find-references))
:init (setq lsp-ui-doc-delay 1.5
      lsp-ui-doc-position 'bottom
	  lsp-ui-doc-max-width 100
))

(use-package helm-lsp
:ensure t
:after (lsp-mode)
:commands (helm-lsp-workspace-symbol)
:init (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol))

(use-package lsp-mode
:ensure t
:hook (
	 (lsp-mode . lsp-enable-which-key-integration)
	 (java-mode . #'lsp-deferred)
	 (latex-mode . #'lsp-deferred)
	 (gdscript-mode . #'lsp-deferred)
	 (css-mode . #'lsp-deferred)
)
:init (setq 
	  lsp-keymap-prefix "C-c l"              ; this is for which-key integration documentation, need to use lsp-mode-map
	  lsp-enable-file-watchers nil
	  read-process-output-max (* 1024 1024)  ; 1 mb
	  lsp-completion-provider :capf
	  lsp-idle-delay 0.500
)
:config 
	  (setq lsp-intelephense-multi-root nil) ; don't scan unnecessary projects
	  (with-eval-after-load 'lsp-intelephense
	  (setf (lsp--client-multi-root (gethash 'iph lsp-clients)) nil))
	  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
)

(setq dap-auto-configure-features '(sessions locals controls tooltip))

(use-package lsp-java 
:ensure t
:config (add-hook 'java-mode-hook 'lsp))
;; (setq lsp-java-workspace-dir "/home/ivvil/Documentos/Eclipse")
(require 'dap-java)

(use-package treesit
      :mode (("\\.tsx\\'" . tsx-ts-mode)
             ("\\.js\\'"  . typescript-ts-mode)
             ("\\.mjs\\'" . typescript-ts-mode)
             ("\\.mts\\'" . typescript-ts-mode)
             ("\\.cjs\\'" . typescript-ts-mode)
             ("\\.ts\\'"  . typescript-ts-mode)
             ("\\.jsx\\'" . tsx-ts-mode)
             ("\\.json\\'" .  json-ts-mode)
             ("\\.Dockerfile\\'" . dockerfile-ts-mode)
             ("\\.prisma\\'" . prisma-ts-mode)
             ;; More modes defined here...
             )
      :preface
      (defun os/setup-install-grammars ()
        "Install Tree-sitter grammars if they are absent."
        (interactive)
        (dolist (grammar
                 '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                   (bash "https://github.com/tree-sitter/tree-sitter-bash")
                   (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                   (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                   (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                   (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                   (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                   (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                   (make "https://github.com/alemuller/tree-sitter-make")
                   (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                   (cmake "https://github.com/uyha/tree-sitter-cmake")
                   (c "https://github.com/tree-sitter/tree-sitter-c")
                   (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                   (toml "https://github.com/tree-sitter/tree-sitter-toml")
                   (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                   (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                   (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                   (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
          (add-to-list 'treesit-language-source-alist grammar)
          ;; Only install `grammar' if we don't already have it
          ;; installed. However, if you want to *update* a grammar then
          ;; this obviously prevents that from happening.
          (unless (treesit-language-available-p (car grammar))
            (treesit-install-language-grammar (car grammar)))))

      ;; Optional, but recommended. Tree-sitter enabled major modes are
      ;; distinct from their ordinary counterparts.
      ;;
      ;; You can remap major modes with `major-mode-remap-alist'. Note
      ;; that this does *not* extend to hooks! Make sure you migrate them
      ;; also
      (dolist (mapping
               '((python-mode . python-ts-mode)
                 (css-mode . css-ts-mode)
                 (typescript-mode . typescript-ts-mode)
                 (js-mode . typescript-ts-mode)
                 (js2-mode . typescript-ts-mode)
                 (c-mode . c-ts-mode)
                 (c++-mode . c++-ts-mode)
                 (c-or-c++-mode . c-or-c++-ts-mode)
                 (bash-mode . bash-ts-mode)
                 (css-mode . css-ts-mode)
                 (json-mode . json-ts-mode)
                 (js-json-mode . json-ts-mode)
                 (sh-mode . bash-ts-mode)
                 (sh-base-mode . bash-ts-mode)))
        (add-to-list 'major-mode-remap-alist mapping))
      :config
      (os/setup-install-grammars))

(use-package ada-mode
  :ensure t)

(defgroup project-build nil
  "LSP options for Project"
  :group 'ada-mode)

(defcustom project-build-type "Debug"
  "Controls the type of build of a project.
   Default is Debug, other choices are Release and Coverage."
  :type '(choice
          (const "Debug")
          (const "Coverage")
          (const "Release"))
  :group 'project-build)

(add-hook 'html-mode-hook 'lsp)

(use-package svelte-mode :ensure t)

(use-package web-mode
  :ensure t)

;; ASTRO
(define-derived-mode astro-mode web-mode "astro")
(setq auto-mode-alist
      (append '((".*\\.astro\\'" . astro-mode))
              auto-mode-alist))

(setq web-mode-enable-engine-detection t)

(add-hook 'js-mode-hook 'lsp)
(add-hook 'js-ts-mode-hook 'lsp)

(add-to-list 'major-mode-remap-alist '(js-mode . js-ts-mode))

(use-package typescript-mode :ensure t)

(require 'ob-js)

(add-to-list 'org-babel-load-languages '(js . t))
(org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages)
(add-to-list 'org-babel-tangle-lang-exts '("js" . "js"))

(use-package jsdoc
	:ensure t)

(use-package ob-typescript
	:ensure t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((typescript . t)
	 ))

;; (setq org-babel-command:typescript "npx -p typescript -- tsc")

(use-package json-mode :ensure t)

;; (add-hook 'scss-mode-hook
;; 			(lambda () ((skewer-reload-stylesheets-reload-on-save)(skewer-reload-stylesheets-start-editing))))

(use-package sly :ensure t)
(setq inferior-lisp-program "sbcl")
(add-hook 'common-lisp-hook 'company-mode)

(load "~/quicklisp/log4sly-setup.el")
(global-log4sly-mode 1)

(use-package paredit
  :ensure t)

(use-package sly-quicklisp
  :ensure t)

(add-hook 'lisp-mode-hook
  		  (lambda ()
  			(paredit-mode t)
			(sly-quicklisp-mode)
  			(turn-on-eldoc-mode)
  			(eldoc-add-command
  			 'paredit-backward-delete
  			 'paredit-close-round)
  			(local-set-key (kbd "RET") 'electrify-return-if-match)
  			(eldoc-add-command 'electrify-return-if-match)
  			(show-paren-mode t)))

(use-package geiser-guile :ensure t)
(use-package ac-geiser :ensure t)

(use-package cider
  :ensure t)

;; (add-hook 'clojure-mode-hook
;; 		  (lambda ()
;; 			(lsp)
;; 			(cider)))

(use-package auctex :ensure t :hook (latex-mode . acutex-mode))
(use-package company-auctex :ensure t)
(use-package auto-complete-auctex :ensure t)
(setq-default TeX-engine 'xetex)

(use-package markless
	:ensure t)

(use-package nix-mode :ensure t :hook (nix-mode . lsp-deferred))
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "nixd")
                    :major-modes '(nix-mode)
                    :priority 0
                    :server-id 'nixd)))
(setq lsp-nix-nixd-formatting-command ["alejandra"])

(use-package cargo-mode
  :ensure t)

(use-package cargo-transient
  :ensure t)

(use-package rust-mode
  :ensure t
  :hook ((rust-mode . cargo-minor-mode)
		 (rust-mode . lsp)))
(use-package flycheck-rust
  :ensure t)

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(use-package gdscript-mode :ensure t)

(use-package ccls
  ;; :hook ((c-mode c++-mode objc-mode cuda-mode) .
  ;;        (lambda () (require 'ccls) (lsp)))
  )

(setq lsp-disabled-clients '(semgrep-ls))

(require 'dap-lldb)
(require 'dap-cpptools)
(require 'dap-gdb-lldb)

(add-hook 'csharp-mode-hook 'lsp)

(use-package php-mode
  :ensure t
  :hook (php-mode . lsp-deferred))
;;(setq lsp-enabled-clients '(phpactor))

(use-package kotlin-mode
  :ensure t
  :hook (kotlin-mode . lsp-deferred))

(use-package lsp-python-ms
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))  ; or lsp-deferred

;;  ;;;; Code Completion

;; (use-package corfu
;;   ;; Optional customizations
;;   :custom
;;   (corfu-cycle t)                 ; Allows cycling through candidates
;;   (corfu-auto t)                  ; Enable auto completion
;;   (corfu-auto-prefix 2)
;;   (corfu-auto-delay 0.3)
;;   (corfu-popupinfo-delay '(0.5 . 0.2))
;;   (corfu-preview-current 'insert) ; insert previewed candidate
;;   (corfu-preselect 'prompt)
;;   (corfu-on-exact-match nil)      ; Don't auto expand tempel snippets

;;   ;; Optionally use TAB for cycling, default is `corfu-complete'.
;;   :bind (:map corfu-map
;; 			  ("M-SPC"      . corfu-insert-separator)
;; 			  ("TAB"        . corfu-next)
;; 			  ([tab]        . corfu-next)
;; 			  ("S-TAB"      . corfu-previous)
;; 			  ([backtab]    . corfu-previous)
;; 			  ("S-<return>" . corfu-insert)
;; 			  ("RET"        . nil))

;;   :init
;;   (global-corfu-mode)
;;   (corfu-history-mode)
;;   (corfu-popupinfo-mode) ; Popup completion info
;;   :config
;;   (add-hook 'eshell-mode-hook
;; 			(lambda () (setq-local corfu-quit-at-boundary t
;; 								   corfu-quit-no-match t
;; 								   corfu-auto nil)
;; 			  (corfu-mode))))

;; (use-package eaf
;;   :load-path "~/.emacs.d/site-lisp/emacs-application-framework"
;;   :custom
;; 										; See https://github.com/emacs-eaf/emacs-application-framework/wiki/Customization
;;   (eaf-browser-continue-where-left-off t)
;;   (eaf-browser-enable-adblocker t)
;;   (browse-url-browser-function 'eaf-open-browser)
;;   :config
;;   (defalias 'browse-web #'eaf-open-browser)
;;   (eaf-bind-key scroll_up "C-n" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key scroll_down "C-p" eaf-pdf-viewer-keybinding)
;;   (eaf-bind-key take_photo "p" eaf-camera-keybinding)
;;   (eaf-bind-key nil "M-q" eaf-browser-keybinding)) ;; unbind, see more in the Wiki

;; (require 'eaf-browser)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-music-player)
;; (require 'eaf-video-player)
;; (require 'eaf-js-video-player)
;; (require 'eaf-image-viewer)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-org-previewer)
;; (require 'eaf-camera)
;; (require 'eaf-git)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-netease-cloud-music)
;; (require 'eaf-system-monitor)
;; (require 'eaf-file-browser)
;; (require 'eaf-file-sender)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-2048)
;; (require 'eaf-markmap)
;; (require 'eaf-map)
;; (require 'eaf-demo)
;; (require 'eaf-vue-demo)
;; (require 'eaf-vue-tailwindcss)
;; (require 'eaf-pyqterminal)
