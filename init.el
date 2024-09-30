;; Create a variable to indicate where emacs's configuration is installed
(setq EMACS_DIR "~/.emacs.d/")

;; Avoid garbage collection at statup
(setq gc-cons-threshold most-positive-fixnum ; 2^61 bytes
      gc-cons-percentage 0.6)

;; All the settings and package installation is set in configuration.org
(org-babel-load-file "~/.emacs.d/emacs-configuration.org")

(envrc-global-mode)

(add-hook 'emacs-startup-hook
  (lambda ()
    (setq gc-cons-threshold 300000000 ; 300mb	
          gc-cons-percentage 0.1)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(doom-solarized-light))
 '(custom-safe-themes
   '("9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
	 "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
	 "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
	 "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
	 "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
	 "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
	 "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43"
	 default))
 '(emms-browser-covers '(emms-browser-cache-thumbnail))
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
	 "-XX:AdaptiveSizePolicyWeight=90"
	 "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m"
	 "--enable-preview"))
 '(package-selected-packages
   '(ac-geiser all-the-icons-dired auto-complete-auctex ccls cdlatex
			   company-auctex dashboard docker dockerfile-mode
			   doom-modeline doom-themes elcord envrc
			   eshell-syntax-highlighting eshell-vterm flycheck-rust
			   gdscript-mode geiser-guile heaven-and-hell
			   helm-descbinds helm-lsp helm-swoop helm-tramp hl-todo
			   jsdoc json-mode ligature lsp-java lsp-ui markless
			   multiple-cursors nix-mode ob-typescript org-modern
			   org-roam page-break-lines php-mode platformio-mode
			   prettier quickrun rainbow-delimiters rust-mode
			   skewer-reload-stylesheets sly smart-compile
			   solo-jazz-theme svelte-mode treemacs-all-the-icons
			   treemacs-magit treemacs-projectile undo-tree
			   use-package-chords yaml-mode yasnippet-snippets)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font-10")
(set-frame-font "CaskaydiaCove Nerd Font-10" nil t)



