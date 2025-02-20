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
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
	 "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
	 "2721b06afaf1769ef63f942bf3e977f208f517b187f2526f0e57c1bd4a000350"
	 "da75eceab6bea9298e04ce5b4b07349f8c02da305734f7c0c8c6af7b5eaa9738"
	 "9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392"
	 "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc"
	 "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e"
	 "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1"
	 "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d"
	 "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874"
	 "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43"
	 default))
 '(emms-browser-covers '(emms-browser-cache-thumbnail))
 '(lsp-ada-als-executable "steam-run ada_language_server")
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
	 "-XX:AdaptiveSizePolicyWeight=90"
	 "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m"
	 "--enable-preview"))
 '(package-selected-packages
   '(ac-geiser ada-mode all-the-icons-dired arduino-mode
			   auto-complete-auctex cargo-mode cargo-transient ccls
			   cdlatex cider company-auctex dashboard doom-modeline
			   doom-themes elcord ellama envrc
			   eshell-syntax-highlighting eshell-vterm flycheck-rust
			   gdscript-mode geiser-guile heaven-and-hell
			   helm-descbinds helm-lsp helm-swoop helm-tramp hl-todo
			   jsdoc json-mode kotlin-mode ligature lsp-java
			   lsp-tailwindcss lsp-ui magit-todos markless meson-mode
			   multiple-cursors nix-mode nov ob-typescript org-modern
			   org-roam page-break-lines paredit php-mode
			   platformio-mode prettier quickrun rainbow-delimiters
			   rust-mode scss-mode skewer-reload-stylesheets sly
			   smart-compile solo-jazz-theme svelte-mode
			   treemacs-all-the-icons treemacs-magit
			   treemacs-projectile twig-mode typescript-mode undo-tree
			   use-package-chords web-mode yaml-mode
			   yasnippet-snippets))
 '(safe-local-variable-directories
   '("/home/ivvil/Documentos/DAW/Desarrollo web entorno servidor I/laravel_base/"
	 "/home/ivvil/Proyectos/milftp/"
	 "/home/ivvil/Documentos/DAW/Desarrollo web entorno servidor I/prueba/"
	 "/home/ivvil/Proyectos/klim/"))
 '(safe-local-variable-values
   '((web-mode-indent-style . 2) (web-mode-block-padding . 4)
	 (web-mode-script-padding . 4) (web-mode-style-padding . 4))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font-11")
(set-frame-font "CaskaydiaCove Nerd Font-11" nil t)



