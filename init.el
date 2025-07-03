;; -*- lexical-binding: t; -*-
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
   '("0f1341c0096825b1e5d8f2ed90996025a0d013a0978677956a9e61408fcd2c77"
	 "f1e8339b04aef8f145dd4782d03499d9d716fdc0361319411ac2efc603249326"
	 "b7a09eb77a1e9b98cafba8ef1bd58871f91958538f6671b22976ea38c2580755"
	 "691d671429fa6c6d73098fc6ff05d4a14a323ea0a18787daeb93fde0e48ab18b"
	 "77fff78cc13a2ff41ad0a8ba2f09e8efd3c7e16be20725606c095f9a19c24d3d"
	 "2614a89f7e54cd9512343a3efba0e084fb9568c38e11165d7109a887e924a970"
	 "48042425e84cd92184837e01d0b4fe9f912d875c43021c3bcb7eeb51f1be5710"
	 "7c28419e963b04bf7ad14f3d8f6655c078de75e4944843ef9522dbecfcd8717d"
	 "34cf3305b35e3a8132a0b1bdf2c67623bc2cb05b125f8d7d26bd51fd16d547ec"
	 "571661a9d205cb32dfed5566019ad54f5bb3415d2d88f7ea1d00c7c794e70a36"
	 "8d3ef5ff6273f2a552152c7febc40eabca26bae05bd12bc85062e2dc224cde9a"
	 "0325a6b5eea7e5febae709dab35ec8648908af12cf2d2b569bedc8da0a3a81c1"
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
			   forge gdscript-mode geiser-guile heaven-and-hell
			   helm-descbinds helm-lsp helm-swoop helm-tramp hl-todo
			   intellij-theme jsdoc json-mode kotlin-mode ligature
			   lsp-java lsp-tailwindcss lsp-ui magit-todos markless
			   meson-mode multiple-cursors nix-mode nov ob-typescript
			   org-modern org-roam page-break-lines paredit php-mode
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




