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
   '("9013233028d9798f901e5e8efb31841c24c12444d3b6e92580080505d56fd392" "10e5d4cc0f67ed5cafac0f4252093d2119ee8b8cb449e7053273453c1a1eb7cc" "7964b513f8a2bb14803e717e0ac0123f100fb92160dcf4a467f530868ebaae3e" "c5878086e65614424a84ad5c758b07e9edcf4c513e08a1c5b1533f313d1b17f1" "4e2e42e9306813763e2e62f115da71b485458a36e8b4c24e17a2168c45c9cf9d" "5f128efd37c6a87cd4ad8e8b7f2afaba425425524a68133ac0efd87291d05874" "b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" default))
 '(emms-browser-covers '(emms-browser-cache-thumbnail))
 '(lsp-java-vmargs
   '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx1G" "-Xms100m" "--enable-preview"))
 '(package-selected-packages
   '(solaire-mode org-roam treemacs-nerd-icons lua-mode cmake-mode jsdoc platformio-mode treemacs-projectile treemacs-magit treemacs-all-the-icons markless magit-todos hl-todo solo-jazz-theme evil rainbow-mode rainbow-delimiters flycheck-rust typescript-mode ob-typescript yaml-mode xmlgen multiple-cursors emacsql org-modern nov dashboard minions doom-modeline sqlite3 emms sly corfu geiser-guile gdscript-mode lsp-java lsp-intellij helm-lsp lsp-ui dap-mode flycheck yasnippet-snippets company quickrun which-key avy helm-swoop helm-descbinds helm wfnames use-package-chords projectile popup helm-core heaven-and-hell exec-path-from-shell doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font-10")
(set-frame-font "CaskaydiaCove Nerd Font-10" nil t)



