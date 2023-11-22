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
 '(TeX-engine 'default)
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(doom-palenight))
 '(custom-safe-themes
   '("b1acc21dcb556407306eccd73f90eb7d69664380483b18496d9c5ccc5968ab43" default))
 '(emms-browser-covers '(emms-browser-cache-thumbnail))
 '(package-selected-packages
   '(emms sly corfu geiser-guile gdscript-mode lsp-java lsp-intellij helm-lsp lsp-ui dap-mode flycheck yasnippet-snippets company quickrun which-key avy helm-swoop helm-descbinds helm wfnames use-package-chords projectile popup helm-core heaven-and-hell exec-path-from-shell doom-themes)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :font "CaskaydiaCove Nerd Font-10")
(set-frame-font "CaskaydiaCove Nerd Font-10" nil t)

(let ((elisp-directory "/home/ivvil/.emacs.d/elisp/"))
  (when (file-directory-p elisp-directory)
    (dolist (entry (directory-files elisp-directory t "^[^.]" 'nosort))
      (when (file-directory-p entry)
        (add-to-list 'load-path entry)))))
(require 'gdscript-mode)
(require 'which-linux-distribution)
