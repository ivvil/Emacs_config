(defun my-initial-buffer ()
  (interactive)
  (let ((buf (get-buffer-create "*Initial Buffer*"))
        (image-file "/ruta/a/tu/imagen/decorativa.jpg"))
    (with-current-buffer buf
      (erase-buffer)
      (insert "¡Bienvenido a Emacs!\n\n")
      (insert-image (create-image image-file 'jpeg t :scale 0.5))
      (insert "\n\n")
      (insert "Enlaces útiles:\n")
	  (insert-button "Editar Configuración de Zsh"
                     'action (lambda (_) (my-open-bash-config)))
      (insert "\n")
      (insert-button "Editar Configuración de Emacs"
                     'action (lambda (_) (my-open-emacs-config)))
      (insert "\n")
      (insert-button "Abrir Dired en Unidades Extraíbles"
                     'action (lambda (_) (my-open-dired-removable-drives)))
      ;; (dolist (file "~/.emacs.d/recentf")
      ;;   (insert (format "- %s\n" file)))
      (goto-char (point-min)))
    (pop-to-buffer buf)))

(defun my-open-bash-config ()
  (interactive)
  (find-file "~/.zshrc"))

(defun my-open-emacs-config ()
  (interactive)
  (find-file "~/.emacs.d/emacs-configuration.org"))

(defun my-open-dired-removable-drives ()
  (interactive)
  (dired "/media/")) ; Cambia la ruta según la ubicación de tus unidades extraíbles

(global-set-key (kbd "C-x C-b") 'my-initial-buffer)
(global-set-key (kbd "C-c e b") 'my-open-bash-config)
(global-set-key (kbd "C-c e e") 'my-open-emacs-config)
(global-set-key (kbd "C-c e d") 'my-open-dired-removable-drives)
