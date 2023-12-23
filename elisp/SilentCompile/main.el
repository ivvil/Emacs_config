;;; silent-compile --- Makes compilation windows dissapear
;;
;;; Commentary:
;; Makes compilation window dissapear after some time if compilation is successful
;; from enberg on #emacs

;;; Code:
(defvar silent-compile-timeout "2 sec" "Timeout for successful compilation.")

(add-hook 'compilation-finish-functions
		  (lambda (buf str)
			(if (null (string-match ".*exited abnormally.*" str))
				;;no errors, make the compilation window go away in a few seconds
				(progn
				  (run-at-time
				   silent-compile-timeout nil 'delete-windows-on
				   (get-buffer-create "*compilation*"))
				  (message "No Compilation Errors!")))))
;;; main.el ends here
