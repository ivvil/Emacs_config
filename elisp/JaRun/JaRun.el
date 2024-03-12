;;; JaRun --- Compile and run java projects from emacs  -*- lexical-binding: t; -*-
;;
;;; Commentary:
;; from https://emacs.stackexchange.com/questions/18205/how-can-i-distinguish-between-linux-distributions-in-emacs
;; Gets the distro name using lsb_release -sd, throws an error if it isn't found

;;; Code:

(require 'eshell)

(defvar JaRun-javac-path (executable-find "javac") "Java compiler path.")
(defvar JaRun-javac-args '() "Plist for argumets for java compilation.")

(defvar JaRun-bin-folder-name "bin" "Name for the output folder of class compilation.")
(defvar JaRun-src-folder-name "src" "Name for the input folder of class compilation.")

(defvar JaRun-java-path (executable-find "java") "Java path.")
(defvar JaRun-java-args '() "Plist for argumets for java.")

(defvar JaRun-project-root (get-project-root ) "Root for the current project.")

(defvar JaRun-project-bin-file (JaRun--get-folder (concat JaRun-project-root "/" JaRun-bin-folder-name)) "Output folder for binnary compilaton of classes.")
(defvar JaRun-project-src-file (JaRun--get-folder (concat JaRun-project-root "/" JaRun-src-folder-name)) "Input folder for binnary compilaton of classes.")

(defvar JaRun-project-main-class "Main" "Main class for the current project.")

(defun get-files-by-extension (directory extension)
  "Recursively get all files in DIRECTORY with the specified EXTENSION."
  (let ((files '()))
    (dolist (file (directory-files-recursively directory (concat "\\." extension "$")))
      (unless (file-directory-p file)
        (setq files (cons file files))))
    (nreverse files)))

(defun JaRun-compile-files (bin dest)
  "Compiles files in BIN to DEST."
  (shell-command-to-string
   (concat JaRun-javac-path " -d " dest " -sourcepath " bin " " (mapconcat 'identity (get-files-by-extension bin "java") " "))))

(defun JaRun-run-class (class root)
  "Run the specified CLASS form project ROOT."
  (interactive "P")
  (JaRun--run-eshell root (concat JaRun-java-path " -cp " class)))

(defun JaRun--get-buffer-package-definition ()
  "Get the curent pakcage definition for the buffer."
  (save-excursion
	(goto-char (point-min))
	(re-search-forward "^\s*package\s+([a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*);\s*$" nil t)))

(defun JaRun--check-package-deinition (package-definition path)
  "Ckecks wether PACKAGE-DEFINITION matches PATH"
  (let* ((package-parts (split-string package-definition "\\." t))
		 (dir-parts (split-string (file-name-directory path) "/" t))
		 ())
  )


;; (defun get-project-root (file)
;;   "Gets the project root for FILE."

;;   ;; Options for getting the project root:
;;   ;;  - Searching for a package declaration inside buffer and setting project root accordingly.
;;   ;;  - Walking backwards the directory tree until a src/ folder is found.
  
;;   (let* ((package (split-string (save-excursion
;; 					(goto-char (point-min)) ; Move to the beginning of the buffer
;; 					(re-search-forward "^\s*package\s+([a-zA-Z_]\w*(?:\.[a-zA-Z_]\w*)*);\s*$" nil t)) "\\." t))
;; 		 ())
;; 	)
;;   )

(defun JaRun--get-folder (folder)
  "Gets a folder FOLDER.  If it doesn't exist, create it."
  (unless (file-exists-p folder)
    (make-directory folder t))
  folder)

(defun JaRun--buffer-contains-regex (regex)
  "Check if the current buffer contains a match for the given REGEX."
  (save-excursion
    (goto-char (point-min)) ; Move to the beginning of the buffer
    (re-search-forward regex nil t)))

(defun JaRun-compile-project ()
  "Compliles current project."
  (interactive "P")
  (JaRun-compile-files JaRun-project-bin-file JaRun-project-src-file))

(defun JaRun--run-eshell (path cmd)
  "Run the CMD in PATH with eshell."
  (eshell)
  (goto-char (point-max))
  (eshell-kill-input)
  (insert (concat "cd " path))
  (eshell-send-input)
  (insert cmd)
  (eshell-send-input))

(defun JaRun-run-project (&optional class)
  "Run the Main class of a project.  Optionaly run CLASS."
  (interactive "P")
  (setq JaRun-project-main-class (or class JaRun-project-main-class))
  (JaRun-compile-project)
  (JaRun-run-class JaRun-project-main-class JaRun-project-root))

;;; JaRun.el ends here
