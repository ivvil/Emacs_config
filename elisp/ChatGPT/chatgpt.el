;;; chatgpt.el --- Simple ChatGPT frontend for Emacs  -*- lexical-binding: t -*-

;; Copyright (C) Gavin Jaeger-Freeborn

;; This package is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This package is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;; Author: Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>
;; Maintainer: Gavin Jaeger-Freeborn <gavinfreeborn@gmail.com>
;; Created: 2022
;; Version: 0.34
;; Package-Requires: ((emacs "28.1"))

;;; Commentary:

;; Basic commands to use the OpenAI API and use some of the power
;; ChatGPT provides within Emacs.

;; Features include code explanation, attempted code completion,
;; replacing inline queries with the results and more.

;;; Code:

(require 'seq)
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'env)
  (require 'json))

(defgroup chatgpt nil
  "ChatGPT frontend."
  :group 'convenience
  :prefix "chatgpt-")

(defcustom chatgpt-max-tokens 300
  "Upper limit on the number of tokens the API will return."
  :type 'integer)

(defvar chatgpt-buffer "*ChatGPT*"
  "Title of the buffer used to store the results of an OpenAI API query.")

(define-error 'chatgpt-error "An error related to the ChatGPT emacs package")

(define-error 'chatgpt-parsing-error
			  "An error caused by a failure to parse an OpenAI API Response")

(defmacro chatgpt-show-results-buffer-if-active ()
  "Show the results in other window if necessary."
  `(if (and (not ;; visible
             (get-buffer-window chatgpt-buffer))
			(called-interactively-p 'interactive))
       (lambda (&optional buf) (ignore buf)
         (with-current-buffer buf
           (view-mode t))
         (switch-to-buffer-other-window chatgpt-buffer))
     #'identity))

;;;###autoload
(defun chatgpt-prompt (prompt callback)
  "Query OpenAI with PROMPT calling the CALLBACK function on the resulting buffer.
Returns buffer containing the text from this query"
  (interactive (list (read-string "Prompt ChatGPT with: ")
                     (lambda (buf) (with-current-buffer buf
									 (view-mode t))
                       (switch-to-buffer-other-window chatgpt-buffer))))
  (chatgpt--query-open-api prompt
                           (lambda (results)
                             (with-current-buffer (get-buffer-create chatgpt-buffer)
                               ;; Erase contents of buffer after receiving response
                               (read-only-mode -1)
                               (erase-buffer)
                               (insert results)
                               ;; Return the chatgpt output buffer for non interactive usage
                               (funcall callback (current-buffer))))))

;;;###autoload
(defun chatgpt-fix-region (BEG END)
  "Takes a region BEG to END asks ChatGPT to explain whats wrong with it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (chatgpt-prompt (chatgpt--append-to-prompt
                     current-code
                     "Why doesn't this code work?")
                    (chatgpt-show-results-buffer-if-active))))

;;;###autoload
(defun chatgpt-explain-region (BEG END)
  "Takes a region BEG to END asks ChatGPT what it does.
The answer in the displays in `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (chatgpt-prompt (chatgpt--append-to-prompt
                     current-code
                     "What does this code do?")
                    (chatgpt-show-results-buffer-if-active))))

;;;###autoload
(defun chatgpt-gen-tests-for-region (BEG END)
  "Takes a region BEG to END asks ChatGPT to write a test for it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code (buffer-substring BEG END)))
    (chatgpt-prompt (chatgpt--append-to-prompt
                     current-code
                     "Write me a tests for this code")
                    (chatgpt-show-results-buffer-if-active))))

;; TODO currently just says what changed but doesn't wanna show the code it's self
;; (defun chatgpt-optimize-region (BEG END)
;;   "Takes a region BEG to END asks ChatGPT to optimize it for speed.
;; It then displays the answer in the `chatgpt-buffer'."
;;   (interactive "r")
;;   (let ((current-code         (buffer-substring BEG END)))
;;     (chatgpt-prompt (chatgpt--append-to-prompt
;;                      current-code
;;                      "Refactor this code for speed and tell me what you changed and why it's faster")
;;                     (chatgpt-show-results-buffer-if-active))))

;;;###autoload
(defun chatgpt-refactor-region (BEG END)
  "Takes a region BEG to END asks ChatGPT refactor it.
It then displays the answer in the `chatgpt-buffer'."
  (interactive "r")
  (let ((current-code         (buffer-substring BEG END)))
    (chatgpt-prompt (chatgpt--append-to-prompt
                     current-code
                     "Refactor this code and tell me what you changed")
                    (chatgpt-show-results-buffer-if-active))))

;;;###autoload
(defun chatgpt-prompt-region (BEG END)
  "Prompt ChatGPT with the region BEG END.
It then displays the results in a separate buffer `chatgpt-buffer'."
  (interactive "r")
  (chatgpt-prompt (buffer-substring BEG END)
                  ;; Show the results if not already being viewed
                  (chatgpt-show-results-buffer-if-active)))

;;;###autoload
(defun chatgpt-prompt-region-and-replace (BEG END)
  "Replace region from BEG to END with the response from the ChatGPT API.

The region is BEG and until END"
  (interactive "r")

  (let ((og-buf (current-buffer)))
    (chatgpt-prompt (buffer-substring BEG END)
                    (lambda (buf)
                      (save-excursion
                        (with-current-buffer og-buf
                          (delete-region BEG END)
                          (goto-char BEG)
                          (insert (with-current-buffer buf (buffer-string)))))))))
(defun chatgpt--append-to-prompt (prompt comment-str)
  "Append the string COMMENT-STR extra information to a PROMPT as a comment."
  (concat prompt
          "\n"
		  comment-start
          " "
		  comment-str))

(defun chatgpt--extract-text-from-query (query-result)
  "Extract the resulting text from a given OpenAI response QUERY-RESULT."
  (condition-case err
      (thread-last query-result
                   (assoc-default 'choices)
                   seq-first
                   (assoc-default 'text)
                   string-trim)
    (error
     (signal 'chatgpt-parsing-error err))))

(defun chatgpt--parse-response (status callback)
  "Ignoring STATUS and parse the response executing the CALLBACK function on the resulting string."
  (ignore status)
  ;; All this is ran inside the buffer containing the response
  (goto-char 0)
  (re-search-forward "^$")
  (funcall callback (chatgpt--extract-text-from-query (json-read))))

(defun chatgpt--query-open-api (prompt callback)
  "Send a string PROMPT to OpenAI API and pass the resulting buffer to CALLBACK.
The environment variable OPENAI_API_KEY is used as your API key

You can register an account here
https://beta.openai.com/docs/introduction/key-concepts"
  (let* ((api-key (getenv "OPENAI_API_KEY"))
         (url-request-method (encode-coding-string "POST" 'us-ascii))
		 (url-request-extra-headers `(("Content-Type" . "application/json")
									  ("Authorization" . ,(format "Bearer %s" api-key))))
         (url-request-data (json-encode
							`(("model" . "text-davinci-003")
							  ("prompt" . ,prompt)
							  ("max_tokens" . ,chatgpt-max-tokens)
							  ("temperature" . 0)))))
    (cl-assert (not (string= "" api-key))
               t
               "Current contents of the environmental variable OPENAI_API_KEY
are '%s' which is not an appropriate OpenAI token please ensure
you have the correctly set the OPENAI_API_KEY variable"
               api-key)
    (url-retrieve
     "https://api.openai.com/v1/completions"
     'chatgpt--parse-response
     (list callback))))

(provide 'chatgpt)
;;; chatgpt.el ends here
