;;; combyier.el --- A plugin for running Comby with templates in Emacs

;; Author: Your Name <your.email@example.com>
;; Version: 1.0
;; Keywords: comby, templates, Emacs
;; URL: https://your-plugin-repository-url.com

;; This file provides a plugin for interacting with the Comby program inside Emacs.
;; The plugin allows users to manage templates and execute Comby with selected matching,
;; rewrite, and matcher templates.
;;
;; Functions:
;; - combyier-open-templates: Open an editable buffer with template sections.
;; - combyier-extract-sections: Extract sections and call Comby with the templates.
;; - combyier-remove-colors: Remove ANSI color codes from the output.
;; - combyier-call-comby: Call the Comby program with the appropriate arguments asynchronously.
;; - combyier-show-result: Display the *Comby result* buffer.

;;; Code:

(defun combyier-open-templates ()
  "Open an editable buffer named *Comby Templates* with predefined sections."
  (interactive)
  (switch-to-buffer "*Comby Templates*")
  (when (zerop (buffer-size)) ; Initialize buffer if empty
    (erase-buffer)
    (insert ";; matching template\n\n\n;; rewrite template\n\n\n;; matcher (.lisp, .js...)\n")
    (set-buffer-modified-p nil)))

(defun combyier-extract-sections ()
  "Extract the content of the matching, rewrite template, and matcher sections from the buffer and call the comby program."
  (interactive)
  (when (string= (buffer-name) "*Comby Templates*")
    (let* ((content (buffer-string))
	   (matching-template (combyier-extract-section content ";; matching template" ";; rewrite template"))
	   (rewrite-template (combyier-extract-section content ";; rewrite template" ";; matcher (.lisp, .js...)"))
	   (matcher (combyier-extract-section content ";; matcher (.lisp, .js...)" nil)))
      (combyier-call-comby matching-template rewrite-template matcher))))

(defun combyier-extract-section (content start-marker end-marker)
  "Extract content from CONTENT between START-MARKER and END-MARKER.
If END-MARKER is nil, extract until the end of CONTENT."
  (let ((start (string-match (concat "^" (regexp-quote start-marker)) content))
	(end (if end-marker
		 (string-match (concat "^" (regexp-quote end-marker)) content))))
    (when start
      (let ((start-pos (+ start (length start-marker))))
	(string-trim (substring content start-pos (or end (length content))))))))

(defun combyier-remove-colors (str)
  "Remove ANSI color escape sequences from the given string STR."
  (replace-regexp-in-string
   "\033\\[[0-9;]*[mK]" "" str))  ;; This removes ANSI color codes

(defun combyier-call-comby (matching-template rewrite-template matcher)
  "Call the comby program passing the matching template, rewrite template, matcher, and -diff argument asynchronously,
   then display the result in *Comby result* buffer."
  (let ((command (format "comby '%s' '%s' -matcher '%s' -diff"
			matching-template rewrite-template matcher))
	(output-buffer "*Comby result*"))
    (message "Calling comby asynchronously with: %s" command)
    ;; Create or switch to the output buffer
    (get-buffer-create output-buffer)
    (with-current-buffer output-buffer
      (erase-buffer))  ;; Clear the buffer each time
    ;; Set the modeline to indicate that the process is running
    (setq mode-line-process '(" Comby running..."))
    ;; Start the comby process asynchronously
    (let ((process (start-process-shell-command "comby-process" output-buffer command)))
      ;; Set up the process sentinel to handle the process's completion
      (set-process-sentinel process 'combyier-process-sentinel))))

(defun combyier-process-sentinel (process event)
  "Sentinel function to handle the completion of the comby process."
  (if (string-match-p "finished" event)
      (let ((output-buffer (process-buffer process)))
	;; Capture the output of the comby command
	(with-current-buffer output-buffer
	  ;; Read the process output, remove color codes, and insert into the buffer
	  (let ((result (with-current-buffer output-buffer
			  (buffer-string))))
	    (let ((cleaned-result (combyier-remove-colors result)))
	      (erase-buffer)  ;; Clear the buffer before inserting the cleaned result
	      (insert cleaned-result)
	      (set-buffer-modified-p nil)
	      (diff-mode))))  ;; Switch to diff-mode after inserting content
    ;; Update the modeline to indicate the process is complete
    (setq mode-line-process '(" Finished"))
    (message "Comby process finished."))

  ;; Remove the modeline process indicator when the process ends
  (force-mode-line-update)))

(defun combyier-show-result ()
  "Show the *Comby result* buffer. If the buffer does not exist, create it."
  (interactive)
  (switch-to-buffer-other-window "*Comby result*"))

(global-set-key (kbd "C-c c m") 'combyier-open-templates)  ;; Change to C-c c m for opening templates
(global-set-key (kbd "C-c c e") 'combyier-extract-sections)
(global-set-key (kbd "C-c c b") 'combyier-show-result)  ;; New binding for showing result

;;; combyier.el ends here
