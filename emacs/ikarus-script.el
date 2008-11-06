;;; ikarus-script.el --- Ikarus script support

;; Copyright (C) 2008  Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>
;; Keywords: languages, tools

;; This program is free software; you can
;; redistribute it and/or modify it under the terms of the GNU General
;; Public License as published by the Free Software Foundation; either
;; version 3 of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This code implements a major mode derived from compilation-mode,
;; which overrides some functionality in it to allow it handling
;; Ikarus' error source locations.

;; Ikarus uses character-based file locations, not line and column,
;; which makes it necessary to override code of compilation-mode
;; instead of using it as-is, but on the other hand, the overriding
;; code is quite a bit simpler that the overriden code, as charater
;; positions are easier to handle.

;; Usage:

;; You can use this by hooking `ikarus-script-setup-buffer' into
;; `scheme-mode-hook':
;;
;; (add-hook 'scheme-mode-hook 'ikarus-script-setup-buffer)
;;
;; This will prepare the buffers in the Scheme major mode for use of
;; `ikarus-run-script' wich runs a script using Ikarus and
;; `ikarus-rerun-script', which lets you start the script with the
;; arguments from the last invocation, or without arguments on the
;; first invocation without prompting. You can also invoke
;; `ikarus-rerun-script' from within an *ikarus-script* buffer, and it
;; will rerun the script with same arguments. 

;; In `ikarus-script-mode', `ikarus-rerun-script' is bound to "C-c
;; C-r" and `ikarus-run-script' to "C-c C-i". It makes sense to also
;; bind those two functions in `scheme-mode-map', for example:
;;
;; (define-key scheme-mode-map (kbd "C-c i") 'ikarus-run-script)
;; (define-key scheme-mode-map (kbd "C-c r") 'ikarus-rerun-script)

;;; Code:


(defun ikarus-toggle-trace ()
  ;; IMPLEMENTME
  t)

(setq ikarus-filepos-regex "&source-information:\n\\s-*file-name: \"\\([^\"]+\\)\"\n\\s-*character: \\([0-9]+\\)")

(defun ikarus-visit-last-trace ()
  "Visit the location indicated by the last &source-information condition.
This searches backwards in the current buffer for a Ikarus source location,
and visits that position in the other window. You can for instance us this
in a shell buffer that you have run ikarus in.

See also `ikarus-run-script' and `ikarus-rerun-script' for a convinient way
to start R6RS scripts with ikarus and visit resulting error locations."
  (interactive)
  (let ((found-p (save-excursion
		   (re-search-backward ikarus-filepos-regex nil t))))
    (cond (found-p
	   (let ((filename (match-string 1))
		 (pos (string-to-number (match-string 2))))
	     (find-file-other-window filename)
	     (goto-char pos)))
	  (t
	   (message "no trace file position found")))))

(setq ikarus-script-error-regexp-alist `((,ikarus-filepos-regex 1 2)))

(defun ikarus-script-next-error-function (n &optional reset)
  "Advance to the next Ikarus error message and visit the file where the error was.
This is the value of `next-error-function' in Ikarus-Script buffers."
  (interactive "p")
  (when reset
    (setq compilation-current-error nil))
  (let* ((columns compilation-error-screen-columns) ; buffer's local value
	 (last 1)
	 (loc (compilation-next-error (or n 1) nil
				      (or compilation-current-error
					  compilation-messages-start
					  (point-min))))
	 (marker (point-marker)))
    (setq compilation-current-error (point-marker)
	  overlay-arrow-position
	    (if (bolp)
		compilation-current-error
	      (copy-marker (line-beginning-position)))
	  loc (car loc))
    ;; If loc contains no marker, no error in that file has been
    ;; visited.  If the marker is invalid the buffer has been killed.
    ;; So, recalculate the marker.
    (unless (and (nth 3 loc) (marker-buffer (nth 3 loc)))
      (with-current-buffer (compilation-find-file marker (caar (nth 2 loc))
						  (cadr (car (nth 2 loc))))
	(save-excursion
	  (goto-char (nth 1 loc))
	  (if (nth 3 loc)
	      (set-marker (nth 3 loc) (point))
	    (setcdr (nthcdr 2 loc) `(,(point-marker)))))))
    (compilation-goto-locus marker (nth 3 loc) nil)
    (setcdr (nthcdr 3 loc) t)))

(defun ikarus-run-script (filename &rest args)
  "Run the R6RS script in FILENAME with ARGS as arguments using Ikarus"
  (interactive (append (list (read-file-name "R6RS script to launch ikarus on: "
					     nil (buffer-file-name)))
		       (split-string (read-string "Script arguments: "))))
  (flet ((compilation-mode-font-lock-keywords () (ikarus-script-mode-font-lock-keywords)))
    (setq ikarus-run-script-command-line (cons filename args))
    (compilation-start (concat "ikarus --r6rs-script "
			       (mapconcat 'identity (cons filename args) " "))
		       'ikarus-script-mode)))

(defun ikarus-script-setup-buffer ()
  "Prepare a buffer for usage with the ikarus-script commands."
  (set (make-local-variable 'ikarus-run-script-command-line)
       (list (buffer-file-name))))

(defun ikarus-rerun-script ()
  "Invoke `ikarus-run-script' with the script and arguments from the last run.
If the current buffer contains a script that has not yet been invoked, it will be run
without arguments.

For this function to work properly outside of *ikarus-script* buffers, the buffers that
this function is invoked must have been prepared by calling `ikarus-script-setup-buffer'."
  (interactive)
  (apply 'ikarus-run-script (or (and compilation-arguments 
				     (cddr (split-string (car compilation-arguments))))
				ikarus-run-script-command-line)))

(defun ikarus-script-mode-font-lock-keywords ()
  "Return expressions to highlight in Ikarus-Script mode."
  (append
   (mapcar
    (lambda (item)
      (if (symbolp item)
	  (setq item (cdr (assq item
				compilation-error-regexp-alist-alist))))
      (let ((file (nth 1 item))
	    (char (nth 2 item)))
	`(,(nth 0 item)

	  ,@(when (integerp file)
	      `((,file compilation-error-face)))

	  ,@(when char
	      `((,char compilation-line-face nil t)))
	  
	  (0
	   (ikarus-script-error-properties ',file ,char)
	   append))))			; for compilation-message-face
    ikarus-script-error-regexp-alist)
   compilation-mode-font-lock-keywords))

(defun ikarus-script-error-properties (file char)
  (unless (< (next-single-property-change (match-beginning 0) 'directory nil (point))
	     (point))
    (and char
	 (setq char (match-string-no-properties char))
	 (setq char (string-to-number char)))
    (setq file (match-string-no-properties file))
    `(face ,compilation-message-face
	   message ,(list (list nil char `((,file nil)) nil) 2 nil)
	   help-echo ,(if char
			  "mouse-2: visit this file and position"
			"mouse-2: visit this file")
	   keymap compilation-button-map
	   mouse-face highlight)))

(define-compilation-mode ikarus-script-mode "Ikarus-Script"
  "Specialization of compilation-mode for running Ikarus Scheme scripts"
  (local-set-key (kbd "C-c C-i") 'ikarus-run-script)
  (local-set-key (kbd "C-c C-r") 'ikarus-rerun-script)
  (local-set-key (kbd "RET") 'ikarus-visit-error)
  (setq next-error-function 'ikarus-script-next-error-function))

(provide 'ikarus-script)

;;; ikarus-script.el ends here
