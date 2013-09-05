;;; lcEngine.el --- code completion using libclang for emacs, only support win32 platform

;; Copyright (C) 2011-2013 lynnux

;; Author: lynnux <lynnux.cn@gmail.com>
;; Thanks: Tomohiro Matsuyama for his gccsense.el
;; Keywords: code completion, convenience, clang, libclang

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; You should firstly run lcEngineConfig.exe in emacs bin directory. That will 
;; add lcEngine.dll to emacs's import table, and also gather some further information
;; for lcEngine.dll to hack emacs.

;; Usage:
;; (add-hook 'c-mode-common-hook
;; (lambda ()
;;  (local-set-key (kbd "C-.") 'lcEngine-complete) ;;using system complete buffer
;;   or
;;  (local-set-key (kbd "C-.") 'ac-complete-lcEngine)) ;;using auto complete, but
;;   
;;  (ac-lcEngine-setup) ;; 
;; )


;;; Code:

(require 'auto-complete)

(defgroup lcEngine nil
  "code complete using libclang."
  :group 'completion
  :prefix "lcEngine-")

(defconst lcEngine-call-process-proc "runemacs.exe"
  "Do not change this !")

(defconst lcEngine-process-send-region-name "*lcEngine*"
  "Do not change this !")

(defconst lcEngine-process-send-region-proc "cmd.exe"
  "")

(defvar lcEngine-result-format "^\\($$r:\\(.*\\)\\)*$$t:\\(.*\\)$$p:\\(.*\\)" )
;; create a puppet process, which signal lcEngine to log the buffer content
(defun lcEngine-send-process-buffer (&optional buffer)
  (or buffer (setq buffer (current-buffer)))
  (with-current-buffer buffer
    (unless (get-process lcEngine-process-send-region-name)
      (set-process-query-on-exit-flag (start-process lcEngine-process-send-region-name nil lcEngine-process-send-region-proc) nil))
    (process-send-region lcEngine-process-send-region-name (point-min) (point-max))))

;; will get ("runemacs.exe" . "-Cfile:line:column:saved?")
(defun lcEngine-make-command (&rest command)
  (append `(,lcEngine-call-process-proc)
          command))

;; send command to lcEngine
(defun lcEngine-command-to-string (command)
  (with-output-to-string
    (with-current-buffer standard-output
      (apply 'call-process (car command) nil t nil (cdr command)))))

(defun lcEngine-get-completions (&optional buffer point)
  (or buffer (setq buffer (current-buffer)))
  (or point (setq point (point)))
  (with-current-buffer buffer
    (save-excursion
      (goto-char point)
      (let* ((buffer-saved (not (buffer-modified-p buffer))) ; if saved, there is no need use unsaved files in libclang
	     (filename (buffer-file-name buffer))
             (line (line-number-at-pos))
             (column (1+ (current-column)))
	     lines)
	(unless buffer-saved
	  (lcEngine-send-process-buffer buffer)) ; send buffer context
	;; get result
	(setq lines (delq nil
	       (split-string (lcEngine-command-to-string
			      (lcEngine-make-command
			       (format "-C%s|%s|%s|%s" filename line column 
				       (if buffer-saved
					   "n" ; no unsaved
					 "y"))))
			     "\n")))
	;; push raw result to 'lcEngine-yas propertize
	;; raw format: $$r:returntype$$t:typedstring$$p:rest
	;; display format: typedstring /*returntype typedstring rest*/
	(dolist (l lines result) 
	  (when (string-match lcEngine-result-format l)
	    (let ((returntype (match-string-no-properties 2 l)) ; the constructor may don't have a retun type
		  (typedstring (match-string-no-properties 3 l))
		  (rest (match-string-no-properties 4 l))
		  match)
	      (setq match (concat typedstring " /*" returntype " " typedstring rest "*/"))
	      (setq match (propertize match 'lcEngine-raw-text l))
	      (push match result))))
	))))

(defun lcEngine-complete ()
  (interactive)
  (if (save-excursion (re-search-backward "\\(?:\\.\\|->\\|::\\)\\(.*\\)\\=" (line-beginning-position) t))
      (let* ((offset (match-beginning 1))
             (point (match-end 0))
             (prefix (match-string 1))
             (list (all-completions prefix
                                    (delete-dups (lcEngine-get-completions (current-buffer)
									   offset))))
             (common (try-completion prefix list))
             (buffer "*Completions*"))
        (when (and (stringp common)
                   (not (equal prefix common)))
          (delete-region offset point)
          (insert common)
          (setq prefix common))
        (cond
         ((null list)
          (message "No completions"))
         ((eq (length list) 1)
          (let ((window (get-buffer-window buffer)))
            (if window
                (with-selected-window window
                  (or (window-dedicated-p window)
                      (bury-buffer))))))
         (t
          (with-output-to-temp-buffer buffer
            (display-completion-list list prefix))
          (display-buffer buffer))))))

;; Auto Complete Mode
(defvar ac-source-lcEngine-member
  '((candidates . (lcEngine-get-completions nil ac-point))
    (prefix "\\(?:\\.\\|->\\)\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
;    (prefix . ac-prefix-default)
					;   (document . (lambda (item) (car item)))
    (requires . 0)
    (symbol . "m")
    ;(action . ac-lcEngine-action)
    (cache)))

(defvar ac-source-lcEngine-static-member
  '((candidates . (lcEngine-get-completions nil ac-point))
    (prefix "::\\(\\(?:[a-zA-Z_][a-zA-Z0-9_]*\\)?\\)" nil 1)
;    (prefix . ac-prefix-default)
					;    (document . (lambda (item) (car item)))
    (requires . 0)
    (symbol . "M")
    ;(action . ac-lcEngine-action)
    (cache)))

;;; from emacs-clang-complete-async
(defun ac-lcEngine-async-preemptive ()
  (interactive)
  (self-insert-command 1)
  (ac-start))

(defvar ac-lcEngine-do-autocompletion-automatically t)
 
(defun ac-lcEngine-autocomplete-autotrigger ()
  (interactive)
  (if ac-lcEngine-do-autocompletion-automatically
      (ac-lcEngine-async-preemptive)
    (self-insert-command 1)))

(defun lcEngine-yas-expand-string (string &optional remove-undo-boundary)
  "Expand `STRING' into the buffer and update `ac-prefix' to `STRING'.
This function records deletion and insertion sequences by `undo-boundary'.
If `remove-undo-boundary' is non-nil, this function also removes `undo-boundary'
that have been made before in this function.  When `buffer-undo-list' is
`t', `remove-undo-boundary' has no effect."
  (when (eq buffer-undo-list t)
    (setq remove-undo-boundary nil))
  (when (not (equal string (buffer-substring ac-point (point))))
    (undo-boundary)
    ;; We can't use primitive-undo since it undoes by
    ;; groups, divided by boundaries.
    ;; We don't want boundary between deletion and insertion.
    ;; So do it manually.
    ;; Delete region silently for undo:
    (if remove-undo-boundary
        (progn
          (let (buffer-undo-list)
            (save-excursion
              (delete-region ac-point (point))))
          (setq buffer-undo-list
                (nthcdr 2 buffer-undo-list)))
      (delete-region ac-point (point)))
    (yas-expand-snippet string)
    ;; Sometimes, possible when omni-completion used, (insert) added
    ;; to buffer-undo-list strange record about position changes.
    ;; Delete it here:
    (when (and remove-undo-boundary
               (integerp (cadr buffer-undo-list)))
      (setcdr buffer-undo-list (nthcdr 2 buffer-undo-list)))
    (undo-boundary)
    (setq ac-selected-candidate string)
    (setq ac-prefix string)))

(defadvice ac-complete-1 (around lcEngine-ac-complete-1 (candidate) activate)
  (let ((raw-text (get-text-property 0 'lcEngine-raw-text candidate)))
    (if raw-text
	;; copy from ac-complete-1
	(let ((action (popup-item-property candidate 'action))
	      (fallback nil))
	  (when candidate
	    (when (string-match lcEngine-result-format raw-text)
	      (let ((returntype (match-string-no-properties 2 raw-text)) ; the constructor may don't have a retun type
		    (typedstring (match-string-no-properties 3 raw-text))
		    (rest (match-string-no-properties 4 raw-text))
		    )
		(if (featurep 'yasnippet) 
		    (let ((match (concat typedstring rest)))
		      ;; (yas/expand-snippet match)
		      (lcEngine-yas-expand-string match)
		      )
		  (unless (ac-expand-string typedstring)
		    (setq fallback t)))))
	    
	    ;; Remember to show help later
	    (when (and ac-point candidate)
	      (unless ac-last-completion
		(setq ac-last-completion (cons (make-marker) nil)))
	      (set-marker (car ac-last-completion) ac-point ac-buffer)
	      (setcdr ac-last-completion candidate)))
	  (ac-abort)
	  (cond
	   (action
	    (funcall action))
	   (fallback
	    (ac-fallback-command)))
	  candidate)
      ad-do-it)))

;; current can't resolved
(defadvice ac-expand-common (around lcEngine-ac-expand-common activate)
  nil)

(defun ac-lcEngine-action ()
  (interactive)
  (when (featurep 'yasnippet)
    (yas-expand))
  )

;;; automatically complete without any key down
(defun ac-lcEngine-setup ()
  (setq ac-sources (append '(ac-source-lcEngine-member ac-source-lcEngine-static-member) ac-sources))
  (local-set-key (kbd ".") 'ac-lcEngine-autocomplete-autotrigger)
  (local-set-key (kbd ":") 'ac-lcEngine-autocomplete-autotrigger)
  (local-set-key (kbd ">") 'ac-lcEngine-autocomplete-autotrigger))


;;; 
(defun ac-complete-lcEngine ()
  (interactive)
  (auto-complete '(ac-source-lcEngine-member ac-source-lcEngine-static-member)))

(provide 'lcEngine)
;;; lcEngine.el ends here
