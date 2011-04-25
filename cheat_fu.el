;; cheat_fu.el
;; Time-stamp: <2011-04-25 10:58:16 jpablobr>

;; Copyright (C) Jose Pablo Barrantes 2011 <xjpablobrx@gmail.com>

;; Licensed under the same terms as Emacs.

;; Keywords: cheats bash
;; Created: 25 April 2011
;; Author: Jose Pablo Barrantes 2011 <xjpablobrx@gmail.com>
;; Version: 0.1.0

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This minor mode exists to provide emacs with an interface to cheat_fu
;; https://github.com/jpablobr/cheat_fu

;; You can configure the cheat_fu-sheets root directory by appending
;; a directory name onto the `cheat_fu-root'.

;;; Installation

;; In your emacs config:
;;
;; (add-to-list 'load-path "~/.emacs.d/load/path/cheat_fu.el")
;; (require 'cheat_fu)
;; (setq cheat_fu-root "/path/to/.cheat_fu_sheets/")

;;; ----------------------------------------------------------------------------
;;; - Customization
;;;
(defcustom cheat_fu-root nil
  "Used internally for defining cheat_fu_sheets root path."
  :group 'cheat_fu
  :type 'string)

;;; ----------------------------------------------------------------------------
;;; - Cheat_fu config
;;;
(define-button-type 'cheat_fu-find-file-button
  'follow-link t
  'action #'cheat_fu-find-file-button)

(defvar *cheat_fu-to-include* "*.1.ronn"
  "Regexp of files to exclude from `cheat_fu-sheets'.")

(defvar cheat_fu-completing-library 'ido
  "The library `cheat_fu-sheets' should use for
  completing sheets (`ido' by default)")

(defvar cheat_fu-roff-convert-command "ronn --roff --html \"%s\" "
  "The command used to convert sheets to man and html pages. %s will be
  replaced with the current cheat_fu-sheet.")

(defvar cheat_fu-find-sheets-command "find \"%s\" -type f -name "
  "The command used to find sheets. %s will be replaced with the cheat_fu-root.")

(defvar cheat_fu-list-sheets-command "cheat_fu -l"
  "The command used to list all sheets.")

(defvar cheat_fu-search-sheets-command "cheat_fu -s \"%s\" "
  "The command used to find sheets. %s will be replaced with the search input.")

(defvar *cheat_fu-completing-function-alist* '((ido ido-completing-read)
                                               (icicles  icicle-completing-read)
                                               (none completing-read))
  "The function to call to read sheets.")

(defvar *cheat_fu-completing-minor-mode-alist*
  `((ido ,(lambda (a) (progn (ido-mode a) (setq ido-enable-flex-matching t))))
    (icicles ,(lambda (a) (icy-mode a)))
    (none ,(lambda (a) ())))
  "The list of functions to enable and disable completing minor modes")

;;; ----------------------------------------------------------------------------
;;; - Interctive Functions
;;;
(defun cheat_fu-list ()
  "Output paths to all sheets."
  (interactive)
  (cheat_fu-command "-l"))

(defun cheat_fu-search(what)
  "Output paths to sheets matching search  'input'."
  (interactive "sCheat_fu Sheet: ")
    (cheat_fu-command "-l" what))

(defun cheat_fu-convert ()
  "Converts cheat_fu-sheets to man and html."
  (interactive)
  (shell-command-to-string
   (cheat_fu-string-replace "%s" buffer-file-name cheat_fu-roff-convert-command)) t)

(defun cheat_fu-sheets ()
  "Uses your completing read to quickly jump to the sheets."
  (interactive)
  (find-file
   (concat
    (expand-file-name cheat_fu-root)
    (cheat_fu-completing-read
     "Find file: "
     (mapcar
      (lambda (e)
        (replace-regexp-in-string cheat_fu-root "" e))
      (cheat_fu-project-files  cheat_fu-root))))))

(defun cheat_fu-buttonize-buffer ()
  "turn all file paths into buttons"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "/[^ \t]*" nil t)
      (make-button (match-beginning 0) (match-end 0) :type 'cheat_fu-find-file-button))))

;;; ----------------------------------------------------------------------------
;;; - Helpers
;;;
(defun cheat_fu-command (&rest rest)
  "Run the cheat_fu command with the given arguments, display the output."
  (interactive "sArguments for cheat_fu: \n")
  (let ((buffer (get-buffer-create "*Cheat_fu*"))
        (cmd (string-join " " rest))
        (inhibit-read-only t))
    (setq next-error-last-buffer buffer)
    (with-current-buffer buffer
      (erase-buffer)
      (cheat_fu-mode)
      (setq buffer-read-only t)
      (font-lock-fontify-buffer))
    (shell-command (concat "cheat_fu " cmd) buffer)))

(defun cheat_fu-find-file-button (button)
  (find-file (buffer-substring (button-start button) (button-end button))))

(defun cheat_fu-completing-read (&rest args)
  "Uses `*cheat_fu-completing-function-alist*' to call the appropriate completing
  function."
  (let ((reading-fn
         (cadr (assoc cheat_fu-completing-library
                      *cheat_fu-completing-function-alist*))))
    (apply (symbol-function reading-fn) args)))

(defun cheat_fu-project-files (root)
  "Finds all sheets in cheat_fu-sheets."
  (split-string
   (shell-command-to-string
    (concat
     (cheat_fu-string-replace "%s" root cheat_fu-find-sheets-command)
     *cheat_fu-to-include*
     " | sed 's:"
     cheat_fu-root
     "::'")) "\n" t))

;; http://snipplr.com/view/18683/stringreplace/
(defun cheat_fu-string-replace (this withthat in)
  "replace THIS with WITHTHAT' in the string IN"
  (with-temp-buffer
    (insert in)
    (goto-char (point-min))
    (while (search-forward this nil t)
      (replace-match withthat nil t))
    (buffer-substring (point-min) (point-max))))

;;; ----------------------------------------------------------------------------
;;; - cheat_fu mode
;;;
(defvar cheat_fu-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "l" 'cheat_fu-list)
    (define-key keymap "s" 'cheat_fu-search)
    (define-key keymap "c" 'cheat_fu-convert)
    (define-key keymap "b" 'cheat_fu-buttonize-buffer)
    keymap))

;;;###autoload
(define-minor-mode cheat_fu-mode "Cheat_Fu Emulation Minor Mode"
  :lighter " cheat_fu"
  (use-local-map cheat_fu-mode-map)
  ; activate preferred completion library
  (dolist (mode *cheat_fu-completing-minor-mode-alist*)
    (if (eq (car mode) cheat_fu-completing-library)
        (funcall (cadr mode) t)
      (when (fboundp
             (cadr (assoc (car mode) *cheat_fu-completing-function-alist*)))
        (funcall (cadr mode) -1)))))

(provide 'cheat_fu)
;;; cheat_fu.el ends here