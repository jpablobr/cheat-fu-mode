;; cheat_fu.el
;; Time-stamp: <2011-04-25 03:24:03 jpablobr>

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

;; Customization
(defcustom cheat_fu-root nil
  "Used internally for defining cheat_fu_sheets root path."
  :group 'cheat_fu
  :type 'string)

(defvar *cheat_fu-to-include* "*.ronn"
  "Regexp of files to exclude from `cheat_fu-sheets'.")

(defvar cheat_fu-completing-library 'ido
  "The library `cheat_fu-sheets' should use for
  completing sheets (`ido' by default)")

(defvar cheat_fu-find-files-command "find \"%s\" -type f -name "
  "The command used to find sheets. %s will be replaced with the cheat_fu-root.")

(defvar *cheat_fu-completing-function-alist* '((ido ido-completing-read)
                                               (icicles  icicle-completing-read)
                                               (none completing-read))
  "The function to call to read sheets.")

(defvar *cheat_fu-completing-minor-mode-alist*
  `((ido ,(lambda (a) (progn (ido-mode a) (setq ido-enable-flex-matching t))))
    (icicles ,(lambda (a) (icy-mode a)))
    (none ,(lambda (a) ())))
  "The list of functions to enable and disable completing minor modes")

(defun cheat_fu-ido-fix ()
  "Add up/down keybindings for ido."
  (define-key ido-completion-map [up] 'ido-prev-match)
  (define-key ido-completion-map [down] 'ido-next-match))

(defun cheat_fu-sheets()
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
     (cheat_fu-string-replace "%s" root cheat_fu-find-files-command)
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

(provide 'cheat_fu)
;;; cheat_fu.el ends here
