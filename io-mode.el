;;; io-mode.el --- Major mode to edit Io language files in Emacs

;; Copyright (C) 2010 Sergei Lebedev

;; Version 0.1
;; Keywords: io major mode
;; Author: Sergei Lebedeb <superbobry@gmail.com>
;; URL: http://bitbucket.com/bobry/io-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; No documentation is availible at the moment, but a nice and clean
;; README is soon to come.

;;; Installation

;; In your shell:

;;     $ cd ~/.emacs.d/packges
;;     $ hg clone http://bitbucket.com/bobry/io-mode

;; In your emacs config:

;;     (add-to-list 'load-path "~/.emacs.d/packages/io-mode")
;;     (require 'io-mode)

;;; Thanks

;; Major thanks to defunkt's coffee-mode, which helped me to get over most
;; of the tought parts of writing a major mode.

;;; TODO:

;; - intepreter integration
;; - modify-syntax-entry for comments and strings
;; - replace ugly regexp with (rx ) macros?
;; - comment-dwim (and more inteligent comment handling)

;;; Code:

(require 'font-lock)
(require 'cl)

;;
;; Customizable Variables
;;

(defconst io-mode-version "0.1"
  "The version of this `io-mode'.")

(defgroup io nil
  "A major mode for editing Io language."
  :group 'languages)

(defcustom io-debug-mode nil
  "Whether to run in debug mode or not. Logs to `*Messages*'."
  :type 'boolean
  :group 'io)

(defcustom io-cleanup-whitespace t
  "Should we `delete-trailing-whitespace' on save? Probably."
  :type 'boolean
  :group 'io)

(defcustom io-tab-width tab-width
  "The tab width to use when indenting."
  :type 'integer
  :group 'io)

(defvar io-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

(defvar io-mode-map (make-keymap)
  "Keymap for Io major mode.")


;;
;; Macros
;;

(defun io-debug (string &rest args)
  "Print a message when in debug mode."
  (when io-debug-mode
      (apply 'message (append (list string) args))))

(defmacro io-line-as-string ()
  "Returns the current line as a string."
  `(buffer-substring (point-at-bol) (point-at-eol)))

;;
;; Define Language Syntax
;;

;; Self reference
(defvar io-self-regexp "\\<self\\>")

;; Operators
(defvar io-operators-regexp
  (regexp-opt
   '("++" "--" "*" "/" "%" "^" "+" "-" ">>"
     "<<" ">" "<" "<=" ">=" "==" "!=" "&"
     "^" ".." "|" "&&" "||" "!=" "+=" "-="
     "*=" "/=" "<<=" ">>=" "&=" "|=" "%="
     "=" ":=" "<-" "<->" "->")
   t))

;; Booleans
(defvar io-boolean-regexp "\\b\\(true\\|false\\|nil\\)\\b")

;; Prototypes
(defvar io-prototypes-regexp
  (format "\\<%s\\>"
          (regexp-opt
           '("Array" "Block" "Box" "Buffer" "CFunction"
             "Date" "Error" "File" "Importer" "List" "Lobby"
             "Locals" "Map" "Message" "Number" "Object"
             "Protos" "Regex" "String" "WeakLink")
           t)))

;; Messages
(defvar io-messages-regexp
  (format "\\<%s\\>"
          (regexp-opt
           '("activate" "activeCoroCount" "and" "asString"
             "block" "break" "catch" "clone" "collectGarbage"
             "compileString" "continue" "do" "doFile" "doMessage"
             "doString" "else" "elseif" "exit" "for" "foreach"
             "forward" "getSlot" "getEnvironmentVariable"
             "hasSlot" "if" "ifFalse" "ifNil" "ifTrue"
             "isActive" "isNil" "isResumable" "list"
             "message" "method" "or" "parent" "pass" "pause"
             "perform" "performWithArgList" "print" "proto"
             "raise" "raiseResumable" "removeSlot" "resend"
             "resume" "return" "schedulerSleepSeconds"
             "self" "sender" "setSchedulerSleepSeconds"
             "setSlot" "shallowCopy" "slotNames" "super"
             "system" "then" "thisBlock" "thisContext"
             "thisMessage" "try" "type" "uniqueId" "updateSlot"
             "wait" "while" "write" "yield")
           t)))

;; Comments
(defvar io-comments-regexp "\\(\\(#\\|//\\).*$\\|/\\*\\(.\\|[\r\n]\\)*?\\*/\\)")

;; Create the list for font-lock. Each class of keyword is given a
;; particular face.
(defvar io-font-lock-keywords
  ;; Note: order here matters!
  `((,io-self-regexp . font-lock-variable-name-face)
    (,io-comments-regexp . font-lock-comment-face)
    (,io-operators-regexp . font-lock-builtin-face)
    (,io-boolean-regexp . font-lock-constant-face)
    (,io-prototypes-regexp . font-lock-type-face)
    (,io-messages-regexp . font-lock-keyword-face)))


;;
;; Helper Functions
;;

(defun io-before-save ()
  "Hook run before file is saved. Deletes whitespace if `io-cleanup-whitespace' is non-nil."
  (when io-cleanup-whitespace
    (delete-trailing-whitespace)))

;;
;; Indentation
;;

(defun io-indent-line ()
  "Indent current line as Io source."
  (interactive)

  (if (= (point) (point-at-bol))
      (insert-tab)
    (save-excursion
      (let ((prev-indent 0) (cur-indent 0))
        ;; Figure out the indentation of the previous
        ;; and current lines.
        (setq prev-indent (io-previous-indent)
              cur-indent (current-indentation))

        ;; Shift one column to the left
        (beginning-of-line)
        (insert-tab)

        (when (= (point-at-bol) (point))
          (forward-char io-tab-width))

        ;; We're too far, remove all indentation.
        (when (> (- (current-indentation) prev-indent) io-tab-width)
          (backward-to-indentation 0)
          (delete-region (point-at-bol) (point)))))))

(defun io-previous-indent ()
  "Return the indentation level of the previous non-blank line."
  (save-excursion
    (forward-line -1)
    (if (bobp)
        0
      (progn
        (while (io-line-empty-p) (forward-line -1))
        (current-indentation)))))

(defun io-line-empty-p ()
  "Is this line empty? Returns non-nil if so, nil if not."
  (or (bobp)
      (string-match "^\\s *$" (io-line-as-string))))

(defun io-newline-and-indent ()
  "Inserts a newline and indents it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)) (indent-next nil))
    (newline)
    (insert-tab (/ prev-indent io-tab-width)))

  ;; Last line was a comment so this one should probably be,
  ;; too. Makes it easy to write multi-line comments (like the
  ;; one I'm writing right now).
  (when (io-previous-line-is-comment)
    ;; Using `match-string' is probably not obvious, but current
    ;; implementation of `io-previous-is-comment' is using `looking-at',
    ;; which modifies match-data variables.
    (insert (match-string 0))))


;;
;; Comments
;;

(defun io-previous-line-is-comment ()
  "Returns `t' if previous line is a comment."
  (save-excursion
    (forward-line -1)
    (io-line-is-comment)))

(defun io-line-is-comment ()
  "Returns `t' if current line is a comment."
  (save-excursion
    (backward-to-indentation 0)
    (looking-at "\\(#\\|//\\)+\s*")))


;;
;; Define Major Mode
;;

;;;###autoload
(define-derived-mode io-mode fundamental-mode
  "io-mode"
  "Major mode for editing Io language..."

  (define-key io-mode-map (kbd "\C-m") 'io-newline-and-indent)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((io-font-lock-keywords)))

  (setq comment-start "//")

  ;; no tabs
  (setq indent-tabs-mode nil)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'io-indent-line
        io-tab-width tab-width) ;; Just in case...

  ;; hooks
  (set (make-local-variable 'before-save-hook) 'io-before-save))

(provide 'io-mode)


;;
;; On Load
;;

;; Run io-mode for files ending in .io.
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.io$" . io-mode))