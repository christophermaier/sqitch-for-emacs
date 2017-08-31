;;; sqitch-plan-mode.el --- Major mode for interacting with Sqitch plan files

;; Copyright (C) 2014 Christopher Maier

;; Author: Christopher Maier <christopher.maier@gmail.com>
;; Maintainer: Christopher Maier <christopher.maier@gmail.com>
;; Version: 0.0.1
;; Keywords: sql, sqitch
;; URL: https://github.com/christophermaier/sqitch-for-emacs

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Provides basic navigation across files of a Sqitch project from a
;; 'sqitch.plan' file. See http://www.sqitch.org for details.
;;
;; To use this file, make sure that the following code (or its
;; equivalent) is in your Emacs configuration:
;;
;;     (add-to-list 'load-path "/path/to/sqitch-for-emacs")
;;     (require 'sqitch-plan-mode)
;;
;; Files named 'sqitch.plan' are automatically opened in this mode.

;;; Code:

(require 'derived)
(require 'thingatpt)

(defun sqitch-plan-maybe-changeset-at-point ()
  "Crude way of (maybe) getting the changeset at point. Not at
  all guaranteed to actually *be* a changeset, absent a more robust
  implementation."
  (thing-at-point 'symbol))

(defun sqitch-plan-find-script (script-type)
  "Opens the SCRIPT-TYPE Sqitch script for the changeset
  described by the current line of a Sqitch plan file.

  SCRIPT-TYPE should be one of \"deploy\", \"verify\", or
  \"revert\"."
  (let ((changeset (sqitch-plan-maybe-changeset-at-point)))
    (if changeset
        (let* ((script-dir (concat (file-name-directory (buffer-file-name)) script-type))
               (script (concat changeset ".sql"))
               (script-file (concat (file-name-as-directory script-dir)
                                    script)))
          (if (file-exists-p script-file)
              (find-file script-file)
            (message "'%s' is not a sqitch changeset" changeset))))))

(defun sqitch-plan-find-deploy-script ()
  "Open the deploy script for the changeset described by the
  current line of a Sqitch plan file."
  (interactive)
  (sqitch-plan-find-script "deploy"))

(defun sqitch-plan-find-verify-script ()
  "Open the verify script for the changeset described by the
  current line of a Sqitch plan file."
  (interactive)
  (sqitch-plan-find-script "verify"))

(defun sqitch-plan-find-revert-script ()
  "Open the revert script for the changeset described by the
  current line of a Sqitch plan file."
  (interactive)
  (sqitch-plan-find-script "revert"))

(defvar sqitch-plan-mode-map nil "sqitch-plan-mode keymap")

(if sqitch-plan-mode-map
    nil
  (setq sqitch-plan-mode-map (make-sparse-keymap))
  (define-key sqitch-plan-mode-map (kbd "C-c d") 'sqitch-plan-find-deploy-script)
  (define-key sqitch-plan-mode-map (kbd "C-c C-d") 'sqitch-plan-find-deploy-script)

  (define-key sqitch-plan-mode-map (kbd "C-c v") 'sqitch-plan-find-verify-script)
  (define-key sqitch-plan-mode-map (kbd "C-c C-v") 'sqitch-plan-find-verify-script)

  (define-key sqitch-plan-mode-map (kbd "C-c r") 'sqitch-plan-find-revert-script)
  (define-key sqitch-plan-mode-map (kbd "C-c C-r") 'sqitch-plan-find-revert-script))

(defvar sqitch-plan-highligts nil "first element for `font-lock-defaults'")
(setq sqitch-plan-highligts
      '(
        ;; pragmas
        ("^[[:space:]]*\\(%\\)[[:space:]]*\\([^=]+\\)[[:space:]]*\\(=\\)[[:space:]]*\\([^\n]+\\)[[:space:]]*$" .
         ((1 font-lock-type-face)
          (2 font-lock-variable-name-face)
          (3 font-lock-type-face)
          (4 font-lock-string-face)
          ))

        ;; timestamp
        ("[[:space:]]+\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}T[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}Z\\)[[:space:]]+" .
         (1 font-lock-builtin-face))
        ;; requirements
        ("[[:space:]]+\\(\\[[^]]+\\][[:space:]]+\\)" . (1 font-lock-keyword-face))
        ;; tags
        ("^[[:space:]]*\\(@[^ ]+\\)" . (1 font-lock-warning-face))
        ;; changes
        ("^[[:space:]]*\\([^%@][^ ]+\\)" . (1 font-lock-constant-face))
        ;; author
        ("Z[[:space:]]+\\([^>]+>\\)" . (1 font-lock-string-face))
        ))

(defvar sqitch-plan-mode-syntax-table nil "Syntax table for `sqitch-plan-mode'")
(setq sqitch-plan-mode-syntax-table
      (let ((synTable (make-syntax-table)))
        ;; perl style comment: "# …"
        (modify-syntax-entry ?# "<" synTable)
        (modify-syntax-entry ?\n ">" synTable)
        synTable))

;;;###autoload
(define-derived-mode sqitch-plan-mode text-mode "sqitch-plan"
  "Major mode for interacting with Sqitch plan files.

\\{sqitch-plan-mode-map}"
  (setq font-lock-defaults '(sqitch-plan-highligts)))

;;;###autoload
(add-to-list 'auto-mode-alist '("sqitch\.plan$" . sqitch-plan-mode))

(provide 'sqitch-plan-mode)

;;; sqitch-plan-mode.el ends here
