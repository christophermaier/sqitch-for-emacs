;;; sqitch-mode.el --- Minor-mode for interacting with Sqitch SQL scripts

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

;; Provides basic navigation across files of a Sqitch project.
;; See http://www.sqitch.org for details
;;
;; To use this file, make sure that the following code (or its
;; equivalent) is in your Emacs configuration:
;;
;;     (add-to-list 'load-path "/path/to/sqitch-for-emacs")
;;     (require 'sqitch-mode)
;;
;; When you open a SQL file (i.e., '*.sql') in `sql-mode` that is in a
;; 'deploy', 'verify', or 'revert' directory, and that has a
;; 'sqitch.plan' file as a sibling of those directories, the
;; 'sqitch-mode' minor mode will be activated automatically.

;;; Code:

(defun sqitch-find-script (script-type)
  "Opens the corresponding SCRIPT-TYPE Sqitch script for
  the current Sqitch script file.

  SCRIPT-TYPE should be one of \"deploy\", \"verify\", or
  \"revert\"."
  (let* ((current-script (buffer-file-name))
         (current-dir (file-name-directory current-script))
         (base-name (file-name-nondirectory current-script))
         (sqitch-file (expand-file-name
                       (concat current-dir
                               (file-name-as-directory "..")
                               (file-name-as-directory script-type)
                               base-name))))
    (if (file-exists-p sqitch-file)
        (find-file sqitch-file)
      (message "Could not find a Sqitch %s script for '%s'" script-type (buffer-file-name)))))

(defun sqitch-find-deploy-script ()
 "Open the corresponding deploy script for the current Sqitch
  script."
  (interactive)
  (sqitch-find-script "deploy"))

(defun sqitch-find-verify-script ()
 "Open the corresponding verify script for the current Sqitch
  script."
  (interactive)
  (sqitch-find-script "verify"))

(defun sqitch-find-revert-script ()
 "Open the corresponding revert script for the current Sqitch
  script."
  (interactive)
  (sqitch-find-script "revert"))

(defun sqitch-plan-file ()
  "Return the path to the sqitch.plan for the current buffer, if it exists"
  (let ((proj-dir (locate-dominating-file (buffer-file-name) "sqitch.plan")))
    (if proj-dir
        (concat (file-name-as-directory proj-dir) "sqitch.plan"))))

(defun sqitch-find-plan ()
  "Open the sqitch.plan file for the Sqitch script, if it
  exists."
  (interactive)
  (let ((plan (sqitch-plan-file)))
    (if plan
        (let (changeset change-re tag)
          (setq changeset
                (split-string (file-name-base (buffer-file-name)) "@" t split-string-default-separators))
          (setq change-re (concat "^\s*" (car changeset) "\s+"))
          (setq tag (car (cdr changeset)))
          (find-file plan)
          (if tag
              (progn
                (goto-char (point-min))
                (search-forward-regexp (concat "^\s*@" tag "\s+"))
                (search-forward-regexp change-re))
            (goto-char (point-max))
            (search-backward-regexp change-re))
          (back-to-indentation))
      (message "Could not find a Sqitch plan file for '%s'" (file-name-base (buffer-file-name))))))

(defvar sqitch-mode-keymap nil "sqitch-mode keymap")
(if sqitch-mode-keymap
    nil
  (setq sqitch-mode-keymap (make-sparse-keymap))
  (define-key sqitch-mode-keymap (kbd "C-c d") 'sqitch-find-deploy-script)
  (define-key sqitch-mode-keymap (kbd "C-c C-d") 'sqitch-find-deploy-script)

  (define-key sqitch-mode-keymap (kbd "C-c v") 'sqitch-find-verify-script)
  (define-key sqitch-mode-keymap (kbd "C-c C-v") 'sqitch-find-verify-script)

  (define-key sqitch-mode-keymap (kbd "C-c r") 'sqitch-find-revert-script)
  (define-key sqitch-mode-keymap (kbd "C-c C-r") 'sqitch-find-revert-script)

  (define-key sqitch-mode-keymap (kbd "C-c p") 'sqitch-find-plan)
  (define-key sqitch-mode-keymap (kbd "C-c C-p") 'sqitch-find-plan))

;;;###autoload
(define-minor-mode sqitch-mode
  "Minor mode for interacting with Sqitch SQL scripts."
  :init-value nil
  :lighter " sqitch"
  :keymap sqitch-mode-keymap)

(defun sqitch-script-p ()
  "Indicate whether the current buffer corresponds to a deploy,
   verify, or revert script in a Sqitch project"
  (and (sqitch-plan-file)
       (member (file-name-base (directory-file-name
                                (file-name-directory (buffer-file-name))))
               '("deploy" "verify" "revert"))))

;;;###autoload
(defun sqitch-maybe-enable-mode ()
  "Call this as an sql-mode-hook to determine whether or not to
   turn on the sqitch-mode for the current file."
  (interactive)
  (if (sqitch-script-p)
      (sqitch-mode)
    (message "The current buffer is NOT a Sqitch script!")))

;;;###autoload
(add-hook 'sql-mode-hook 'sqitch-maybe-enable-mode)

(provide 'sqitch-mode)

;;; sqitch-mode.el ends here
