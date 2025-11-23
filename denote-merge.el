;;; denote-merge.el --- Denote extension to merge contents from one note into another -*- lexical-binding: t -*-

;; Copyright (C) 2025  Free Software Foundation, Inc.

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; Maintainer: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://github.com/protesilaos/denote-merge
;; Version: 0.0.0
;; Package-Requires: ((emacs "28.1") (denote "4.1.2"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This is an optional extension to the `denote' package.  It provides
;; commands and relevant user options to streamline the work of
;; merging contents from one Denote file to another.  This is for
;; users who periodically review their notes to add, remove, or
;; otherwise consolidate their accumulated knowledge.

;;; Code:

(require 'denote)

(defgroup denote-merge ()
  "Optional Denote extension to merge contents from one note into another."
  :group 'denote
  :link '(info-link "(denote) top")
  :link '(info-link "(denote-merge) top")
  :link '(url-link :tag "Denote homepage" "https://protesilaos.com/emacs/denote")
  :link '(url-link :tag "Denote Merge homepage" "https://protesilaos.com/emacs/denote-merge"))

(defcustom denote-merge-annotate-file "MERGED FILE"
  "Text to mark the contents of a file that was merged.
The command `denote-merge-file' inserts this text together with the
title of the file it merged to indicate where the merged file contents
start from.

When the value is nil or an empty string, `denote-merge-file' will not
add an annotation."
  :type '(choice
          (string :tag "Text to use")
          (const :tag "Do not use any text" nil))
  :package-version '(denote-merge . "0.1.0")
  :group 'denote-merge)

(defcustom denote-merge-annotate-region "MERGED REGION"
  "Text to mark a region that is merged from another file.
The command `denote-merge-region' inserts this text together with a link
to the file from where the region was taken.

When the value is nil or an empty string, `denote-merge-region' will not
add an annotation."
  :type '(choice
          (string :tag "Text to use")
          (const :tag "Do not use any text" nil))
  :package-version '(denote-merge . "0.1.0")
  :group 'denote-merge)

(defcustom denote-merge-save-buffers nil
  "When non-nil, automatically save buffers affected by merge operations.
The commands `denote-merge-file' and `denote-merge-region' operate
across files and modify the respective buffers.  The command
`denote-merge-file' checks whether the merged file had any backlinks
and, if so, proceeds to update the links to use the current identifier.
While the command `denote-merge-region' removes the active region from
one file and appends it to the other, establishing links between the two

When this user option is set to nil, those buffers are not saved: the
user must save them manually, perhaps to check the changes made to
them (also see `save-some-buffers').

Saving buffers automatically might leads to data loss.  When in doubt,
keep this option to its default nil value."
  :type 'boolean
  :package-version '(denote-merge . "0.1.0")
  :group 'denote-merge)

(defcustom denote-merge-kill-buffers nil
  "When non-nil, automatically saved buffers affected by merge operations.
Unsaved buffers are never killed automatically.

Killing buffers automatically might lead to data loss.  When in doubt,
keep this option to its default nil value."
  :type 'boolean
  :package-version '(denote-merge . "0.1.0")
  :group 'denote-merge)

(defun denote-merge--get-contents (file)
  "Get the contents of FILE without the front matter.
Work with the assumption that front matter is a continuous region of
lines starting at the top of the file and ending in an empty line."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "^$" nil t)
    (buffer-substring-no-properties (point) (point-max))))

(defun denote-merge--format-heading (string)
  "Create a heading with the given STRING.
Determine the syntax of a heading based on the major mode."
  (setq string (if (or (null denote-merge-annotate-file) (string-blank-p denote-merge-annotate-file))
                   string
                 (format "%s: %s" denote-merge-annotate-file string)))
  (pcase (derived-mode-p major-mode)
    ('org-mode (format "* %s\n\n" string))
    ('markdown-mode (format "# %s\n\n" string))
    ('text-mode (format "%s\n%s\n" string (make-string (length string) ?-)))
    (_ (error "Unsupported major mode `%s'; cannot format heading" major-mode))))

(defun denote-merge--kill-buffer (buffer)
  "Kill the BUFFER without asking any questions."
  (let ((kill-buffer-query-functions nil))
    (kill-buffer buffer)))

(defun denote-merge--replace-identifier-in-file (old-identifier new-identifier file save-p kill-p)
  "Replace every `denote:' link OLD-IDENTIFIER to NEW-IDENTIFIER in FILE.
When SAVE-P is non-nil, save the affected buffer.  When KILL-P is
non-nil, kill the buffer if it is saved.  Never kill an unsaved buffer."
  (condition-case error-data
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (let* ((file-type (denote-filetype-heuristics file))
               (regexp (denote--link-in-context-regexp file-type)))
          (while (re-search-forward regexp nil t)
            (replace-match new-identifier nil t nil 1)))
        (when save-p
          (save-buffer))
        (when kill-p
          (denote-merge--kill-buffer (current-buffer))))
    (:success
     (message "Updated `%s' to link to `%s' instead od `%s'"
              (propertize file 'face 'denote-faces-prompt-current-name)
              (propertize new-identifier 'face 'denote-faces-prompt-new-name)
              (propertize old-identifier 'face 'denote-faces-prompt-old-name)))
    ((quit user-error error)
     (message "Stopped at `%s' file because of this: `%s'" file error-data))))

(defun denote-merge--delete-file (file)
  "Delete the given FILE in accordance with `delete-by-moving-to-trash'."
  (when-let* ((buffer (get-file-buffer file)))
    (denote-merge--kill-buffer (current-buffer)))
  (delete-file file delete-by-moving-to-trash))

;;;###autoload
(defun denote-merge-file (destination-file source-file)
  "Merge the contents of SOURCE-FILE to DESTINATION-FILE.
Update any `denote:' links to SOURCE-FILE to point to DESTINATION-FILE
Save and maybe kill the affected buffers subject to the user options
`denote-merge-save-buffers' and `denote-merge-kill-buffers'.  Then
delete SOURCE-FILE.  Annotate the merged file contents with the text of
the user option `denote-merge-annotate-file'.

When called interactively, prompt from SOURCE-FILE as a file in the
variable `denote-directory'.  For the DESTINATION-FILE use the current
file if it is editable and has a known file type, else prompt for one as
well.

Make the DESTINATION-FILE condition what the SOURCE-FILE will be by
limiting the prompt for it to files that share the same file
extension (e.g. merge an Org file into another Org file).  This is to
ensure the contents are appropriate for the given major mode.

When called from Lisp, SOURCE-FILE and DESTINATION-FILE are strings
pointing to file paths.  Throw an error if their file extensions differ."
  (interactive
   (let* ((current-file (if (and buffer-file-name
                                 (denote-file-is-writable-and-supported-p buffer-file-name))
                            buffer-file-name
                          (denote-file-prompt nil "File to merge INTO")))
          (current-file-extension (denote-get-file-extension-sans-encryption current-file))
          (files-to-limit-to (regexp-quote current-file-extension)))
     (list
      current-file
      (denote-file-prompt files-to-limit-to "File to merge FROM"))))
  (unless (called-interactively-p 'interactive)
    (unless (string= (file-name-extension destination-file) (file-name-extension source-file))
      (error "The files do not share the same file extension"))
    (or (seq-every-p #'file-writable-p (list destination-file source-file))
        (error "Both files must be writable")))
  (let ((old-contents (denote-merge--get-contents source-file))
        (old-title (denote-retrieve-title-or-filename source-file (denote-filetype-heuristics source-file)))
        (old-identifier (denote-retrieve-filename-identifier source-file))
        (new-identifier (denote-retrieve-filename-identifier destination-file)))
    (with-current-buffer (find-file destination-file)
      (goto-char (point-max))
      (insert "\n\n")
      (insert (denote-merge--format-heading old-title))
      (insert old-contents))
    (let* ((old-backlinks-xrefs (denote-retrieve-xref-alist-for-backlinks old-identifier))
           (old-backlinks-files (mapcar #'car old-backlinks-xrefs)))
      (dolist (file old-backlinks-files)
        (denote-merge--replace-identifier-in-file old-identifier new-identifier file denote-merge-save-buffers denote-merge-kill-buffers))
      (denote-merge--delete-file source-file))
    (if denote-merge-save-buffers
        (message "Merged `%s' into `%s'"
                 (propertize source-file 'face 'denote-faces-prompt-old-name)
                 (propertize destination-file 'face 'denote-faces-prompt-new-name))
      (message "Merged `%s' into `%s'; do `%s' to save files that had the old file links"
               (propertize source-file 'face 'denote-faces-prompt-old-name)
               (propertize destination-file 'face 'denote-faces-prompt-new-name)
               (propertize "M-x save-some-buffers" 'face 'help-key-binding)))))

(defconst denote-merge-format-region-types
  '(plain plain-indented org-src org-quote org-example markdown-quote markdown-fenced-block)
  "Format region types.")

(defun denote-merge--region-format-link (other-file this-file)
  "Insert link to OTHER-FILE using the THIS-FILE type.
When ADD-NEWLINES is non-nil, insert two newlines after the link."
  (let ((file-type (denote-filetype-heuristics this-file)))
    (denote-format-link
     other-file
     (denote-get-link-description other-file file-type)
     file-type
     nil)))

(defun denote-merge--format-region (string region-type other-file this-file)
  "Format the STRING for `denote-merge-region' in accordance with TYPE.
REGION-TYPE is a symbol among `denote-merge-format-region-types'.  If
REGION-TYPE is unknown, fall back to `plain'.

Annotate STRING with `denote-merge-annotate-region' and include link to
OTHER-FILE given THIS-FILE file type."
  (when (or (null string) (string-blank-p string))
    (error "The string cannot be nil or blank"))
  (let* ((link (or (denote-merge--region-format-link other-file this-file) ""))
         (annotation (if (or (null denote-merge-annotate-region) (string-blank-p denote-merge-annotate-region))
                         (format "%s:\n\n" link)
                       (format "%s: %s\n\n" denote-merge-annotate-region link)))
         (string (if (string-suffix-p "\n" string)
                     string
                   (format "%s\n" string)))
         (org-block-fn (lambda (specifier)
                         (with-temp-buffer
                           (insert annotation)
                           (insert (format "#+begin_%s\n" specifier))
                           (insert string)
                           (insert (format "#+end_%s\n" specifier))
                           (buffer-string))))
         (quote-fn (lambda (prefix)
                     (with-temp-buffer
                       (insert prefix)
                       (insert string)
                       (goto-char (point-min))
                       (insert annotation)
                       (while (and (forward-line 1) (not (eobp)))
                         (insert prefix))
                       (buffer-string)))))
    (unless (memq region-type denote-merge-format-region-types)
      (setq region-type 'plain))
    (pcase region-type
      ('plain (with-temp-buffer
                (insert annotation)
                (insert string)
                (buffer-string)))
      ;; TODO 2025-11-21: Does it make sense for us to rely on
      ;; `tab-width' or should we have our own user option?
      ('plain-indented (funcall quote-fn (make-string tab-width ? )))
      ('org-src (funcall org-block-fn "src"))
      ('org-quote (funcall org-block-fn "quote"))
      ('org-example (funcall org-block-fn "example"))
      ('markdown-quote (funcall quote-fn "> "))
      ('markdown-fenced-block (with-temp-buffer
                                (insert annotation)
                                (insert "```\n")
                                (insert string)
                                (insert "```\n")
                                (buffer-string)))
      (_ (error "Unknown region type: `%S'" region-type)))))

(defvar denote-merge-format-region-type-prompt-history nil
  "Minibuffer history for `denote-merge-format-region-type-prompt'.")

(defun denote-merge-annotate-format-region-types (type)
  "Annotate completion candidate of TYPE for `denote-sequence-type-prompt'."
  (when-let* ((type (intern-soft type))
              (description (pcase type
                             ('plain "Insert the text as-is")
                             ('plain-indented "Insert the text as-is plus indentation")
                             ('org-src "Wrap the text in an Org SRC block")
                             ('org-quote "Wrap the text in an Org QUOTE block")
                             ('org-example "Wrap the text in an Org EXAMPLE block")
                             ('markdown-quote "Render the text as Markdown blockquote")
                             ('markdown-fenced-block "Wrap the text in Markdown fenced block delimiters"))))
    (format "%s-- %s"
            (propertize " " 'display '(space :align-to 25))
            (propertize description 'face 'completions-annotations))))

(defun denote-merge-format-region-type-prompt ()
  "Prompt for a type of formatting for a region.
Available types are those defined in `denote-merge-format-region-types'."
  (let ((default (car denote-merge-format-region-type-prompt-history))
        (completion-extra-properties
         (list :annotation-function #'denote-merge-annotate-format-region-types)))
    (intern
     (completing-read
      (format-prompt "Select type of region formatting" default)
      (denote--completion-table 'denote-merge-region-types denote-merge-format-region-types)
      nil t nil
      'denote-merge-format-region-type-prompt-history default))))

;;;###autoload
(defun denote-merge-region (destination-file &optional format-region-as)
  "Merge the currently active region DESTINATION-FILE.
If the current buffer has a file that conforms with the Denote
file-naming scheme, delete the region from it after merging it and then
create a link pointing to the DESTINATION-FILE.

With optional FORMAT-REGION-AS format the region according to one among
the symbols in `denote-merge-format-region-types'.  When called
interactively, FORMAT-REGION-AS is the prefix argument.  In that case,
prompt for the type of formatting.  When called from Lisp
FORMAT-REGION-AS is a symbol among `denote-merge-format-region-types'.

Annotate the merged text with the text of the user option
`denote-merge-annotate-region'.

Automatically save the affected buffer if the user option
`denote-merge-save-buffers' is non-nil.  Then automatically kill the
buffer if the user option `denote-merge-kill-buffers' is non-nil.  Only
kill the buffer if it is saved."
  (interactive
   (if (region-active-p)
       (list
        (denote-file-prompt nil "Merge region into FILE")
        (when current-prefix-arg
          (denote-merge-format-region-type-prompt)))
     (user-error "There is no active region; aborting")))
  (unless (called-interactively-p 'interactive)
    (unless (region-active-p)
      (error "There is no active region; aborting"))
    (unless (file-writable-p destination-file)
      (error "The file `%s' is not writable; aborting" destination-file)))
  (let* ((beg (region-beginning))
         (end (region-end))
         (text (buffer-substring-no-properties beg end))
         (source-file buffer-file-name))
    (delete-region beg end)
    (when-let* ((link (denote-merge--region-format-link destination-file source-file)))
      (insert link))
    (when denote-merge-save-buffers
      (save-buffer))
    (with-current-buffer (find-file-noselect destination-file)
      (goto-char (point-max))
      (insert "\n\n")
      (insert (denote-merge--format-region text format-region-as source-file destination-file))
      (when denote-merge-save-buffers
        (save-buffer))
      (when denote-merge-kill-buffers
        (denote-merge--kill-buffer (current-buffer))))))

(defmacro denote-merge-define-region-convenience-command (format-type)
  "Define a convenience variant of `denote-merge-region' with FORMAR-TYPE.
FORMAT-TYPE is a symbol one among `denote-merge-format-region-types'."
  (unless (memq format-type denote-merge-format-region-types)
    (error "The format type `%s' is not a member of `denote-merge-format-region-types'"))
  `(defun ,(intern (format "denote-merge-region-%s" format-type)) (destination-file)
     ,(format "Merge region into DESTINATION-FILE as %s." format-type)
     (interactive
      (list
       (denote-file-prompt nil ,(format "Merge region as %s into FILE" format-type))))
     (denote-merge-region destination-file ',format-type)))

;;;###autoload (autoload 'denote-merge-region-plain "denote-merge")
(denote-merge-define-region-convenience-command plain)

;;;###autoload (autoload 'denote-merge-region-plain-indented "denote-merge")
(denote-merge-define-region-convenience-command plain-indented)

;;;###autoload (autoload 'denote-merge-region-org-src "denote-merge")
(denote-merge-define-region-convenience-command org-src)

;;;###autoload (autoload 'denote-merge-region-org-quote "denote-merge")
(denote-merge-define-region-convenience-command org-quote)

;;;###autoload (autoload 'denote-merge-region-org-example "denote-merge")
(denote-merge-define-region-convenience-command org-example)

;;;###autoload (autoload 'denote-merge-region-markdown-quote "denote-merge")
(denote-merge-define-region-convenience-command markdown-quote)

;;;###autoload (autoload 'denote-merge-region-markdown-fenced-block "denote-merge")
(denote-merge-define-region-convenience-command markdown-fenced-block)

(provide 'denote-merge)
;;; denote-merge.el ends here
