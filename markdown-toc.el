;;; markdown-toc.el --- A simple TOC generator for markdown file -*- lexical-binding: t; -*-
;; Copyright (C) 2014-2020 Antoine R. Dumont (@ardumont)

;; Author: Antoine R. Dumont (@ardumont)
;;         Karim Aziiev <karim.aziiev@gmail.com>
;; Maintainer: Antoine R. Dumont (@ardumont)
;; URL: https://github.com/KarimAziev/markdown-toc
;; Created: 24th May 2014
;; Version: 0.2.0
;; Keywords: markdown, toc, tools,
;; Package-Requires: ((markdown-mode "2.8") (emacs "28.1"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Generate a TOC from a markdown file: M-x markdown-toc-generate-toc
;; This will compute the TOC at insert it at current position.
;; Update existing TOC: C-u M-x markdown-toc-generate-toc

;; Here is a possible output:
;; <!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->x
;; **Table of Contents**

;; - [some markdown page title](#some-markdown-page-title)
;; - [main title](#main-title)
;;     - [Sources](#sources)
;;         - [Marmalade (recommended)](#marmalade-recommended)
;;         - [Melpa-stable](#melpa-stable)
;;         - [Melpa (~snapshot)](#melpa-~snapshot)
;;     - [Install](#install)
;;         - [Load org-trello](#load-org-trello)
;;     - [Alternative](#alternative)
;;         - [Git](#git)
;;         - [Tar](#tar)
;; - [another title](#another-title)
;;     - [with](#with)
;;     - [some](#some)
;; - [heading](#heading)
;;
;; <!-- markdown-toc end -->

;; Install - M-x package-install RET markdown-toc RET

;;; Code:

(eval-when-compile
  (require 'subr-x))

(require 'markdown-mode)

(defconst markdown-toc--toc-version "0.1.5" "Current version installed.")

(defgroup markdown-toc nil
  "A simple TOC generator for markdown file."
  :group 'markdown)

(defcustom markdown-toc-list-item-marker
  "-"
  "List item marker that should be used.
Example: '-' for unordered lists or '1.' for ordered lists."
  :type '(choice
          (string :tag "Unordered list header" "-")
          (string :tag "Ordered list header" "1."))
  :group 'markdown-toc)

(defcustom markdown-toc-header-toc-start "<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->"
  "Beginning delimiter comment.
If empty or nil, `markdown-toc-header-toc-title' will be used instead."
  :group 'markdown-toc
  :type '(radio
          string
          (const :tag "None" nil)))

(defcustom markdown-toc-header-toc-title
  "**Table of Contents**"
  "Title comment on TOC header."
  :group 'markdown-toc
  :type 'string)

(defcustom markdown-toc-header-toc-end "<!-- markdown-toc end -->"
  "Optional ending delimiter comment."
  :group 'markdown-toc
  :type '(radio
          string
          (const :tag "None" nil)))

(defcustom markdown-toc-indentation-space 2
  "Let the user decide the indentation level."
  :group 'markdown-toc
  :type 'integer)

(defcustom markdown-toc-quote t
  "Whether to quote each line of the generated table of contents.

When set to t, the generated Table of Contents (TOC) entries will be
formatted as blockquotes in Markdown.

When set to nil, the TOC entries will be formatted as regular list
items without blockquotes.

This variable controls the visual style of the TOC in the generated
Markdown document."
  :group 'markdown-toc
  :type 'boolean)

(defcustom markdown-toc-user-toc-structure-manipulation-fn (lambda
                                                             (toc-structure)
                                                             toc-structure)
  "User crafted function to manipulate toc-structure as user sees fit.

The toc-structure has the following form:

\\='((0 . \"some markdown page title\")
  (0 . \"main title\")
  (1 . \"Sources\")
  (2 . \"Marmalade (recommended)\")
  (2 . \"Melpa-stable\")
  (2 . \"Melpa (~snapshot)\")
  (1 . \"Install\")
  (2 . \"Load org-trello\")
  (2 . \"Alternative\")
  (3 . \"Git\")
  (3 . \"Tar\")
  (0 . \"another title\")
  (1 . \"with\")
  (1 . \"some\")
  (1 . \"heading\"))

If the user wanted to remove the first element, it could for
example define the following function:
  (custom-set-variables
    \\='(markdown-toc-user-toc-structure-manipulation-fn \\='cdr))

Default to identity function (do nothing)."
  :group 'markdown-toc
  :type 'function)

(defconst markdown-toc--toc-item-re
  "^\\(>?[\s\t]*\\)-[ ]\\(\\[\\([^]]+\\)]\\)")

(defun markdown-toc-log-msg (args)
  "Log message ARGS."
  (apply #'message (format "markdown-toc - %s" (car args)) (cdr args)))

;;;###autoload
(defun markdown-toc-version ()
  "Markdown-toc version."
  (interactive)
  (message "markdown-toc version: %s" markdown-toc--toc-version))

(defun markdown-toc--compute-toc-structure-from-level (level menu-index)
  "Given a LEVEL and a MENU-INDEX, compute the toc structure."
  (when menu-index
    (let* ((fst   (car menu-index))
           (tail  (cdr menu-index))
           (ttail (if (integerp tail) nil (cdr tail))))
      (cons `(,level . ,fst)
            (apply #'append
                   (mapcar
                    #'(lambda
                        (it)
                        (markdown-toc--compute-toc-structure-from-level
                         (+ 1 level)
                         it))
                    ttail))))))

(defun markdown-toc--compute-toc-structure (imenu-index)
  "Given a IMENU-INDEX, compute the TOC structure."
  (apply #'append
         (mapcar
          #'(lambda
              (it)
              (markdown-toc--compute-toc-structure-from-level 0 it))
          imenu-index)))

(defun markdown-toc--symbol (sym n)
  "Compute the repetition of a symbol SYM N times as a string."
  (let ((it
         (and (>= n 0)
              (make-list n sym))))
    (mapconcat #'identity it "")))

(defconst markdown-toc--dash-protection-symbol
  "09876543214b825dc642cb6eb9a060e54bf8d69288fbee49041234567890"
  "String used to protect dashes in Markdown table of contents.")

(defconst markdown-toc--underscore-protection-symbol "afec96cafb7bc4b0e216bfe86db4bd6c4aab44bca19dd9999b11e162f595d711"
  "Symbol used to protect underscores in Markdown table of contents.")

(defun markdown-toc--str-replace (old new s)
  "Replace occurrences of OLD with NEW in string S.

Argument OLD is the substring to be replaced.

Argument NEW is the replacement substring.

Argument S is the original string where replacements will occur."
  (declare (pure t) (side-effect-free t))
  (replace-regexp-in-string (regexp-quote old) new s t t))

(defun markdown-toc--to-link (title &optional count)
  "Return a formatted Markdown link from TITLE, optionally appending COUNT.

Argument TITLE is the text to be converted into a Markdown link.

Optional argument COUNT is a number used to disambiguate duplicate titles,
defaulting to 0."
  (let ((count (or count 0)))
    (format "[%s](#%s%s)" title
            (thread-last title
                         string-trim
                         downcase
                         (markdown-toc--str-replace
                          "-"
                          markdown-toc--dash-protection-symbol)
                         (markdown-toc--str-replace
                          "_"
                          markdown-toc--underscore-protection-symbol)
                         (replace-regexp-in-string
                          "[[:punct:]]" "")
                         (markdown-toc--str-replace
                          markdown-toc--dash-protection-symbol
                          "-")
                         (markdown-toc--str-replace
                          markdown-toc--underscore-protection-symbol
                          "_")
                         (markdown-toc--str-replace " " "-"))
            (if (> count 0)
                (concat "-" (number-to-string count))
              ""))))

(defun markdown-toc--count-duplicate-titles (toc-structure)
  "Count duplicate titles in a table of contents structure.

Argument TOC-STRUCTURE is a list of cons cells where each cell contains an
indent level and a title string."
  (seq-map-indexed (lambda (n index)
                     (let* ((indent (car n))
                            (title (cdr n))
                            (count
                             (let ((result 0))
                               (let ((list
                                      (seq-take
                                       toc-structure
                                       (+ index 1)))
                                     (i 0))
                                 (while list
                                   (let ((it
                                          (car-safe
                                           (prog1 list
                                             (setq list
                                                   (cdr list)))))
                                         (it-index i))
                                     (ignore it it-index)
                                     (if
                                         (string= title
                                                  (cdr it))
                                         (progn
                                           (setq result
                                                 (1+ result)))))
                                   (setq i
                                         (1+ i))))
                               result)))
                       (list indent title (- count 1))))
                   toc-structure))

(defun markdown-toc--to-markdown-toc (level-title-toc-list)
  "Given LEVEL-TITLE-TOC-LIST, a list of pair level, title, return a TOC string."
  (mapconcat (lambda
               (it)
               (let ((nb-spaces
                      (* markdown-toc-indentation-space
                         (car it)))
                     (title
                      (cadr it))
                     (count (caddr it)))
                 (if markdown-toc-quote
                     (format "> %s%s %s"
                             (markdown-toc--symbol " " nb-spaces)
                             markdown-toc-list-item-marker
                             (markdown-toc--to-link title count))
                   (format "%s%s %s"
                           (markdown-toc--symbol " " nb-spaces)
                           markdown-toc-list-item-marker
                           (markdown-toc--to-link title count)))))
             (markdown-toc--count-duplicate-titles level-title-toc-list)
             "\n"))

(defun markdown-toc--bounds-of-toc ()
  "Return the start and end positions of the table of contents."
  (save-excursion
    (goto-char (point-min))
    (when-let* ((start
                 (when (re-search-forward
                        (concat "^"
                                (regexp-quote
                                 (if
                                     (or
                                      (not
                                       markdown-toc-header-toc-start)
                                      (string-empty-p
                                       markdown-toc-header-toc-start))
                                     markdown-toc-header-toc-title
                                   markdown-toc-header-toc-start)))
                        nil t)
                   (match-beginning 0)))
                (end (if
                         (and markdown-toc-header-toc-end
                              (not (string-empty-p
                                    markdown-toc-header-toc-end)))
                         (re-search-forward
                          (concat "^"
                                  (regexp-quote markdown-toc-header-toc-end))
                          nil t)
                       (while (and (progn
                                     (or
                                      (looking-at "[\s\t]*\n")
                                      (looking-at markdown-toc--toc-item-re)))
                                   (zerop (forward-line))))
                       (when (looking-back "^[\s\t]*\n" 0)
                         (forward-char -1))
                       (point))))
      (when (and start end)
        (cons start end)))))


(defun markdown-toc--generate-toc (toc-structure)
  "Given a TOC-STRUCTURE, compute a new toc."
  (markdown-toc--compute-full-toc
   (markdown-toc--to-markdown-toc toc-structure)))


(defun markdown-toc--compute-full-toc (toc)
  "Given the TOC's content, compute the full toc with comments and title."
  (format "%s\n\n%s\n\n%s\n\n%s\n"
          (or markdown-toc-header-toc-start "")
          markdown-toc-header-toc-title
          toc
          (or markdown-toc-header-toc-end "")))

(defun markdown-toc--insert-toc ()
  "Insert a generated table of contents into the current buffer."
  (insert
   (markdown-toc--generate-toc
    (funcall markdown-toc-user-toc-structure-manipulation-fn
             (markdown-toc--compute-toc-structure
              (funcall imenu-create-index-function))))))

;;;###autoload
(defun markdown-toc-generate-toc (&optional replace-toc-p)
  "Generate a TOC for markdown file at current point.
Deletes any previous TOC.
If called interactively with prefix arg REPLACE-TOC-P, replaces previous TOC."
  (interactive "P")
  (save-excursion
    (when-let* ((bounds (and replace-toc-p
                             (markdown-toc--bounds-of-toc))))
      (goto-char (car bounds))
      (delete-region (car bounds)
                     (cdr bounds)))
    (markdown-toc--insert-toc)))



(defun markdown-toc--refresh-toc-maybe ()
  "Refresh the Table of Contents if it is already present."
  (when-let* ((bounds (markdown-toc--bounds-of-toc)))
    (goto-char (car bounds))
    (delete-region (car bounds)
                   (cdr bounds))
    (markdown-toc--insert-toc)))

;;;###autoload
(defun markdown-toc-generate-or-refresh-toc ()
  "Generate or refresh the Table of Contents in the current markdown file."
  (interactive)
  (markdown-toc-generate-toc t))

;;;###autoload
(defun markdown-toc-refresh-toc ()
  "Refreshes an already generated TOC."
  (interactive)
  (markdown-toc--refresh-toc-maybe))

;;;###autoload
(defun markdown-toc-delete-toc ()
  "Deletes a previously generated TOC."
  (interactive)
  (when-let* ((bounds (and (markdown-toc--bounds-of-toc))))
    (save-excursion
      (delete-region (car bounds)
                     (cdr bounds)))))

(defun markdown-toc--link-title-at-point ()
  "Return the title and indentation level of a markdown link at point."
  (save-excursion
    (beginning-of-line)
    (when (looking-at markdown-toc--toc-item-re)
      (let ((indent-str (match-string-no-properties 1))
            (title (match-string-no-properties 3)))
        (let ((indent (if (string-prefix-p "> " indent-str)
                          (1- (1- (length indent-str)))
                        (length indent-str))))
          (cons
           (when (zerop (% indent markdown-toc-indentation-space))
             (+ 1 (/ indent markdown-toc-indentation-space)))
           title))))))


(defun markdown-toc--link-heading-pos-at-point ()
  "Find and return the position of a markdown heading matching the title."
  (pcase-let ((`(,level . ,title)
               (markdown-toc--link-title-at-point)))
    (when (and level title)
      (let* ((search-fn (lambda (&optional fold-search)
                          (save-excursion
                            (let ((case-fold-search fold-search))
                              (goto-char (point-max))
                              (re-search-backward (concat (make-string level ?#)
                                                          "[\s\t]"
                                                          (regexp-quote title))
                                                  nil t 1))))))
        (or
         (funcall search-fn nil)
         (funcall search-fn t))))))
;;;###autoload
(defun markdown-toc-follow-link-at-point ()
  "On a given toc link, navigate to the current markdown header.
If the toc is misindented (according to `markdown-toc-indentation-space')
or if not on a toc link, this does nothing."
  (interactive)
  (when-let* ((pos (markdown-toc--link-heading-pos-at-point)))
    (goto-char pos)))


(defun markdown-toc--follow-link (link)
  "Navigate to the position of the given LINK in the markdown buffer.

Argument LINK is a string representing the link to follow."
  (let* ((bounds-of-toc (markdown-toc--bounds-of-toc))
         (re (regexp-quote link))
         (pos
          (save-excursion
            (goto-char (if bounds-of-toc
                           (car bounds-of-toc)
                         (point-min)))
            (or
             (and bounds-of-toc
                  (when (re-search-forward re (cdr bounds-of-toc) t 1)
                    (markdown-toc--link-heading-pos-at-point)))
             (when (string-prefix-p "#" link)
               (let ((case-fold-search t)
                     (regex (concat "^#[#]* "
                                    (replace-regexp-in-string
                                     "-" "[-\s]"
                                     (substring-no-properties
                                      (regexp-quote link)
                                      1))
                                    "\n")))
                 (when (re-search-forward regex nil t 1)
                   (match-beginning 0))))))))
    (when pos
      (push-mark)
      (when (goto-char pos)
        (recenter)))
    pos))

(defun markdown-toc--bug-report ()
  "Compute the bug report for the user to include."
  (require 'find-func)
  (string-join
   (list "Please:"
         "- Describe your problem with clarity and conciceness (cf. https://www.gnu.org/software/emacs/manual/html_node/emacs/Understanding-Bug-Reporting.html)"
         "- Explicit your installation choice (melpa, marmalade, el-get, tarball, git clone...)."
         "- Report the following message trace inside your issue." ""
         "System information:"
         (format "- system-type: %s" system-type)
         (format "- locale-coding-system: %s" locale-coding-system)
         (format "- emacs-version: %s"
                 (emacs-version))
         (format "- markdown-mode path: %s"
                 (if
                     (fboundp 'find-library-name)
                     (progn
                       (find-library-name "markdown-mode"))))
         (format "- markdown-toc version: %s" markdown-toc--toc-version)
         (format "- markdown-toc path: %s"
                 (if
                     (fboundp 'find-library-name)
                     (progn
                       (find-library-name "markdown-toc")))))
   "\n"))

;;;###autoload
(defun markdown-toc-bug-report (&optional open-url)
  "Display a bug report message.
When OPEN-URL is filled, with universal argument (`C-u') is used,
opens new issue in markdown-toc's github tracker."
  (interactive "P")
  (when open-url
    (browse-url "https://github.com/KarimAziev/markdown-toc/issues/new"))
  (markdown-toc-log-msg (list (markdown-toc--bug-report))))


;;;###autoload
(define-minor-mode markdown-toc-mode
  "Enable automatic Table of Contents generation and link navigation in Markdown.

Enable automatic generation and updating of a Table of Contents (TOC) for
Markdown files. When enabled, refresh the TOC before saving the buffer and allow
navigation to TOC links."
  :init-value nil
  :lighter " mt"
  :group 'markdown-toc
  (remove-hook 'before-save-hook #'markdown-toc--refresh-toc-maybe 'local)
  (remove-hook 'markdown-follow-link-functions #'markdown-toc--follow-link
               'local)
  (when markdown-toc-mode
    (add-hook 'before-save-hook #'markdown-toc--refresh-toc-maybe nil 'local)
    (add-hook 'markdown-follow-link-functions 'markdown-toc--follow-link nil
              'local)))

(provide 'markdown-toc)
;;; markdown-toc.el ends here
