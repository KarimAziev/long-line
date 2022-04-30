;;; long-line.el --- Configure line -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com:KarimAziev/long-line
;; Keywords: lisp, convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "27.1"))

;; This file is NOT part of GNU Emacs.

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
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;; Commentary:

; An Emacs minor mode which show or hide indicator, depending whether
;; the buffer contain lines longer then value of `fill-column'.

;; Usage

;; (require 'long-line)
;; (add-hook 'emacs-lisp-mode-hook 'long-line-mode)

;; Commands

;; M-x `long-line-show-or-hide-indicator'
;;      Show `fill-column' indicator if buffer contain lines longer then `fill-column'.
;;      If no long lines found hide indicator.

;; M-x `long-line-next-long-line' (&optional arg)
;;      Move to the ARGth next long line greater than `fill-column'.

;; M-x `long-line-prev-long-line'
;;      Move to the previous long line greater than `fill-column'.

;;; Code:

(require 'display-fill-column-indicator)
(defun long-line-re-search-backward-inner (regexp &optional bound count)
  "This function is helper for `long-line-re-search-backward'.
Search backward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-backward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (or (nth 3 parse))
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse)))
              (t
               (setq count (1- count)))))))
  (point))

(defun long-line-re-search-backward (regexp &optional bound noerror count)
  "Search backward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (long-line-re-search-forward regexp bound noerror (if count (- count) -1)))

(defun long-line-find-place-to-split ()
	"Find place to split long line."
  (when (looking-back ")" 0)
    (let ((line-beg (save-excursion
                      (beginning-of-line)
                      (point))))
      (while (and (> (current-column) fill-column)
                  (long-line-re-search-backward "[(]+" line-beg t 1)))
      (when (looking-back "['`]" 0)
        (forward-char -1)))))

(defun long-line-re-search-forward-inner (regexp &optional bound count)
  "This function is helper for `long-line-re-search-forward'.
Search forward from point for regular expression REGEXP.
The optional argument BOUND is a buffer position that bounds
  the search.  The match found must not end after that position.  A
  value of nil means search to the end of the accessible portion of
  the buffer.
The optional argument COUNT is a number that indicates the
  search direction and the number of occurrences to search for."
  (let ((parse))
    (while (> count 0)
      (with-syntax-table emacs-lisp-mode-syntax-table
        (re-search-forward regexp bound)
        (setq parse (syntax-ppss))
        (cond ((and (nth 3 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-sexp))
              ((and (nth 4 parse)
                    (nth 8 parse))
               (goto-char (nth 8 parse))
               (forward-line))
              (t
               (setq count (1- count)))))))
  (point))

(defun long-line-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0) (setq count (- count))
                #'long-line-re-search-backward-inner)
               ((> count 0) #'long-line-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err) (cdr err)))))))

(defun long-line-get-current-length ()
	"Get length of current line ignoring comments and strings."
  (save-excursion
    (let ((line-length (progn (end-of-line)
                              (current-column))))
      (if (< line-length fill-column)
          line-length
        (backward-char (- line-length fill-column))
        (if (or
             (nth 3 (syntax-ppss (point)))
             (looking-back "\"" 1)
             (nth 4 (syntax-ppss (point))))
            (1- fill-column)
          line-length)))))

;;;###autoload
(defun long-line-prev-long-line ()
  "Move to the previous long line greater than `fill-column'."
  (interactive)
  (funcall-interactively (long-line-next-long-line -1)))

;;;###autoload
(defun long-line-next-long-line (&optional arg)
  "Move to the ARGth next long line greater than `fill-column'."
  (interactive "p")
  (or arg
      (setq arg 1))
  (let
      ((opoint
        (point))
       (line-length 0))
    (while
        (and
         (>= fill-column line-length)
         (zerop
          (forward-line
           (if
               (> 0 arg)
               -1 1))))
      (setq line-length
            (long-line-get-current-length)))
    (unwind-protect
        (if
            (> line-length fill-column)
            (if
                (> arg 1)
                (long-line-next-long-line
                 (1- arg))
              (if
                  (> -1 arg)
                  (long-line-next-long-line
                   (1- arg))
                (unless display-fill-column-indicator
                  (display-fill-column-indicator-mode 1))
                (end-of-line 1)
                (message
                 (format "Long line of %d columns found" line-length))
                (skip-chars-backward "  \n")
                (long-line-find-place-to-split)))
          (goto-char opoint)
          (message "Long line not found")
          (when display-fill-column-indicator
            (display-fill-column-indicator-mode -1)))
      (run-at-time 0.4 nil 'km-remove-overlay))))

;;;###autoload
(defun long-line-show-or-hide-indicator ()
  "Show or hide fill column indicator in current buffer.
If buffer contain lines longer then the value of the variable `fill-column'
show it, else hide."
  (interactive)
  (unless buffer-file-read-only
    (save-excursion
      (goto-char (point-min))
      (forward-line 1)
      (let ((arg 1))
        (let ((line-length 0))
          (while (and (<= line-length fill-column)
                      (zerop (forward-line (if (< arg 0) -1 1))))
            (setq line-length (long-line-get-current-length)))
          (display-fill-column-indicator-mode
           (if (> line-length fill-column)
               1 -1)))))))

;;;###autoload
(define-minor-mode long-line-mode
  "Show or hide fill column indicator after save.
If buffer contain lines longer then the value of the variable `fill-column'
show it, else hide."
  :lighter " Longl"
  :global nil
  (if long-line-mode
      (progn
        (long-line-show-or-hide-indicator)
        (add-hook 'after-save-hook 'long-line-show-or-hide-indicator nil
                  'local))
    (remove-hook 'after-save-hook 'long-line-show-or-hide-indicator 'local)))

(provide 'long-line)
;;; long-line.el ends here