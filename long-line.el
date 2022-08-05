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

(defcustom long-line-highlight-line-p t
  "Whether to highlight long line after `long-line-next-long-line'."
  :group 'long-line
  :type 'boolean)

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
        (forward-char -1))
      (when long-line-highlight-line-p
        (pulse-momentary-highlight-one-line)))))

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

(defvar long-line-doc '((defcustom . 3)
                        (defvar . 3)
                        (defvar-local . 3)
                        (defun . 3)
                        (defmacro . 3)
                        (defsubst . 3)
                        (define-derived-mode . 4)
                        (define-generic-mode . 7)
                        (ert-deftest . 3)
                        (cl-defun . 3)
                        (cl-defsubst . 3)
                        (cl-defmacro . 3)
                        (cl-defmethod . 5)
                        (defalias . 3)
                        (defhydra . 3)
                        (cl-defstruct . 2)
                        (define-derived-mode . 4)
                        (define-compilation-mode . 3)
                        (easy-mmode-define-minor-mode . 2)
                        (define-minor-mode . 2)
                        (define-generic-mode . 7)))

(defun long-line-on-doc-string-p ()
  "Return non-nil if point is inside doc string."
  (save-excursion
    (when-let* ((pos (point))
                (char (nth 3 (syntax-ppss (point)))))
      (when (progn (ignore-errors (up-list (- 1) t t))
                   (let ((new-pos (point)))
                     (and (not (= pos new-pos))
                          (equal char (char-after new-pos)))))
        (when-let ((pos (save-excursion (when (long-line-move-with
                                               'backward-up-list 1)
                                          (let ((s (sexp-at-point)))
                                            (when
                                                (and (listp s)
                                                     (symbolp (car s)))
                                              (cdr (assq (car s)
                                                         long-line-doc))))))))
          (long-line-move-with 'backward-sexp pos))))))

(defun long-line-get-current-length ()
	"Get length of current line ignoring comments and strings."
  (save-excursion
    (let ((line-length (progn (end-of-line)
                              (current-column))))
      (if (< line-length fill-column)
          line-length
        (backward-char (- line-length fill-column))
        (if (or
             (when (nth 3 (syntax-ppss (point)))
               (not (long-line-on-doc-string-p)))
             (nth 4 (syntax-ppss (point))))
            (1- fill-column)
          line-length)))))

(defun long-line-move-with (fn &optional n)
  "Move by calling FN N times.
Return new position if changed, nil otherwise."
  (unless n (setq n 1))
  (when-let ((str-start (nth 8 (syntax-ppss (point)))))
    (goto-char str-start))
  (let ((init-pos (point))
        (pos)
        (count n))
    (while (and (not (= count 0))
                (when-let ((end (ignore-errors
                                  (funcall fn)
                                  (point))))
                  (unless (= end (or pos init-pos))
                    (setq pos end))))
      (setq count (1- count)))
    (if (= count 0)
        pos
      (goto-char init-pos)
      nil)))

(defun long-line-has-long-line-p ()
  "Return t if buffer contain lines longer then the value of `fill-column'."
  (save-excursion
    (goto-char (point-min))
    (forward-line 1)
    (let ((arg 1))
      (let ((line-length 0))
        (while (and (<= line-length fill-column)
                    (zerop (forward-line (if (< arg 0) -1 1))))
          (setq line-length (long-line-get-current-length)))
        (> line-length fill-column)))))

;;;###autoload
(defun long-line-prev-long-line ()
  "Move to the previous long line greater than `fill-column'."
  (interactive)
  (funcall-interactively #'long-line-next-long-line -1))

;;;###autoload
(defun long-line-next-long-line (&optional arg)
  "Move to the ARGth next long line greater than `fill-column'."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((opoint (point))
        (line-length 0))
    (while (and (>= fill-column line-length)
                (zerop (forward-line (if (> 0 arg) -1 1))))
      (setq line-length
            (long-line-get-current-length)))
    (cond
     ((>= fill-column line-length)
      (goto-char opoint)
      (message "Long line not found")
      (when display-fill-column-indicator
        (long-line-show-or-hide-indicator)))
     ((or (> arg 1)
          (> -1 arg))
      (long-line-next-long-line (1- arg)))
     (t
      (unless display-fill-column-indicator
        (display-fill-column-indicator-mode 1))
      (end-of-line 1)
      (message
       (format "Long line of %d columns found" line-length))
      (skip-chars-backward "  \n")
      (long-line-find-place-to-split)))))

;;;###autoload
(defun long-line-show-or-hide-indicator ()
  "Show or hide fill column indicator in current buffer.
If buffer contain lines longer then the value of the variable `fill-column'
show it, else hide."
  (interactive)
  (unless buffer-file-read-only
    (display-fill-column-indicator-mode (if (long-line-has-long-line-p) 1 -1))))

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