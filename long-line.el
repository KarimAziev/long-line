;;; long-line.el --- Show and move to long lines -*- lexical-binding: t -*-

;; Copyright (C) 2022 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/long-line
;; Keywords: lisp, convenience
;; Version: 0.1.1
;; Package-Requires: ((emacs "28.1"))

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

;;; Minor mode

;; `long-line-mode'
;;      Show or hide fill column indicator after save.
;;      If buffer contain lines longer then the value of the variable `fill-column'
;;      show it, else hide.

;;; Commands

;; M-x `long-line-show-or-hide-indicator'
;;      Show or hide fill column indicator in current buffer.
;;      If buffer contain lines longer then the value of the variable `fill-column'
;;      show it, else hide.

;; M-x `long-line-next-long-line' (&optional arg)
;;      Move to the ARGth next long line greater than `fill-column'.

;; M-x `long-line-prev-long-line'
;;      Move to the previous long line greater than `fill-column'.

;; M-x `long-line-next-or-prev-long'
;;      Move to the next or previous long line greater than `fill-column'.

;; M-x `long-line-prev-or-next-long'
;;      Move to the previous or next long line greater than `fill-column'.

;;; Customization

;; `long-line-highlight-line-p'
;;      Whether to highlight long line after `long-line-next-long-line'.

;; `long-line-message-function'
;;      Function to show messages.
;;      Should accept the same arguments as `message'.

;;; Code:

(require 'display-fill-column-indicator)
(require 'transient)
(defcustom long-line-message-function 'message
  "Function to show messages.
Should accept the same arguments as `message'."
  :type '(choice
          (const :tag "None" nil)
          (function :tag "Function"))
  :group 'long-line)

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
      (re-search-backward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((and (or (nth 3 parse))
                  (nth 8 parse))
             (goto-char (nth 8 parse)))
            ((and (nth 4 parse)
                  (nth 8 parse))
             (goto-char (nth 8 parse)))
            (t
             (setq count (1- count))))))
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
             (setq count (1- count))))))
  (point))

(defun long-line-re-search-forward (regexp &optional bound noerror count)
  "Search forward from point for REGEXP ignoring elisp comments and strings.
Arguments BOUND, NOERROR, COUNT has the same meaning as `re-search-forward'."
  (unless count (setq count 1))
  (let ((init-point (point))
        (search-fun
         (cond ((< count 0)
                (setq count (- count))
                #'long-line-re-search-backward-inner)
               ((> count 0) #'long-line-re-search-forward-inner)
               (t #'ignore))))
    (condition-case err
        (funcall search-fun regexp bound count)
      (search-failed
       (goto-char init-point)
       (unless noerror
         (signal (car err)
                 (cdr err)))))))

(defvar long-line-doc '(("defcustom" . 3)
                        ("defvar" . 3)
                        ("defvar-local" . 3)
                        ("defun" . 3)
                        ("defmacro" . 3)
                        ("defsubst" . 3)
                        ("define-derived-mode" . 4)
                        ("define-generic-mode" . 7)
                        ("ert-deftest" . 3)
                        ("cl-defun" . 3)
                        ("cl-defsubst" . 3)
                        ("cl-defmacro" . 3)
                        ("cl-defmethod" . 5)
                        ("defalias" . 3)
                        ("defhydra" . 3)
                        ("cl-defstruct" . 2)
                        ("define-derived-mode" . 4)
                        ("define-compilation-mode" . 3)
                        ("easy-mmode-define-minor-mode" . 2)
                        ("define-minor-mode" . 2)
                        ("define-generic-mode" . 7)))

(defun long-line-on-doc-string-p ()
  "Return non-nil if point is inside doc string."
  (save-excursion
    (when-let* ((pos (point))
                (char (nth 3 (syntax-ppss (point)))))
      (when (progn (ignore-errors (up-list (- 1) t t))
                   (let ((new-pos (point)))
                     (and (not (= pos new-pos))
                          (equal char (char-after new-pos)))))
        (when-let ((pos (save-excursion
                          (when (long-line-move-with
                                 'backward-up-list 1)
                            (let ((s (sexp-at-point)))
                              (when (and (listp s)
                                         (symbolp (car s)))
                                (cdr (assoc (symbol-name
                                             (car s))
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
        (let* ((stx (syntax-ppss (point)))
               (inside-str (nth 3 stx))
               (inside-comment (nth 4 stx)))
          (cond (inside-comment (1- fill-column))
                ((long-line-on-doc-string-p)
                 (let ((doc-line
                        (buffer-substring-no-properties
                         (line-beginning-position)
                         (line-end-position))))
                   (when
                       (and (string-match-p "[\s\t]*\"" doc-line)
                            (not (nth 3 (syntax-ppss (line-beginning-position)))))
                     (setq doc-line (substring-no-properties (string-trim-left doc-line) 1)))
                   (length doc-line)))
                (inside-str
                 (1- fill-column))
                (t line-length)))))))

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
    (let* ((arg 1)
           (line-length 0))
      (while (and (<= line-length fill-column)
                  (zerop (forward-line (if (< arg 0) -1 1))))
        (setq line-length (long-line-get-current-length)))
      (> line-length fill-column))))

(defun long-line--cycle-next-or-prev (arg)
  "Move to the previous or next ARGth long line greater than `fill-column'."
  (or (long-line-next-long-line arg)
      (when (long-line-has-long-line-p)
        (if (> arg 0)
            (goto-char (point-min))
          (goto-char (point-max)))
        (long-line-next-long-line arg))
      (progn
        (let ((line-len (long-line-get-current-length)))
          (when (> line-len fill-column)
            (when long-line-highlight-line-p
              (pulse-momentary-highlight-one-line))
            (when long-line-message-function
              (funcall long-line-message-function
                       "Long line of %d columns found"
                       line-len)))
          (long-line-show-or-hide-indicator)))))

;;;###autoload
(defun long-line-prev-or-next-long ()
  "Move to the previous or next long line greater than `fill-column'."
  (interactive)
  (long-line--cycle-next-or-prev -1))

;;;###autoload
(defun long-line-next-or-prev-long ()
  "Move to the next or previous long line greater than `fill-column'."
  (interactive)
  (long-line--cycle-next-or-prev 1))

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
    (cond ((>= fill-column line-length)
           (goto-char opoint)
           (when display-fill-column-indicator
             (long-line-show-or-hide-indicator))
           (when long-line-message-function
             (funcall long-line-message-function
                      "Long line not found"))
           nil)
          ((or (> arg 1)
               (> -1 arg))
           (long-line-next-long-line (1- arg))
           (point))
          (t
           (unless display-fill-column-indicator
             (display-fill-column-indicator-mode 1))
           (end-of-line 1)
           (when long-line-message-function
             (funcall long-line-message-function
                      "Long line of %d columns found"
                      line-length))
           (skip-chars-backward "\s\t\r\f\n")
           (long-line-find-place-to-split)
           (point)))))

;;;###autoload
(defun long-line-transient-set-fill-column ()
  "Interactivelly set fill column."
  (interactive)
  (funcall-interactively
   #'set-fill-column
   (read-number
    (format
     "Fill column (%s): "
     fill-column)))
  (when (eq transient-current-command 'long-line-transient)
    (transient--redisplay)))

;;;###autoload
(defun long-line-show-or-hide-indicator ()
  "Show or hide fill column indicator in current buffer.
If buffer contain lines longer then the value of the variable `fill-column'
show it, else hide."
  (interactive)
  (display-fill-column-indicator-mode
   (if (long-line-has-long-line-p) 1 -1)))

;;;###autoload
(define-minor-mode long-line-mode
  "Show or hide fill column indicator after save.
If buffer contain lines longer then the value of the variable `fill-column'
show it, else hide."
  :lighter " Lg-ln"
  :global nil
  (if long-line-mode
      (progn
        (long-line-show-or-hide-indicator)
        (add-hook 'after-save-hook #'long-line-show-or-hide-indicator
                  nil
                  'local))
    (remove-hook 'after-save-hook #'long-line-show-or-hide-indicator
                 'local)
    (display-fill-column-indicator-mode -1))
  (when (eq transient-current-command 'long-line-transient)
    (transient--redisplay)))

(defun long-line-mode-description ()
  "Return description for transient."
  (let ((description "Long Line Mode"))
    (propertize
     description
     'face
     (if long-line-mode
         'success
       'transient-inactive-value))))


;;;###autoload (autoload 'long-line-transient "long-line" nil t)
(transient-define-prefix long-line-transient ()
  "Command dispatcher for `long-line-mode'."
  :transient-suffix #'transient--do-call
  :transient-non-suffix #'transient--do-exit
  [("m" long-line-mode
    :description long-line-mode-description
    :transient t)
   ("f" long-line-transient-set-fill-column
    :description
    (lambda ()
      (concat "Fill column "
              (propertize
               (format "%s" fill-column)
               'face
               'transient-argument))))]
  [("n" "Next long line" long-line-next-or-prev-long :transient t)
   ("p" "Previous long line" long-line-prev-or-next-long :transient t)])

(provide 'long-line)
;;; long-line.el ends here