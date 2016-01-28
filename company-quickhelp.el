;;; company-quickhelp.el --- Popup documentation for completion candidates

;; Copyright (C) 2015, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/company-quickhelp
;; Keywords: company popup documentation quickhelp
;; Version: 1.2.0
;; Package-Requires: ((emacs "24.4") (company "0.8.9") (pos-tip "0.4.6"))

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; When idling on a completion candidate the documentation for the
;; candidate will pop up after `company-quickhelp-idle-delay' seconds.

;;; Usage:
;;  put (company-quickhelp-mode 1) in you init.el to activate
;;  `company-quickhelp-mode'.

;; You can adjust the time it takes for the documentation to pop up by
;; changing `company-quickhelp-delay'

;;; Code:
(require 'company)
(require 'pos-tip)

(defgroup company-quickhelp nil
  "Documentation popups for `company-mode'"
  :group 'company)

(defcustom company-quickhelp-delay 0.5
  "Delay, in seconds, before the quickhelp popup appears.

If set to nil the popup won't automatically appear, but can still
be triggered manually using `company-quickhelp-show'."
  :type '(choice (number :tag "Delay in seconds")
                 (const :tag "Don't popup help automatically" nil))
  :group 'company-quickhelp)

(defcustom company-quickhelp-max-lines nil
  "When not NIL, limits the number of lines in the popup."
  :type '(choice (integer :tag "Max lines to show in popup")
                 (const :tag "Don't limit the number of lines shown" nil))
  :group 'company-quickhelp)

(defvar company-quickhelp-mode-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "M-h") #'company-quickhelp-manual-begin)
    keymap)
  "The keymap used by `company-quickhelp'.")

(defvar company-quickhelp--timer nil
  "Quickhelp idle timer.")

(defvar company-quickhelp--original-tooltip-width nil
  "The documentation popup breaks inexplicably when we transition
  from a large pseudo-tooltip to a small one.  We solve this by
  overriding `company-tooltip-minimum-width' and save the
  original value here so we can restore it.")

(defun company-quickhelp-frontend (command)
  "`company-mode' front-end showing documentation in a `pos-tip' popup."
  (pcase command
    (`post-command (when company-quickhelp-delay
                     (company-quickhelp--set-timer)))
    (`hide
     (when company-quickhelp-delay
       (company-quickhelp--cancel-timer))
     (pos-tip-hide))))

(defun company-quickhelp--doc-and-meta (doc)
  ;; The company backend can either return a buffer with the doc or a
  ;; cons containing the doc buffer and a position at which to start
  ;; reading.
  (let ((doc-buffer (if (consp doc) (car doc) doc))
        (doc-begin (when (consp doc) (cadr doc))))
    (with-current-buffer doc-buffer
      (let ((truncated t))
        (goto-char (or doc-begin (point-min)))
        (if company-quickhelp-max-lines
            (forward-line company-quickhelp-max-lines)
          (goto-char (point-max)))
        (beginning-of-line)
        (when (= (line-number-at-pos)
                 (save-excursion (goto-char (point-max)) (line-number-at-pos)))
          (setq truncated nil))
        (while (and (not (= (line-number-at-pos) 1))
                    (or
                     ;; [back] appears at the end of the help elisp help buffer
                     (looking-at-p "\\[back\\]")
                     ;; [source] cider's help buffer contains a link to source
                     (looking-at-p "\\[source\\]")
                     (looking-at-p "^\\s-*$")))
          (forward-line -1))
        (list :doc (buffer-substring-no-properties (point-min) (point-at-eol))
              :truncated truncated)))))

(defun company-quickhelp--completing-read (prompt candidates &rest rest)
  "`cider', and probably other libraries, prompt the user to
resolve ambiguous documentation requests.  Instead of failing we
just grab the first candidate and press forward."
  (first candidates))

(defun company-quickhelp--doc (selected)
  (cl-letf (((symbol-function 'completing-read)
             #'company-quickhelp--completing-read))
    (let* ((doc (company-call-backend 'doc-buffer selected))
           (doc-and-meta (company-quickhelp--doc-and-meta doc))
           (truncated (plist-get doc-and-meta :truncated))
           (doc (plist-get doc-and-meta :doc)))
      (unless (string= doc "")
        (if truncated
            (concat doc "\n\n[...]")
          doc)))))

(defun company-quickhelp-manual-begin ()
  "Manually trigger the `company-quickhelp' popup for the
currently active `company' completion candidate."
  (interactive)
  ;; This might seem a bit roundabout, but when I attempted to call
  ;; `company-quickhelp--show' in a more direct manner it triggered a
  ;; redisplay of company's list of completion candidates which looked
  ;; quite weird.
  (let ((company-quickhelp-delay 0.01))
    (company-quickhelp--set-timer)))

(defun company-quickhelp--show ()
  (company-quickhelp--ensure-compatibility)
  (company-quickhelp--cancel-timer)
  (let* ((selected (nth company-selection company-candidates))
         (doc (company-quickhelp--doc selected))
         (ovl company-pseudo-tooltip-overlay)
         (overlay-width (* (frame-char-width)
                           (if ovl (overlay-get ovl 'company-width) 0)))
         (overlay-position (* (frame-char-width)
                              (- (if ovl (overlay-get ovl 'company-column) 1) 1)))
         (x-gtk-use-system-tooltips nil))
    (when (and ovl doc)
      (with-no-warnings
        (pos-tip-show doc nil (overlay-start ovl) nil 300 80 nil
                      (+ overlay-width overlay-position) 1)))))

(defun company-quickhelp--set-timer ()
  (when (null company-quickhelp--timer)
    (setq company-quickhelp--timer
          (run-with-idle-timer company-quickhelp-delay nil
                               'company-quickhelp--show))))

(defun company-quickhelp--cancel-timer ()
  (when (timerp company-quickhelp--timer)
    (cancel-timer company-quickhelp--timer)
    (setq company-quickhelp--timer nil)))

(defun company-quickhelp-hide ()
  (company-cancel))

(defun company-quickhelp--ensure-compatibility ()
  ;; Originally this code was in `company-quickhelp-enable' but that
  ;; caused trouble for --daemon users reported in #16.
  (cond
   ((or (not (fboundp 'x-hide-tip))
        (not (fboundp 'x-show-tip)))
    (user-error "Company-quickhelp doesn't work on your system.
Most likely this means you're on a mac with an Emacs build using Cocoa instead of X"))
   ((or (null window-system)
        (eq window-system 'pc))
    (user-error "Company-quickhelp doesn't work in the terminal"))))

(defun company-quickhelp--enable ()
  (add-hook 'focus-out-hook #'company-quickhelp-hide)
  (setq company-quickhelp--original-tooltip-width company-tooltip-minimum-width
        company-tooltip-minimum-width (max company-tooltip-minimum-width 40))
  (add-to-list 'company-frontends 'company-quickhelp-frontend :append))

(defun company-quickhelp--disable ()
  (remove-hook 'focus-out-hook #'company-quickhelp-hide)
  (company-quickhelp--cancel-timer)
  (setq company-tooltip-minimum-width company-quickhelp--original-tooltip-width
        company-frontends
        (delq 'company-quickhelp-frontend company-frontends)))

;;;###autoload
(define-minor-mode company-quickhelp-mode
  "Provides documentation popups for `company-mode' using `pos-tip'."
  :global t :keymap company-quickhelp-mode-map
  (if company-quickhelp-mode
      (company-quickhelp--enable)
    (company-quickhelp--disable)))

(provide 'company-quickhelp)

;;; company-quickhelp.el ends here
