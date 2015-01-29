;;; company-quickhelp.el --- Popup documentation for completion candidates

;; Copyright (C) 2015, Lars Andersen

;; Author: Lars Andersen <expez@expez.com>
;; URL: https://www.github.com/expez/company-quickhelp
;; Keywords: company popup documentation quickhelp
;; Version: 0.1
;; Package-Requires: ((emacs "24") (company "0.8.9") (pos-tip "0.4.6"))

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

(defcustom company-quickhelp-delay 0.5
  "Delay, in seconds, before the quickhelp popup appears."
  :group 'company-quickhelp)

(defun company-quickhelp-frontend (command)
  "`company-mode' front-end showing documentation in a `pos-tip' popup."
  (pcase command
    (`post-command (company-quickhelp--set-timer))
    (`hide
     (company-quickhelp--cancel-timer)
     (pos-tip-hide))))

(defun company-quickhelp--show ()
  (company-quickhelp--cancel-timer)
  (let* ((selected (nth company-selection company-candidates))
         (doc-buffer (company-call-backend 'doc-buffer selected))
         (ovl company-pseudo-tooltip-overlay)
         (x-gtk-use-system-tooltips nil))
    (when (and ovl doc-buffer)
      (with-no-warnings
        (let* ((width (overlay-get ovl 'company-width))
               (col (overlay-get ovl 'company-column))
               (extra (- (+ width col) (company--window-width))))
          (pos-tip-show (with-current-buffer doc-buffer (buffer-string))
                        nil
                        nil
                        nil
                        300
                        80
                        nil
                        (* (frame-char-width)
                           (- width (length company-prefix)
                              (if (< 0 extra) extra 1)))))))))

(defvar company-quickhelp--timer nil
  "Quickhelp idle timer.")

(defun company-quickhelp--set-timer ()
  (when (null company-quickhelp--timer)
    (setq company-quickhelp--timer
          (run-with-idle-timer company-quickhelp-delay nil
                               'company-quickhelp--show))))

(defun company-quickhelp--cancel-timer ()
  (when (timerp company-quickhelp--timer)
    (cancel-timer company-quickhelp--timer)
    (setq company-quickhelp--timer nil)))

;;;###autoload
(define-minor-mode company-quickhelp-mode
  "Provides documentation popups for `company-mode' using `pos-tip'."
  :global t
  (if company-quickhelp-mode
      (push 'company-quickhelp-frontend company-frontends)
    (setq company-frontends
          (delq 'company-quickhelp-frontend company-frontends))
    (company-quickhelp--cancel-timer)))

(provide 'company-quickhelp)

;;; company-quickhelp.el ends here
