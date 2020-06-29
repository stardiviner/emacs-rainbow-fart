;;; rainbow-fart.el --- Checks the keywords of code to play suitable sounds -*- lexical-binding: t; -*-

;;; Time-stamp: <2020-06-29 16:46:14 stardiviner>

;; Authors: stardiviner <numbchild@gmail.com>
;; Package-Requires: ((emacs "25.1") (flycheck "32-cvs"))
;; Package-Version: 0.1
;; Keywords: tools
;; homepage: https://github.com/stardiviner/emacs-rainbow-fart

;; rainbow-fart is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; rainbow-fart is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; 
;; Usage:
;; (add-hook 'prog-mode-hook #'rainbow-fart-mode)

;;; Code:

(require 'flycheck)
(require 'url)

(defgroup rainbow-fart nil
  "rainbow-fart-mode customize group."
  :prefix "rainbow-fart-"
  :group 'rainbow-fart)

(defcustom rainbow-fart-voice-alist
  '(("defun" . ("function.mp3" "function_01.mp3" "function_02.mp3" "function_03.mp3"))
    ("defn" . ("function.mp3" "function_01.mp3" "function_02.mp3" "function_03.mp3"))
    ("def" . ("function.mp3" "function_01.mp3" "function_02.mp3" "function_03.mp3"))
    ("fn" . ("function.mp3" "function_01.mp3" "function_02.mp3" "function_03.mp3"))
    ("lambda" . ("function.mp3" "function_01.mp3" "function_02.mp3" "function_03.mp3"))
    ("function" . ("function.mp3" "function_01.mp3" "function_02.mp3" "function_03.mp3"))
    ("->" . ("arrow_function_01.mp3"))
    ("->>" . ("arrow_function_01.mp3"))
    ("=>" . ("arrow_function_01.mp3"))
    ("if" . ("if_01.mp3" "if_02.mp3" "if_03.mp3"))
    ("while" . ("if_01.mp3" "if_02.mp3" "if_03.mp3"))
    ("when" . ("if_01.mp3" "if_02.mp3" "if_03.mp3"))
    ("until" . ("if_01.mp3" "if_02.mp3" "if_03.mp3"))
    ("for" . ("for_01.mp3" "for_02.mp3" "for_03.mp3"))
    ("loop" . ("for_01.mp3" "for_02.mp3" "for_03.mp3"))
    ("await" . ("await_01.mp3" "await_02.mp3" "await_03.mp3"))
    ("promise" . ("await_01.mp3" "await_02.mp3" "await_03.mp3"))
    ("catch" . ("catch_01.mp3" "catch_02.mp3" "catch_03.mp3"))
    ("import" . ("import_01.mp3" "import_02.mp3"))
    (":import" . ("import_01.mp3" "import_02.mp3"))
    (":require" . ("import_01.mp3" "import_02.mp3"))
    ("require" . ("import_01.mp3" "import_02.mp3"))
    ("load" . ("import_01.mp3" "import_02.mp3"))
    ("load-file" . ("import_01.mp3" "import_02.mp3"))
    ("v-html" . ("v_html_01.mp3"))
    ("fuck" . ("fuck_pm_01.mp3" "fuck_pm_02.mp3"))
    ("shit" . ("fuck_pm_01.mp3" "fuck_pm_02.mp3"))
    ("damn" . ("fuck_pm_01.mp3" "fuck_pm_02.mp3"))
    ;; time
    ("hour" . ("time_each_hour_01.mp3" "time_each_hour_02.mp3"
               "time_each_hour_03.mp3" "time_each_hour_04.mp3" "time_each_hour_05.mp3"))
    ("morning" . ("time_morning_01.mp3"))
    ("before_noon" . ("time_before_noon_01.mp3" "time_before_noon_02.mp3"
                      "time_before_noon_03.mp3" "time_before_noon_04.mp3"))
    ("noon" . ("time_noon_01.mp3"))
    ("evening" . ("time_evening_01.mp3"))
    ("midnight" . ("time_midnight_01.mp3"))
    ;; TODO `flycheck' support
    ("info" . ())
    ("warning" . ())
    ("error" . ()))
  "An alist of pairs of programming language keywords and voice filenames."
  :type 'alist
  :safe #'listp
  :group 'rainbow-fart)

(defcustom rainbow-fart-voice-model "JustKowalski"
  "The voice model to be used."
  :type 'string
  :safe #'stringp
  :group 'rainbow-fart)

(defcustom rainbow-fart-voices-directory
  (concat (file-name-directory (or load-file-name buffer-file-name)) "voices/")
  "The directory of voices."
  :type 'string
  :safe #'stringp
  :group 'rainbow-fart)

(defcustom rainbow-fart-keyword-interval (* 60 5)
  "The time interval in seconds of rainbow-fart play voice for keywords.
If it is nil, will play sound for every keywords."
  :type 'number
  :safe #'numberp
  :group 'rainbow-fart)

(defcustom rainbow-fart-time-interval (* 60 15)
  "The time interval in seconds of rainbow-fart play voice for hours.
If it's nil, the hours remind will not started."
  :type 'number
  :safe #'numberp
  :group 'rainbow-fart)

(defcustom rainbow-fart-recorder-template nil
  "The command line template to record voice file.

%f will be replaced to the voice file name."
  :type 'string
  :safe #'stringp
  :group 'rainbow-fart)

(defvar rainbow-fart--playing nil
  "The status of rainbow-fart playing.")

(defvar rainbow-fart--play-last-time nil
  "The last time of rainbow-fart play.")

(defun rainbow-fart--get-media-uri (keyword)
  "Get media uri based on KEYWORD."
  (when-let ((uris (cdr (assoc keyword rainbow-fart-voice-alist))))
    (let ((uri (nth (random (length uris)) uris))
          (voice-model-directory
           (expand-file-name rainbow-fart-voice-model rainbow-fart-voices-directory)))
      (if (url-type (url-generic-parse-url uri))
          uri
        (let ((uri (expand-file-name uri voice-model-directory)))
          (when (file-exists-p uri)
            uri))))))


(defun rainbow-fart--play (keyword)
  "A private function to play voice for matched KEYWORD."
  (unless (or rainbow-fart--playing
              (when rainbow-fart-keyword-interval
                (not (if rainbow-fart--play-last-time
                         (> (- (float-time) rainbow-fart--play-last-time) rainbow-fart-keyword-interval)
                       (setq rainbow-fart--play-last-time (float-time))))))
    (when-let ((uri (rainbow-fart--get-media-uri keyword))
               (command (or
                         (executable-find "mpg123")
                         (executable-find "mplayer")
                         (executable-find "mpv"))))
      (setq rainbow-fart--playing t)
      (make-process :name "rainbow-fart"
                    :command `(,command ,uri)
                    :buffer "*rainbow-fart*"
                    :sentinel (lambda (_ __)
                                (setq rainbow-fart--playing nil)
                                (setq rainbow-fart--play-last-time (float-time)))))))
;;; prefix detection

(defun rainbow-fart--post-self-insert ()
  "A hook function on `post-self-insert-hook' to play audio."
  (let* ((prefix (thing-at-point 'symbol))
         (face (get-text-property (1- (point)) 'face)))
    (when (or (memq face '(font-lock-keyword-face))
              (null face))
      (rainbow-fart--play prefix))))

;;; linter like `flycheck'

(defun rainbow-fart--linter-display-error (err)
  "Play voice for `flycheck-error' ERR."
  (let ((level (flycheck-error-level err)))
    (rainbow-fart--play level)))

(defun rainbow-fart--linter-display-errors (errors)
  "A function to report ERRORS used as replacement of linter like `flycheck' and `flymake'."
  (rainbow-fart--play
   (mapc #'rainbow-fart--linter-display-error
         (seq-uniq
          (seq-mapcat #'flycheck-related-errors errors)))))

;;; timer

(defun rainbow-fart--timing ()
  "Play voice for current time quantum."
  (let* ((time (format-time-string "%H:%M"))
         (pair (split-string time ":"))
         (hour (string-to-number (car pair))))
    (cond
     ((and (> hour 05) (< hour 08))     ; 05:00 -- 08:00
      "morning")
     ((and (> hour 08) (< hour 10))     ; 08:00 -- 10:00
      "hour")
     ((and (> hour 10) (< hour 11))     ; 10:00 -- 11:00
      "before_noon")
     ((and (> hour 11) (< hour 13))     ; 11:00 -- 13:00
      "noon")
     ((and (> hour 13) (< hour 15))     ; 13:00 -- 15:00
      "hour")
     ((and (> hour 15) (< hour 17))     ; 15:00 -- 17:00
      "afternoon")
     ((and (> hour 18) (< hour 22))     ; 18:00 -- 21:00
      "evening")
     ((or (> hour 23) (< hour 01))     ; 23:00 -- 01:00
      "midnight"))))

(defun rainbow-fart--timing-remind ()
  "Remind you in specific time quantum."
  (when (and rainbow-fart--play-last-time
             (> (- (float-time) rainbow-fart--play-last-time) rainbow-fart-time-interval))
    (rainbow-fart--play (rainbow-fart--timing))
    (setq rainbow-fart--play-last-time (float-time))))

(defvar rainbow-fart--timer nil)

;;;###autoload
(defun rainbow-fart-add-fart-voice ()
  "Create a voice file which stored under the voice model directory."
  (interactive)
  (unless rainbow-fart-recorder-template
    (error "The variable rainbow-fart-recorder-template is undefined!"))
  (let* ((keyword (read-string "what keyword do you want to recorded for: " (thing-at-point 'symbol)))
         (model-directory (expand-file-name rainbow-fart-voice-model rainbow-fart-voices-directory))
         (voice-file-name (format "%s-%s.mp3" keyword (float-time)))
         (voice-file-path (expand-file-name voice-file-name model-directory))
         (record-cmd (replace-regexp-in-string "%f" voice-file-path rainbow-fart-recorder-template)))
    (shell-command record-cmd)))

;;;###autoload
(define-minor-mode rainbow-fart-mode
  "A minor mode add an encourager when you programming."
  :init-value nil
  :lighter " rainbow-fart "
  :group 'rainbow-fart
  (if rainbow-fart-mode
      (progn
        (add-hook 'post-self-insert-hook #'rainbow-fart--post-self-insert t t)
        (advice-add (buffer-local-value 'flycheck-display-errors-function (current-buffer))
                    :before 'rainbow-fart--linter-display-errors)
        (when rainbow-fart-time-interval
          (setq rainbow-fart--timer
                (run-with-timer 10 rainbow-fart-time-interval 'rainbow-fart--timing-remind))))
    (remove-hook 'post-self-insert-hook #'rainbow-fart--post-self-insert t)
    (advice-remove (buffer-local-value 'flycheck-display-errors-function (current-buffer))
                   'rainbow-fart--linter-display-errors)
    (when rainbow-fart--timer
      (cancel-timer rainbow-fart--timer))))



(provide 'rainbow-fart)

;;; rainbow-fart.el ends here
