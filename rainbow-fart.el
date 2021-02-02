;;; rainbow-fart.el --- Checks the keywords of code to play suitable sounds -*- lexical-binding: t; -*-

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
(require 'json)

(defgroup rainbow-fart nil
  "rainbow-fart-mode customize group."
  :prefix "rainbow-fart-"
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

(defcustom rainbow-fart-ignore-modes nil
  "A list of major modes which will enable rainbow-fart-mode."
  :type 'list
  :safe #'listp
  :group 'rainbow-fart)

(defvar rainbow-fart--playing nil
  "The status of rainbow-fart playing.")

(defvar rainbow-fart--play-last-time nil
  "The last time of rainbow-fart play.")

(defcustom rainbow-fart-voice-pack-alist '((t . "JustKowalski"))
  "A list of model voice packs."
  :type 'alist
  :safe #'listp
  :group 'rainbow-fart)

;;; TODO Support multiple voice packs data structure.
(defvar rainbow-fart-manifest-alist nil
  "An alist of model voice pack's manifest info.")

;;; TODO Support multiple voice packs data structure.
(defcustom rainbow-fart-keyword-voices-alist '()
  "An alist of pairs of programming language keywords and voice filenames."
  :type 'alist
  :safe #'listp
  :group 'rainbow-fart)

;;; Parsing voice pack manifest.json

(defun rainbow-fart-voice-pack-find-json-files (voice-pack)
  "Find voice package manifest.json and contributes.json two files."
  (let ((voice-model-dir (expand-file-name voice-pack rainbow-fart-voices-directory)))
    (if (file-exists-p (expand-file-name "contributes.json" voice-model-dir))
        (list
         (expand-file-name "manifest.json" voice-model-dir)
         (expand-file-name "contributes.json" voice-model-dir))
      (list (expand-file-name "manifest.json" voice-model-dir) nil))))

(defun rainbow-fart-voice-pack-parse-manifest (two-json)
  "Read in manifest.json file."
  (let* ((manifest-json-file (car two-json))
         (contributes-json-file (cadr two-json))
         (manifest (json-read-file manifest-json-file))
         (name (alist-get 'name manifest))
         (display-name (alist-get 'display-name manifest))
         (version (alist-get 'version manifest))
         (author (alist-get 'author manifest))
         ;; (description (alist-get 'description manifest))
         ;; (avatar (alist-get 'avatar manifest))           ; "avatar.jpg"
         ;; (avatar-dark (alist-get 'avatar-dark manifest)) ; "avatar-dark.jpg"
         (languages (alist-get 'languages manifest))     ; vector ["python"]
         ;; (locale (alist-get 'locale manifest))           ; "zh"
         ;; (gender (alist-get 'gender manifest))           ; "female"
         ;; `contributes' is a vector of keywords, voices and texts.
         (contributes (or (alist-get 'contributes manifest) ; "contributes" is in "manifest.json"
                          ;; "contributes" is in another file "contributes.json"
                          (alist-get 'contributes
                                     (json-read-file
                                      (expand-file-name
                                       "contributes.json"
                                       (file-name-directory manifest-json-file)))))))
    (setq rainbow-fart-manifest-alist manifest)
    (message "Loading rainbow-fart voice pack: %s (%s) by %s." name version author)
    (when (vectorp contributes)
      ;; reset voices alist
      (setq rainbow-fart-keyword-voices-alist nil)
      ;; NOTE `contributes' is a vector. Can't use `loop' to iterate.
      ;; append to data structure
      (mapcar
       (lambda (definition-alist)
         (let ((keywords (mapcar #'identity (alist-get 'keywords definition-alist)))
               (voices (mapcar #'identity (alist-get 'voices definition-alist)))
               (texts (mapcar #'identity (alist-get 'texts definition-alist))))
           (mapcar
            (lambda (key-str)
              (if-let ((keyword (string-trim key-str)))
                  (add-to-list 'rainbow-fart-keyword-voices-alist (cons keyword voices) 'append)))
            keywords)))
       contributes))
    (message "The rainbow-fart voice pack model: {%s} loaded." display-name)))

;; initialize with default voice pack.
(rainbow-fart-voice-pack-parse-manifest
 (rainbow-fart-voice-pack-find-json-files (alist-get 't rainbow-fart-voice-pack-alist)))

;;; Play

(defun rainbow-fart--get-media-uri (keyword)
  "Get media uri based on KEYWORD."
  (when-let ((uris (cdr (assoc keyword rainbow-fart-keyword-voices-alist))))
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
                         (> (- (float-time) rainbow-fart--play-last-time)
                            rainbow-fart-keyword-interval)
                       (setq rainbow-fart--play-last-time (float-time))))))
    (when-let ((uri (rainbow-fart--get-media-uri keyword))
               (command (or
                         (executable-find "mpg123")
                         (executable-find "mplayer")
                         (executable-find "mpv"))))
      (setq rainbow-fart--playing t)
      (make-process :name "rainbow-fart"
                    :command `(,command ,uri)
                    :buffer " *rainbow-fart*"
                    :sentinel (lambda (_ __)
                                (setq rainbow-fart--playing nil)
                                (setq rainbow-fart--play-last-time (float-time)))))))
;;; prefix detection

(defun rainbow-fart-get-prefix (regexp &optional expression limit)
  (when (looking-back regexp limit)
    (or (match-string-no-properties (or expression 0)) "")))

(defun rainbow-fart--post-self-insert ()
  "A hook function on `post-self-insert-hook' to play audio."
  (when (and (derived-mode-p 'prog-mode)
             (memq major-mode rainbow-fart-ignore-modes))
    (let* ((prefix (save-excursion
                     ;; support prefix like "if(", "if (", "=>" etc keywords following punctuation.
                     (rainbow-fart-get-prefix "\\(?1:\\_<[^\\ ].\\_>\\)\\ ?[[:punct:]]?" 1)))
           (face (get-text-property (- (point) 1) 'face)))
      (when (or (memq face '(font-lock-keyword-face))
                (null face))
        (rainbow-fart--play prefix)))))

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
(defun rainbow-fart-record-voice-for-keyword ()
  "Record a voice file which stored under the voice model directory."
  (interactive)
  (unless rainbow-fart-recorder-template
    (error "The variable rainbow-fart-recorder-template is undefined!"))
  (let* ((keyword (read-string "what keyword do you want to recorded for: " (thing-at-point 'symbol)))
         (model-directory (expand-file-name rainbow-fart-voice-model rainbow-fart-voices-directory))
         (voice-file-name (format "%s-%s.mp3" keyword (float-time)))
         (voice-file-path (expand-file-name voice-file-name model-directory))
         (record-cmd (replace-regexp-in-string "%f" voice-file-path rainbow-fart-recorder-template)))
    (shell-command record-cmd))
  ;; TODO write new audio file and keyword to contributions JSON file.
  )


;;;###autoload
(define-minor-mode rainbow-fart-mode
  "A minor mode add an encourager when you programming."
  :init-value nil
  :lighter nil
  :group 'rainbow-fart
  :global t
  (if rainbow-fart-mode
      (progn
        (add-hook 'post-self-insert-hook #'rainbow-fart--post-self-insert t t)
        (advice-add (eval 'flycheck-display-errors-function)
                    :before 'rainbow-fart--linter-display-errors)
        (when rainbow-fart-time-interval
          (setq rainbow-fart--timer
                (run-with-timer 0 rainbow-fart-time-interval 'rainbow-fart--timing-remind))))
    (remove-hook 'post-self-insert-hook #'rainbow-fart--post-self-insert t)
    (advice-remove (eval 'flycheck-display-errors-function)
                   'rainbow-fart--linter-display-errors)
    (when (timerp rainbow-fart--timer)
      (cancel-timer rainbow-fart--timer))))



(provide 'rainbow-fart)

;;; rainbow-fart.el ends here
