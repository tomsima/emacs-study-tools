;; my-cedict.el --- CEDICT Chinese dictionary lookup -*- lexical-binding: t; -*-
;;; Commentary:
;; Chinese dictionary lookup using CEDICT, jieba tokenisation, and pinyin conversion.
;;; Code:

(require 'cl-lib)

(defvar my-cedict-file "~/.emacs.d/cedict_ts.u8")
(defvar my-cedict-table nil)

(defun my-cedict-build-table ()
  (setq my-cedict-table (make-hash-table :test 'equal :size 120000))
  (with-temp-buffer
    (insert-file-contents my-cedict-file)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\(\\[[^]]+\\] /.*\\)$" nil t)
      (let ((trad (match-string 1))
            (simp (match-string 2))
            (rest (match-string 3)))
        (push rest (gethash trad my-cedict-table))
        (unless (string= trad simp)
          (push rest (gethash simp my-cedict-table)))))))

(defun my-chinese-word-at-point ()
  (let* ((raw (thing-at-point 'word t))
         (raw-start (car (bounds-of-thing-at-point 'word)))
         (cursor-offset (- (point) raw-start))
         (tokens (my-jieba-segment raw))
         (pos 0)
         result)
    (dolist (tok tokens)
      (let ((end (+ pos (length tok))))
        (when (and (<= pos cursor-offset) (< cursor-offset end))
          (setq result tok))
        (setq pos end)))
    (or result (car tokens))))

;; (defun my-cedict-lookup (word)
;;   (unless my-cedict-table (my-cedict-build-table))
;;   (when-let* ((entries (gethash word my-cedict-table))
;;               (best (or (cl-find-if (lambda (e) (string-match "\\[[a-z]" e)) entries)
;;                         (car entries)))
;;               (_ (string-match "\\[\\([^]]+\\)\\] /\\(.*\\)/" best))
;;               (pinyin (match-string 1 best))
;;               (defs (match-string 2 best)))
;;     (format "%s *%s* /%s/"
;;             word
;;             (my-pinyin-convert pinyin)
;;             (replace-regexp-in-string "/" ", " defs))))

(defun my-cedict-lookup (word)
  (unless my-cedict-table (my-cedict-build-table))
  (let ((entries (gethash word my-cedict-table)))
    (when entries
      (let* ((index 0)
             (results
              (delq nil
                    (mapcar (lambda (entry)
                              (when (string-match "\\[\\([^]]+\\)\\] /\\(.*\\)" entry)
                                (let* ((pinyin (match-string 1 entry))
                                       (defs (match-string 2 entry))
                                       (w (if (= index 0)
                                              word
                                            (make-string (+ 4 (string-width word)) ?\s)))
                                       (line (format "%s *%s* /%s/"
                                                     w
                                                     (my-pinyin-convert pinyin)
                                                     (replace-regexp-in-string "/" ", "
                                                       (replace-regexp-in-string "/$" "" defs)))))
                                  (prog1 line
                                    (setq index (1+ index))))))
                            entries))))
        (when results
          (mapconcat #'identity results "\n"))))))

(defvar my-cedict-log-buffer "*chinese-lookups*")

(defun my-cedict-log (result)
  (with-current-buffer (get-buffer-create my-cedict-log-buffer)
    (when (= (buffer-size) 0)
      (org-mode))
    (goto-char (point-max))
    (insert result "\n")
    (when-let (win (get-buffer-window my-cedict-log-buffer))
      (set-window-point win (point-max)))))

(defun my-cedict-ensure-window ()
  (unless (get-buffer-window my-cedict-log-buffer)
    (let ((win (split-window-right (/ (* (frame-width) 2) 3))))
      (set-window-buffer win (get-buffer-create my-cedict-log-buffer)))))

(defun my-chinese-lookup ()
  (interactive)
  (unless my-cedict-table
    (message "Loading dictionary...")
    (my-cedict-build-table))
  (let* ((word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (my-chinese-word-at-point)))
         (result (my-cedict-lookup word)))
    (my-cedict-ensure-window)
    (if result
        (my-cedict-log result)
      (my-cedict-log (format "No entry found for: %s" word)))))

;; (add-hook 'emacs-startup-hook #'my-cedict-build-table)

(defvar my-pinyin-tone-map
  '(("a" "ā" "á" "ǎ" "à")
    ("e" "ē" "é" "ě" "è")
    ("i" "ī" "í" "ǐ" "ì")
    ("o" "ō" "ó" "ǒ" "ò")
    ("u" "ū" "ú" "ǔ" "ù")
    ("ü" "ǖ" "ǘ" "ǚ" "ǜ")
    ("v" "ǖ" "ǘ" "ǚ" "ǜ")))

;; (defun my-pinyin-add-tone (syllable tone)
;;   "Add tone mark (1-5) to a pinyin syllable. Tone 5 = neutral (no mark)."
;;   (if (= tone 5)
;;       syllable
;;     (let* ((s (replace-regexp-in-string "v" "ü" syllable))
;;            ;; Tone mark goes on 'a' or 'e' first, then the second vowel
;;            ;; of 'ou', otherwise the last vowel
;;            (vowel (cond
;;                    ((string-match "a" s) "a")
;;                    ((string-match "e" s) "e")
;;                    ((string-match "ou" s) "o")
;;                    ((string-match "ü" s) "ü")
;;                    (t (let ((last nil))
;;                         (dolist (v '("i" "o" "u" "ü"))
;;                           (when (string-match v s) (setq last v)))
;;                         last)))))
;;       (if (null vowel)
;;           syllable
;;         (let* ((row (assoc vowel my-pinyin-tone-map))
;;                (marked (nth tone row)))
;;           (replace-regexp-in-string vowel marked s t t))))))

(defun my-pinyin-add-tone (syllable tone)
  (if (= tone 5)
      syllable
    (let* ((s (downcase (replace-regexp-in-string "v" "ü" syllable)))
           (vowel (cond
                   ((string-match "a" s) "a")
                   ((string-match "e" s) "e")
                   ((string-match "ou" s) "o")
                   ((string-match "ü" s) "ü")
                   (t (let ((last nil))
                        (dolist (v '("i" "o" "u" "ü"))
                          (when (string-match v s) (setq last v)))
                        last)))))
      (if (null vowel)
          syllable
        (let* ((row (assoc vowel my-pinyin-tone-map))
               (marked (nth tone row)))
          (replace-regexp-in-string vowel marked s t t))))))

(defun my-pinyin-convert (pinyin)
  "Convert a pinyin string like 'ni3 hao3' to 'nǐ hǎo'."
  (mapconcat
   (lambda (syllable)
     (if (string-match "^\\(.*\\)\\([1-5]\\)$" syllable)
         (my-pinyin-add-tone (match-string 1 syllable)
                             (string-to-number (match-string 2 syllable)))
       syllable))
   (split-string pinyin " " t)
   " "))



(defvar my-jieba-process nil)

(defun my-jieba-segment (text)
  (let ((proc (my-jieba-ensure))
        (result nil)
        (deadline (+ (float-time) 2.0)))
    (process-put proc :jieba-output "")
    (set-process-filter proc
      (lambda (p s)
        (process-put p :jieba-output
                     (concat (process-get p :jieba-output) s))))
    (process-send-string proc (concat text "\n"))
    (while (and (null result) (< (float-time) deadline))
      (accept-process-output proc 0.1)
      (let ((out (process-get proc :jieba-output)))
        (when (string-match "\n" out)
          (setq result (string-trim out)))))
    (split-string (or result "") nil t)))

(defun my-jieba-ensure ()
  (unless (process-live-p my-jieba-process)
    (setq my-jieba-process
          (make-process :name "jieba"
                        :command '("jieba-cli")
                        :connection-type 'pipe
                        :noquery t))
    ;; warm up jieba's dictionary before first real use
    (my-jieba-segment "预热"))
  my-jieba-process)

(provide 'my-cedict)
;;; my-cedict.el ends here
