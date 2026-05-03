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

(defvar my-cedict-pinyin-table nil
  "Hash table mapping pinyin strings to lists of (trad . simp) cons cells.")

(defun my-cedict-build-pinyin-table ()
  (setq my-cedict-pinyin-table (make-hash-table :test 'equal :size 120000))
  (with-temp-buffer
    (insert-file-contents my-cedict-file)
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([^ ]+\\) \\([^ ]+\\) \\(\\[[^]]+\\]\\)" nil t)
      (let ((trad (match-string 1))
            (simp (match-string 2))
            (bracket (match-string 3)))
        (when (string-match "\\[\\([^]]+\\)\\]" bracket)
          (let ((pinyin (downcase (match-string 1 bracket))))
            (push (cons trad simp)
                  (gethash pinyin my-cedict-pinyin-table))))))))

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
                                              (format "_%s_" word)
                                            (make-string (+ 2 (string-width word)) ?\s)))
                                       (line (format "%s *%s* /%s/"
						     w
						     (my-pinyin-convert pinyin)
						     (replace-regexp-in-string "/" ", "
									       (replace-regexp-in-string "/$" ""
													 (replace-regexp-in-string "\\[\\([^]]+\\)\\]" "" defs))))))
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
    (let ((win (condition-case nil
                   (split-window-right (/ (* (frame-width) 2) 3))
                 (error (cl-reduce (lambda (a b)
                                     (if (< (window-width a) (window-width b)) a b))
                                   (window-list))))))
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

(defvar my-cedict-pinyin-log-buffer "*cangjie-check*")

;;----------------------------------------------------------------------------pinyin search

(defun my-cedict-pinyin-ensure-window ()
  (unless (get-buffer-window my-cedict-pinyin-log-buffer)
    (let ((win (condition-case nil
                   (split-window-right (/ (* (frame-width) 2) 3))
                 (error (cl-reduce (lambda (a b)
                                     (if (< (window-width a) (window-width b)) a b))
                                   (window-list))))))
      (set-window-buffer win (get-buffer-create my-cedict-pinyin-log-buffer))
      (with-selected-window win
        (text-scale-set 4)))))

(defun my-cedict-pinyin-log (result)
  (with-current-buffer (get-buffer-create my-cedict-pinyin-log-buffer)
    (goto-char (point-max))
    (insert result "\n")
    (when-let (win (get-buffer-window my-cedict-pinyin-log-buffer))
      (set-window-point win (point-max)))))

(defun my-cedict-search-by-pinyin (pinyin-query)
  (interactive "spinyin: ")
  (unless my-cedict-pinyin-table
    (message "loading pinyin index...")
    (my-cedict-build-pinyin-table))
  (unless cangjie-table
    (message "loading cangjie index...")
    (setq cangjie-table (cangjie-load-table)))
  (let* ((query (downcase (my-cedict-normalize-pinyin pinyin-query)))
         (pairs (gethash query my-cedict-pinyin-table))
         (seen-pairs (make-hash-table :test 'equal))
         (matches '()))
    (dolist (pair pairs)
      (let ((key (concat (car pair) "/" (cdr pair))))
        (unless (gethash key seen-pairs)
          (puthash key t seen-pairs)
          (push pair matches))))
    (my-cedict-pinyin-ensure-window)
    (my-cedict-pinyin-log
     (propertize (format "───── %s ─────"
			 (my-pinyin-to-zhuyin query))
		 'display '(height 0.5)))
    (if matches
        (let ((seen-chars (make-hash-table :test 'equal)))
          (dolist (pair (nreverse matches))
	    (let* ((trad (car pair))
		   (simp (cdr pair))
		   (format-char (lambda (c)
				  (let ((code (gethash c cangjie-table)))
				    (if code
					(cangjie-code-to-radicals
					 (car (split-string code " / ")))
				      nil))))
		   (lines '()))
	      (cl-loop for tc across trad
		       for sc across simp
		       do (let* ((t-str (char-to-string tc))
				 (s-str (char-to-string sc))
				 (same (string= t-str s-str)))
			    (unless (gethash t-str seen-chars)
			      (puthash t-str t seen-chars)
			      (when-let (code (funcall format-char t-str))
				(push (format "%s：%s" t-str code) lines)))
			    (unless (or same (gethash s-str seen-chars))
			      (puthash s-str t seen-chars)
			      (when-let (code (funcall format-char s-str))
				(push (format "%s：%s" s-str code) lines)))))
	      (when lines
		(my-cedict-pinyin-log (mapconcat #'identity (nreverse lines) "\n"))))))
      (my-cedict-pinyin-log (format "No entries found for pinyin: %s" query)))))

(defun my-cedict-normalize-pinyin (query)
  "Normalize pinyin input like \"peng2you\" or \"peng2 you\" to CEDICT form."
  (let* ((s (string-trim query))
         ;; insert space between digit and following letter
         (s (replace-regexp-in-string
             "\\([1-5]\\)\\([a-züāáǎàēéěèīíǐìōóǒòūúǔùǖǘǚǜ]\\)"
             "\\1 \\2" s))
         ;; add tone 5 to any toneless final syllable
         (s (replace-regexp-in-string
             "\\([a-züāáǎàēéěèīíǐìōóǒòūúǔùǖǘǚǜ]\\)\\( \\|$\\)"
             "\\15\\2" s)))
    s))

;;--------------------------------------------------------------------cangjie lookup

(defvar cangjie-table nil
  "Hash table mapping characters to Cangjie codes.")

(defvar cangjie-key-map
  '((?a . "日") (?b . "月") (?c . "金") (?d . "木") (?e . "水")
    (?f . "火") (?g . "土") (?h . "竹") (?i . "戈") (?j . "十")
    (?k . "大") (?l . "中") (?m . "一") (?n . "弓") (?o . "人")
    (?p . "心") (?q . "手") (?r . "口") (?s . "尸") (?t . "廿")
    (?u . "山") (?v . "女") (?w . "田") (?x . "難") (?y . "卜")
    (?z . "重"))
  "Mapping from Cangjie key letters to their radical characters.")

(defun cangjie-code-to-radicals (code)
  "Convert a Cangjie code string like \"otg\" to radicals like \"人卜廿\"."
  (mapconcat (lambda (c)
               (or (cdr (assoc c cangjie-key-map)) (string c)))
             code ""))

(defun cangjie-load-table ()
  "Load the Cangjie code table from file."
  (let ((file (expand-file-name "~/.emacs.d/cangjie-5-all-codes.txt"))
        (table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (search-forward "%chardef begin")
      (forward-line 1)
      (while (not (eobp))
        (let ((line (buffer-substring-no-properties
                     (line-beginning-position)
                     (line-end-position))))
          (when (string-match "^\\([a-z]+\\)\\s-+\\(.+\\)$" line)
            (let ((code (match-string 1 line))
                  (char (match-string 2 line)))
              (let ((existing (gethash char table)))
                (puthash char
                         (if existing
                             (concat existing " / " code)
                           code)
                         table)))))
        (forward-line 1)))
    table))

(defun cangjie-lookup-char (char)
  "Look up the Cangjie code for CHAR and display radicals in minibuffer."
  (interactive (list (char-to-string (char-after (point)))))
  (unless cangjie-table
    (message "Loading Cangjie table...")
    (setq cangjie-table (cangjie-load-table)))
  (let ((code (gethash char cangjie-table)))
    (if code
        (message "%s" (mapconcat #'cangjie-code-to-radicals
                            (split-string code " / ")
                            " / "))
      (message "No Cangjie code found for: %s" char))))

;;------------------------------------------------------------------bopomofo

(defvar my-pinyin-to-zhuyin-map
  '(;; special whole-syllable mappings first
    ("zhi" . "ㄓ") ("chi" . "ㄔ") ("shi" . "ㄕ") ("ri"  . "ㄖ")
    ("zi"  . "ㄗ") ("ci"  . "ㄘ") ("si"  . "ㄙ")
    ;; initials
    ("zh" . "ㄓ") ("ch" . "ㄔ") ("sh" . "ㄕ")
    ("b" . "ㄅ") ("p" . "ㄆ") ("m" . "ㄇ") ("f" . "ㄈ")
    ("d" . "ㄉ") ("t" . "ㄊ") ("n" . "ㄋ") ("l" . "ㄌ")
    ("g" . "ㄍ") ("k" . "ㄎ") ("h" . "ㄏ")
    ("j" . "ㄐ") ("q" . "ㄑ") ("x" . "ㄒ")
    ("r" . "ㄖ") ("z" . "ㄗ") ("c" . "ㄘ") ("s" . "ㄙ")
    ;; finals (longest first)
    ("iang" . "ㄧㄤ") ("iong" . "ㄩㄥ") ("uang" . "ㄨㄤ")
    ("ian" . "ㄧㄢ") ("iao" . "ㄧㄠ") ("ing" . "ㄧㄥ")
    ("uai" . "ㄨㄞ") ("uan" . "ㄨㄢ") ("uen" . "ㄨㄣ") ("ueng" . "ㄨㄥ")
    ("ang" . "ㄤ") ("eng" . "ㄥ") ("ong" . "ㄨㄥ")
    ("ian" . "ㄧㄢ") ("iao" . "ㄧㄠ") ("iou" . "ㄧㄡ")
    ("ai" . "ㄞ") ("ei" . "ㄟ") ("ao" . "ㄠ") ("ou" . "ㄡ")
    ("an" . "ㄢ") ("en" . "ㄣ") ("er" . "ㄦ")
    ("ia" . "ㄧㄚ") ("ie" . "ㄧㄝ") ("in" . "ㄧㄣ")
    ("ua" . "ㄨㄚ") ("uo" . "ㄨㄛ") ("ui" . "ㄨㄟ") ("un" . "ㄨㄣ")
    ("van" . "ㄩㄢ") ("vn" . "ㄩㄣ") ("ve" . "ㄩㄝ") ("v" . "ㄩ")
    ("a" . "ㄚ") ("o" . "ㄛ") ("e" . "ㄜ")
    ("i" . "ㄧ") ("u" . "ㄨ")))

(defvar my-zhuyin-tone-map
  '((1 . "") (2 . "ˊ") (3 . "ˇ") (4 . "ˋ") (5 . "˙")))

(defun my-pinyin-syllable-to-zhuyin (syllable)
  "Convert a single pinyin syllable with tone number to zhuyin."
  (if (not (string-match "^\\(.*\\)\\([1-5]\\)$" syllable))
      syllable
    (let* ((body (match-string 1 syllable))
           (tone (string-to-number (match-string 2 syllable)))
           (tone-mark (cdr (assoc tone my-zhuyin-tone-map))))
      ;; normalise y/w spellings to their zhuyin equivalents
      (let* ((s (replace-regexp-in-string "^yi" "i" body))
             (s (replace-regexp-in-string "^yu" "v" s))
             (s (replace-regexp-in-string "^y" "i" s))
             (s (replace-regexp-in-string "^wu" "u" s))
             (s (replace-regexp-in-string "^w" "u" s))
             (s (replace-regexp-in-string "iu" "iou" s))
             (s (replace-regexp-in-string "ui$" "uei" s))
             (s (replace-regexp-in-string "un$" "uen" s))
             (s (replace-regexp-in-string "ü" "v" s))
             (remaining s)
             (result "")
             (continue t))
        (while (and continue (not (string= remaining "")))
          (let ((matched nil))
            (dolist (pair my-pinyin-to-zhuyin-map)
              (when (and (not matched)
                         (string-prefix-p (car pair) remaining))
                (setq result (concat result (cdr pair)))
                (setq remaining (substring remaining (length (car pair))))
                (setq matched t)))
            (unless matched
              (setq result (concat result (substring remaining 0 1)))
              (setq remaining (substring remaining 1)))))
        (concat result tone-mark)))))

(defun my-pinyin-to-zhuyin (pinyin)
  "Convert a pinyin string like 'ni3 hao3' to zhuyin 'ㄋㄧˇ ㄏㄠˇ'."
  (mapconcat #'my-pinyin-syllable-to-zhuyin
             (split-string pinyin " " t)
             " "))

(provide 'my-cedict)
;;; my-cedict.el ends here
