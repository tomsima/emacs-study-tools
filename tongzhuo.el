;; tongzhuo.el --- CEDICT Chinese dictionary lookup -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar my-cedict-file "~/.emacs.d/cedict_ts.u8")
(defvar my-cedict-table nil)
(defvar my-cedict-romanisation 'pinyin)
(defvar my-cedict-last-word nil
  "The last word looked up, used for romanisation toggle refresh.")
(defvar my-kanji-file "~/.emacs.d/kanji-hepburn.txt")
(defvar my-korean-hanja-file "~/.emacs.d/korean-hanja-2136.txt")

(defun my-cedict-build-table ()
  (setq my-cedict-table (make-hash-table :test 'equal :size 120000))
  (with-temp-buffer
    (insert-file-contents my-cedict-file)
    (goto-char (point-min))
    (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\(\\[[^]]+\\] /.*\\)$" nil t)
      (let ((trad (match-string 1))
            (simp (match-string 2))
            (rest (match-string 3)))
        (push (cons trad rest) (gethash trad my-cedict-table))
        (unless (string= trad simp)
          (push (cons trad rest) (gethash simp my-cedict-table))))))
  ;; `push' above prepends, so each character's entry list ends up in
  ;; the reverse of the order the lines appear in the .u8 file.  Put
  ;; it back to file order so downstream code (e.g. phonetic-lookup)
  ;; displays entries in the same order CEDICT lists them.
  (maphash (lambda (k v) (puthash k (nreverse v) my-cedict-table))
           my-cedict-table))

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

;;; Replace my-cedict-lookup:

(defun my-cedict-lookup (word)
  (unless my-cedict-table (my-cedict-build-table))
  (let ((entries (gethash word my-cedict-table)))
    (when entries
      (let* ((index 0)
             (results
              (delq nil
                    (mapcar (lambda (item)
                              (let ((entry (cdr item)))
                                (when (string-match "\\[\\([^]]+\\)\\] /\\(.*\\)" entry)
                                  (let* ((pinyin (match-string 1 entry))
                                         (defs (match-string 2 entry))
                                         (w (if (= index 0)
                                                (format "*%s*" word)
                                              (make-string (+ 4 (string-width word)) ?\s)))
                                         (line (format "%s _%s_ /%s/"
					     w
					     (my-pinyin-convert pinyin)

					     ;;(yale pinyin)
					     (string-trim
					      (replace-regexp-in-string
					       ", $" ""
					       (replace-regexp-in-string
						"/" ", "
						(replace-regexp-in-string
						 "\\[\\([^]]+\\)\\]" "" defs)))))))
                                    (prog1 line (setq index (1+ index)))))))
                            entries))))
        (when results
          (mapconcat #'identity results "\n"))))))

(defun my-chinese-lookup ()
  (interactive)
  (unless my-cedict-table
    (message "loading dictionary...")
    (my-cedict-build-table))
  (let* ((word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (my-chinese-word-at-point)))
         (result (my-cedict-lookup word)))
    (setq my-cedict-last-word word)
    (my-cedict-ensure-window)
    (if result
        (my-cedict-log result)
      (my-cedict-log (format "no entry found for: %s" word)))))

;; (defun my-cedict-tsangyin-minibuffer ()
;;   "Show tsangyin romanisation for the word at point in the minibuffer."
;;   (interactive)
;;   (unless my-cedict-table (my-cedict-build-table))
;;   (let* ((word (if (use-region-p)
;;                    (buffer-substring-no-properties (region-beginning) (region-end))
;;                  (my-chinese-word-at-point)))
;;          (entries (gethash word my-cedict-table)))
;;     (if (not entries)
;;         (message "no entry for: %s" word)
;;       (message "%s"
;;                (mapconcat #'identity
;;                           (delq nil
;;                                 (mapcar (lambda (entry)
;;                                           (when (string-match "\\[\\([^]]+\\)\\]" entry)
;;                                             (tsangyin (match-string 1 entry))))
;;                                         entries))
;;                           "  ")))))

(defvar my-cedict-log-buffer "*辭典*")

(defun my-cedict-log (result)
  (with-current-buffer (get-buffer-create my-cedict-log-buffer)
    (when (= (buffer-size) 0)
      (org-mode))
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert result "\n"))
    (when-let (win (get-buffer-window my-cedict-log-buffer))
      (set-window-point win (point-max)))))

(defun my-cedict-ensure-window ()
  (unless (get-buffer-window my-cedict-log-buffer)
    (let ((win (condition-case nil
                   (split-window-right (/ (* (frame-width) 2) 3))
                 (error (cl-reduce (lambda (a b)
                                     (if (< (window-width a) (window-width b)) a b))
                                   (window-list))))))
      (set-window-buffer win (get-buffer-create my-cedict-log-buffer))))
  (with-current-buffer my-cedict-log-buffer
    (read-only-mode 1)))

;; (defun my-chinese-lookup ()
;;   (interactive)
;;   (unless my-cedict-table
;;     (message "loading dictionary...")
;;     (my-cedict-build-table))
;;   (let* ((word (if (use-region-p)
;;                    (buffer-substring-no-properties (region-beginning) (region-end))
;;                  (my-chinese-word-at-point)))
;;          (result (my-cedict-lookup word)))
;;     (setq my-cedict-last-word word)
;;     (my-cedict-ensure-window)
;;     (if result
;;         (my-cedict-log result)
;;       (my-cedict-log (format "no entry found for: %s" word)))))

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

;;--------------------------------------------------------------pinyin search for cangjie code

(defvar my-cedict-pinyin-log-buffer "*倉頡*")

(defun my-cedict-pinyin-ensure-window ()
  (unless (get-buffer-window my-cedict-pinyin-log-buffer)
    (let ((win (condition-case nil
                   (split-window-right (/ (* (frame-width) 2) 3))
                 (error (cl-reduce (lambda (a b)
                                     (if (< (window-width a) (window-width b)) a b))
                                   (window-list))))))
      (set-window-buffer win (get-buffer-create my-cedict-pinyin-log-buffer))
      (with-current-buffer my-cedict-pinyin-log-buffer
        (fundamental-mode)
        (read-only-mode 1))
      (with-selected-window win
        (text-scale-set 3)))))

(defun my-cedict-pinyin-log (result)
  (with-current-buffer (get-buffer-create my-cedict-pinyin-log-buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert result "\n"))))

(defun my-cedict-search-by-pinyin (pinyin-query)
  (interactive "scangjie search: ")
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
    (with-current-buffer (get-buffer-create my-cedict-pinyin-log-buffer)
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (if matches
        (let ((seen-chars (make-hash-table :test 'equal))
              (file-lines '()))
          (let* ((first-pair (car matches))
                 (trad-str (car first-pair))
                 (simp-str (cdr first-pair))
                 (single-word (cl-every (lambda (p) (string= (car p) trad-str)) matches))
                 (diff-chars (cl-loop for tc across trad-str
                                      for sc across simp-str
                                      collect (if (char-equal tc sc)
                                                  "-"
                                                (char-to-string sc))))
                 (all-same (cl-every (lambda (s) (string= s "-")) diff-chars))
                 (header (if (or (= (length trad-str) 1) (not single-word))
                             (my-pinyin-to-zhuyin query)
                           (format "%s%s %s"
                                   trad-str
                                   (if all-same
                                       ""
                                     (format "（%s）\n" (mapconcat #'identity diff-chars "")))
                                   (my-pinyin-to-zhuyin query)))))
            (my-cedict-pinyin-log
             (propertize header 'face '(:height 0.9 :foreground "tomato"))))
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
                                (push (format "%s：%s" t-str code) file-lines)
                                (push (if same
                                          (format "%s：%s" t-str code)
                                        (propertize (format "%s：%s" t-str code)
                                                    'face '(:foreground "slate gray")))
                                      lines)))
                            (unless (or same (gethash s-str seen-chars))
                              (puthash s-str t seen-chars)
                              (when-let (code (funcall format-char s-str))
                                (push (format "%s：%s" s-str code) file-lines)
                                (push (format "%s：%s" s-str code) lines)))))
              (when lines
                (my-cedict-pinyin-log
                 (mapconcat #'identity (nreverse lines) "\n")))))
          (my-cedict-pinyin-save-to-file query (nreverse file-lines)))
      (my-cedict-pinyin-log (format "n/a" query)))))

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

;;--------------------------------------------------------------------cangjie code query

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
    (message "loading cangjie table...")
    (setq cangjie-table (cangjie-load-table)))
  (let ((code (gethash char cangjie-table)))
    (if code
        (message "%s" (mapconcat #'cangjie-code-to-radicals
                            (split-string code " / ")
                            " / "))
      (message "No Cangjie code found for: %s" char))))

;;------------------------------------------------------------------pinyin to bopomofo conversion

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
    (let* ((body (downcase (match-string 1 syllable)))
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



;;------------------------------------------------------write checks to cangjie-check.org file

(defun my-cedict-pinyin-save-to-file (query seen-chars-data)
  (let ((file (expand-file-name "~/Documents/cangjie-check.org")))
    (with-temp-buffer
      (insert (format "* %s\n" (my-pinyin-to-zhuyin query)))
      (dolist (item seen-chars-data)
        (insert item "\n"))
      (write-region (point-min) (point-max) file t 'silent))))

;;-------------------------------------------------------tsangyin

(defvar tsangyin-tone-symbols
  '((1 . "⤒") (2 . "↗") (3 . "⤓") (4 . "↘") (5 . "") (0 . "")))

;;-------------------------------------------------------tsangyin (hash table version)

(defvar pinyin-to-tsangyin-map
  #s(hash-table test equal data
     (
      ;; bare finals
      "a"    "a"      "ai"   "ai"     "an"   "an"     "ang"  "ang"
      "ao"   "ao"     "e"    "uh"     "ei"   "ei"     "en"   "en"
      "er"   "er"     "ou"   "ou"
      ;; b
      "ba"   "ba"     "bai"  "bai"    "ban"  "ban"    "bang" "bang"
      "bao"  "bao"    "bei"  "bei"    "ben"  "ben"    "beng" "bung"
      "bi"   "bi"     "bian" "bien"   "biao" "biao"   "bie"  "bie"
      "bin"  "bin"    "bing" "bing"   "bo"   "bwo"    "bu"   "bu"
      ;; ch (pinyin ch = ㄔ)
      "cha"    "cha"     "chai"   "chai"    "chan"   "chan"    "chang"  "chang"
      "chao"   "chao"    "che"    "chuh"    "chen"   "chen"    "cheng"  "chung"
      "chi"    "chr"     "chong"  "chong"   "chou"   "chou"    "chu"    "chu"
      "chua"   "chwa"    "chuai"  "chwai"   "chuan"  "chuan"   "chuang" "chuang"
      "chui"   "chwei"   "chun"   "chwun"   "chuo"   "chwo"
      ;; c (ㄘ)
      "ca"   "tsa"    "cai"  "tsai"   "can"  "tsan"   "cang" "tsang"
      "cao"  "tsao"   "ce"   "tsuh"   "cen"  "tsen"   "ceng" "tsung"
      "ci"   "tsz"    "cong" "tsong"  "cou"  "tsou"   "cu"   "tsoo"
      "cuan" "tsuan"  "cui"  "tswei"  "cun"  "tswun"  "cuo"  "tswo"
      ;; d
      "da"   "da"     "dai"  "dai"    "dan"  "dan"    "dang" "dang"
      "dao"  "dao"    "de"   "duh"    "dei"  "dei"    "den"  "den"
      "deng" "dung"   "di"   "di"     "dian" "dien"   "diang""diang"
      "diao" "diao"   "die"  "dieh"   "ding" "ding"   "diu"  "diou"
      "dong" "dong"   "dou"  "dou"    "du"   "du"     "duan" "dwan"
      "dui"  "dwei"   "dun"  "dwun"   "duo"  "dwoh"
      ;; f
      "fa"   "fa"     "fan"  "fan"    "fang" "fang"   "fei"  "fei"
      "fen"  "fen"    "feng" "fung"   "fo"   "fwoh"   "fou"  "fou"
      "fu"   "fu"
      ;; g
      "ga"    "ga"     "gai"   "gai"    "gan"   "gan"    "gang"  "gang"
      "gao"   "gao"    "ge"    "guh"    "gei"   "gei"    "gen"   "gen"
      "geng"  "gung"   "gong"  "gong"   "gou"   "gou"    "gu"    "gu"
      "gua"   "gwah"   "guai"  "gwai"   "guan"  "gwan"   "guang" "gwang"
      "gui"   "gwei"   "gun"   "gwun"   "guo"   "gwoh"
      ;; h
      "ha"    "ha"     "hai"   "hai"    "han"   "han"    "hang"  "hang"
      "hao"   "hao"    "he"    "huuh"   "hei"   "hei"    "hen"   "hen"
      "heng"  "hung"   "hong"  "hong"   "hou"   "hou"    "hu"    "hu"
      "hua"   "hwa"    "huai"  "hwai"   "huan"  "hwan"   "huang" "hwang"
      "hui"   "hwei"   "hun"   "hwun"   "huo"   "hwoh"
      ;; j (pinyin j = ㄐ)
      "ji"    "ji"     "jia"   "jia"    "jian"  "jian"   "jiang" "jiang"
      "jiao"  "jiao"   "jie"   "jie"    "jin"   "jin"    "jing"  "jing"
      "jiong" "jiong"  "jiu"   "jiu"    "ju"    "ju"     "juan"  "juen"
      "jue"   "jue"    "jun"   "jun"
      ;; k
      "ka"    "ka"     "kai"   "kai"    "kan"   "kan"    "kang"  "kang"
      "kao"   "kao"    "ke"    "kuuh"   "ken"   "ken"    "keng"  "kung"
      "kong"  "kong"   "kou"   "kou"    "ku"    "koo"    "kua"   "kwa"
      "kuai"  "kwai"   "kuan"  "kwan"   "kuang" "kwang"  "kui"   "kwei"
      "kun"   "kwun"   "kuo"   "kwo"
      ;; l
      "la"    "la"     "lai"   "lai"    "lan"   "lan"    "lang"  "lang"
      "lao"   "lao"    "le"    "luuh"   "lei"   "lei"    "leng"  "lung"
      "li"    "li"     "lia"   "lia"    "lian"  "lien"   "liang" "liang"
      "liao"  "liao"   "lie"   "lie"    "lin"   "lin"    "ling"  "ling"
      "liu"   "liou"   "lo"    "loh"    "long"  "long"   "lou"   "lou"
      "lu"    "lu"     "luan"  "lwan"   "lun"   "lwun"   "luo"   "lwo"
      "lü"    "lü"     "lüe"   "lüe"    "lün"   "lün"
      ;; m
      "ma"   "ma"     "mai"  "mai"    "man"  "man"    "mang" "mang"
      "mao"  "mao"    "me"   "muuh"   "mei"  "mei"    "men"  "men"
      "meng" "meng"   "mi"   "mi"     "mian" "mien"   "miao" "miao"
      "mie"  "mie"    "min"  "min"    "ming" "ming"   "miu"  "miou"
      "mo"   "moh"    "mou"  "mou"    "mu"   "mu"
      ;; n
      "na"    "na"     "nai"   "nai"    "nan"   "nan"    "nang"  "nang"
      "nao"   "nao"    "ne"    "nuuh"   "nei"   "nei"    "nen"   "nen"
      "neng"  "nung"   "ni"    "ni"     "nia"   "nia"    "nian"  "nien"
      "niang" "niang"  "niao"  "niao"   "nie"   "nie"    "nin"   "nin"
      "ning"  "ning"   "niu"   "niu"    "nong"  "nong"   "nou"   "nou"
      "nu"    "nu"     "nuan"  "nwan"   "nun"   "nwun"   "nuo"   "nwoh"
      "nü"    "nü"     "nüe"   "nüe"
      ;; p
      "pa"   "pa"     "pai"  "pai"    "pan"  "pan"    "pang" "pang"
      "pao"  "pao"    "pei"  "pei"    "pen"  "pen"    "peng" "pung"
      "pi"   "pi"     "pian" "pien"   "piao" "piao"   "pie"  "pie"
      "pin"  "pin"    "ping" "ping"   "po"   "poh"    "pou"  "pou"
      "pu"   "pu"
      ;; q (pinyin q = ㄑ)
      "qi"    "chi"    "qia"   "chia"   "qian"  "chien"  "qiang" "chiang"
      "qiao"  "chiao"  "qie"   "chie"   "qin"   "chin"   "qing"  "ching"
      "qiong" "chiong" "qiu"   "chiou"  "qu"    "chu"    "quan"  "chwan"
      "que"   "chwe"   "qun"   "chun"
      ;; r
      "ran"  "ran"    "rang" "rang"   "rao"  "rao"    "re"   "ruuh"
      "ren"  "ren"    "reng" "rung"   "ri"   "rr"     "rong" "rong"
      "rou"  "rou"    "ru"   "ru"     "ruan" "rwan"   "rui"  "rwei"
      "run"  "rwun"   "ruo"  "rwoh"
      ;; s
      "sa"   "sa"     "sai"  "sai"    "san"  "san"    "sang" "sang"
      "sao"  "sao"    "se"   "suuh"   "sei"  "sei"    "sen"  "sen"
      "seng" "sung"   "si"   "sz"     "song" "song"   "sou"  "sou"
      "su"   "soo"    "suan" "swan"   "sui"  "swei"   "sun"  "swun"
      "suo"  "swoh"
      ;; sh
      "sha"    "sha"    "shai"   "shai"   "shan"   "shan"   "shang"  "shang"
      "shao"   "shao"   "she"    "shuuh"  "shei"   "shei"   "shen"   "shen"
      "sheng"  "shung"  "shi"    "shr"    "shong"  "shong"  "shou"   "shou"
      "shu"    "shoo"   "shua"   "shwa"   "shuai"  "shwai"  "shuan"  "shwan"
      "shuang" "shwang" "shui"   "shwei"  "shun"   "shwun"  "shuo"   "shwo"
      ;; t
      "ta"   "ta"     "tai"  "tai"    "tan"  "tan"    "tang" "tang"
      "tao"  "tao"    "te"   "tuuh"   "teng" "tung"   "ti"   "ti"
      "tian" "tien"   "tiao" "tiao"   "tie"  "tie"    "ting" "ting"
      "tong" "tong"   "tou"  "tou"    "tu"   "too"    "tuan" "twan"
      "tui"  "twei"   "tun"  "twun"   "tuo"  "twoh"
      ;; w
      "wa"   "wa"     "wai"  "wai"    "wan"  "wan"    "wang" "wang"
      "wei"  "wei"    "wen"  "wen"    "weng" "wung"   "wo"   "woh"
      "wu"   "woo"
      ;; x (pinyin x = ㄒ)
      "xi"    "shee"   "xia"   "shia"   "xian"  "shien"  "xiang" "shiang"
      "xiao"  "shiao"  "xie"   "shie"   "xin"   "shin"   "xing"  "shing"
      "xiong" "shiong" "xiu"   "shiou"  "xu"    "shu"    "xuan"  "shuen"
      "xue"   "shue"   "xun"   "shun"
      ;; y
      "ya"   "ya"     "yan"  "yen"    "yang" "yang"   "yao"  "yao"
      "ye"   "ye"     "yi"   "yi"     "yin"  "yin"    "ying" "ying"
      "yong" "yong"   "you"  "you"    "yu"   "yü"     "yuan" "yüen"
      "yue"  "yüe"    "yun"  "yün"
      ;; z (pinyin z = ㄗ)
      "za"   "dzah"   "zai"  "dzai"   "zan"  "dzan"   "zang" "dzang"
      "zao"  "dzao"   "ze"   "dzuh"   "zei"  "dzei"   "zen"  "dzen"
      "zeng" "dzung"  "zi"   "dzz"    "zong" "dzong"  "zou"  "dzou"
      "zu"   "dzu"    "zuan" "dzwan"  "zui"  "dzwei"  "zun"  "dzwun"
      "zuo"  "dzwoh"
      ;; zh (pinyin zh = ㄓ)
      "zha"    "ja"     "zhai"   "jai"    "zhan"   "jan"    "zhang"  "jang"
      "zhao"   "jao"    "zhe"    "juuh"   "zhei"   "jei"    "zhen"   "jen"
      "zheng"  "jung"   "zhi"    "jr"     "zhong"  "jong"   "zhou"   "jou"
      "zhu"    "joo"    "zhua"   "jwa"    "zhuai"  "jwai"   "zhuan"  "jwan"
      "zhuang" "jwang"  "zhui"   "jwei"   "zhun"   "jwun"   "zhuo"   "jwoh"
     ))
  "Hash table mapping pinyin syllable bodies to tsangyin equivalents.")

(defun tsangyin-add-tone (syllable tone)
  "Add tone symbol to a tsangyin SYLLABLE using tsangyin-tone-symbols."
  (let ((symbol (cdr (assoc tone tsangyin-tone-symbols))))
    (concat syllable symbol)))

(defun tsangyin (pinyin)
  "Convert CEDICT numeric pinyin (e.g. \"ni3 hao3\") to tsangyin with tone symbols."
  (mapconcat
   (lambda (syllable)
     (if (not (string-match "^\\(.*\\)\\([1-5]\\)$" syllable))
         syllable
       (let* ((body (downcase (match-string 1 syllable)))
              (tone (string-to-number (match-string 2 syllable)))
              (body (replace-regexp-in-string "v" "ü" body))
              (tsang-body (or (gethash body pinyin-to-tsangyin-map) body)))
         (tsangyin-add-tone tsang-body tone))))
   (split-string pinyin " " t)
   " "))

;;------------------------------------------------------------------yale

;;-------------------------------------------------------yale romanisation

(defvar pinyin-to-yale-map
  #s(hash-table test equal data
     (
      ;; a
      "a" "a"  "ai" "ai"  "an" "an"  "ang" "ang"  "ao" "au"
      "e" "e"  "ei" "ei"  "en" "en"  "er" "er"  "ou" "ou"
      ;; b
      "ba" "ba"  "bai" "bai"  "ban" "ban"  "bang" "bang"  "bao" "bau"
      "bei" "bei"  "ben" "ben"  "beng" "beng"  "bi" "bi"  "bian" "byan"
      "biao" "byau"  "bie" "bye"  "bin" "bin"  "bing" "bing"  "bo" "bwo"
      "bu" "bu"
      ;; ch- (pinyin ch = Yale ch for zh-derived; pinyin q = Yale ch)
      "cha" "cha"  "chai" "chai"  "chan" "chan"  "chang" "chang"  "chao" "chau"
      "che" "che"  "chen" "chen"  "cheng" "cheng"  "chi" "chr"  "chong" "chung"
      "chou" "chou"  "chu" "chu"  "chua" "chwa"  "chuai" "chwai"  "chuan" "chwan"
      "chuang" "chwang"  "chui" "chwei"  "chun" "chwen"  "chuo" "chwo"
      ;; c- (pinyin c = Yale ts)
      "ca" "tsa"  "cai" "tsai"  "can" "tsan"  "cang" "tsang"  "cao" "tsau"
      "ce" "tse"  "cen" "tsen"  "ceng" "tseng"  "ci" "tsz"  "cong" "tsung"
      "cou" "tsou"  "cu" "tsu"  "cuan" "tswan"  "cui" "tswei"  "cun" "tswen"
      "cuo" "tswo"
      ;; d
      "da" "da"  "dai" "dai"  "dan" "dan"  "dang" "dang"  "dao" "dau"
      "de" "de"  "dei" "dei"  "den" "den"  "deng" "deng"  "di" "di"
      "dian" "dyan"  "diang" "dyang"  "diao" "dyau"  "die" "dye"  "ding" "ding"
      "diu" "dyou"  "dong" "dung"  "dou" "dou"  "du" "du"  "duan" "dwan"
      "dui" "dwei"  "dun" "dwen"  "duo" "dwo"
      ;; f
      "fa" "fa"  "fan" "fan"  "fang" "fang"  "fei" "fei"  "fen" "fen"
      "feng" "feng"  "fo" "fwo"  "fou" "fou"  "fu" "fu"
      ;; g
      "ga" "ga"  "gai" "gai"  "gan" "gan"  "gang" "gang"  "gao" "gau"
      "ge" "ge"  "gei" "gei"  "gen" "gen"  "geng" "geng"  "gong" "gung"
      "gou" "gou"  "gu" "gu"  "gua" "gwa"  "guai" "gwai"  "guan" "gwan"
      "guang" "gwang"  "gui" "gwei"  "gun" "gwun"  "guo" "gwo"
      ;; h
      "ha" "ha"  "hai" "hai"  "han" "han"  "hang" "hang"  "hao" "hau"
      "he" "he"  "hei" "hei"  "hen" "hen"  "heng" "heng"  "hong" "hung"
      "hou" "hou"  "hu" "hu"  "hua" "hwa"  "huai" "hwai"  "huan" "hwan"
      "huang" "hwang"  "hui" "hwei"  "hun" "hwen"  "huo" "hwo"
      ;; j- (pinyin j = Yale jy)
      "ji" "ji"  "jia" "jya"  "jian" "jyan"  "jiang" "jyang"  "jiao" "jyau"
      "jie" "jye"  "jin" "jin"  "jing" "jing"  "jiong" "jyung"  "jiu" "jyou"
      "ju" "jyu"  "juan" "jywan"  "jue" "jywe"  "jun" "jyun"
      ;; k
      "ka" "ka"  "kai" "kai"  "kan" "kan"  "kang" "kang"  "kao" "kau"
      "ke" "ke"  "ken" "ken"  "keng" "keng"  "kong" "kung"  "kou" "kou"
      "ku" "ku"  "kua" "kwa"  "kuai" "kwai"  "kuan" "kwan"  "kuang" "kwang"
      "kui" "kwei"  "kun" "kwen"  "kuo" "kwo"
      ;; l
      "la" "la"  "lai" "lai"  "lan" "lan"  "lang" "lang"  "lao" "lau"
      "le" "le"  "lei" "lei"  "leng" "leng"  "li" "li"  "lia" "lya"
      "lian" "lyan"  "liang" "lyang"  "liao" "lyau"  "lie" "lye"  "lin" "lin"
      "ling" "ling"  "liu" "lyou"  "lo" "lo"  "long" "lung"  "lou" "lou"
      "lu" "lu"  "luan" "lwan"  "lun" "lwun"  "luo" "lwo"
      "lü" "lyu"  "lün" "lyun"  "lüe" "lywe"
      ;; m
      "ma" "ma"  "mai" "mai"  "man" "man"  "mang" "mang"  "mao" "mau"
      "me" "me"  "mei" "mei"  "men" "men"  "meng" "meng"  "mi" "mi"
      "mian" "myan"  "miao" "myau"  "mie" "mye"  "min" "min"  "ming" "ming"
      "miu" "myou"  "mo" "mwo"  "mou" "mou"  "mu" "mu"
      ;; n
      "na" "na"  "nai" "nai"  "nan" "nan"  "nang" "nang"  "nao" "nau"
      "ne" "ne"  "nei" "nei"  "nen" "nen"  "neng" "neng"  "ni" "ni"
      "nia" "nya"  "nian" "nyan"  "niang" "nyang"  "niao" "nyau"  "nie" "nye"
      "nin" "nin"  "ning" "ning"  "niu" "nyou"  "nong" "nung"  "nou" "nou"
      "nu" "nu"  "nuan" "nwan"  "nun" "nwen"  "nuo" "nwo"
      "nü" "nyu"  "nüe" "nywe"
      ;; p
      "pa" "pa"  "pai" "pai"  "pan" "pan"  "pang" "pang"  "pao" "pau"
      "pei" "pei"  "pen" "pen"  "peng" "peng"  "pi" "pi"  "pian" "pyan"
      "piao" "pyau"  "pie" "pye"  "pin" "pin"  "ping" "ping"  "po" "pwo"
      "pou" "pou"  "pu" "pu"
      ;; q- (pinyin q = Yale ch)
      "qi" "chi"  "qia" "chya"  "qian" "chyan"  "qiang" "chyang"  "qiao" "chyau"
      "qie" "chye"  "qin" "chin"  "qing" "ching"  "qiong" "chyung"  "qiu" "chyou"
      "qu" "chyu"  "quan" "chywan"  "que" "chywe"  "qun" "chyun"
      ;; r
      "ran" "ran"  "rang" "rang"  "rao" "rau"  "re" "re"  "ren" "ren"
      "reng" "reng"  "ri" "r"  "rong" "rung"  "rou" "rou"  "ru" "ru"
      "ruan" "rwan"  "rui" "rwei"  "run" "rwun"  "ruo" "rwo"
      ;; s
      "sa" "sa"  "sai" "sai"  "san" "san"  "sang" "sang"  "sao" "sau"
      "se" "se"  "sei" "sei"  "sen" "sen"  "seng" "seng"  "si" "sz"
      "song" "sung"  "sou" "sou"  "su" "su"  "suan" "swan"  "sui" "swei"
      "sun" "swen"  "suo" "swo"
      ;; sh
      "sha" "sha"  "shai" "shai"  "shan" "shan"  "shang" "shang"  "shao" "shau"
      "she" "she"  "shei" "shei"  "shen" "shen"  "sheng" "sheng"  "shi" "shr"
      "shong" "shung"  "shou" "shou"  "shu" "shu"  "shua" "shwa"  "shuai" "shwai"
      "shuan" "shwan"  "shuang" "shwang"  "shui" "shwei"  "shun" "shwun"  "shuo" "shwo"
      ;; t
      "ta" "ta"  "tai" "tai"  "tan" "tan"  "tang" "tang"  "tao" "tau"
      "te" "te"  "teng" "teng"  "ti" "ti"  "tian" "tyan"  "tiao" "tyau"
      "tie" "tye"  "ting" "ting"  "tong" "tung"  "tou" "tou"  "tu" "tu"
      "tuan" "twan"  "tui" "twei"  "tun" "twen"  "tuo" "two"
      ;; w
      "wa" "wa"  "wai" "wai"  "wan" "wan"  "wang" "wang"  "wei" "wei"
      "wen" "wen"  "weng" "weng"  "wo" "wo"  "wu" "wu"
      ;; x- (pinyin x = Yale sy)
      "xi" "syi"  "xia" "sya"  "xian" "syan"  "xiang" "syang"  "xiao" "syau"
      "xie" "sye"  "xin" "syin"  "xing" "sying"  "xiong" "syung"  "xiu" "syou"
      "xu" "syu"  "xuan" "sywan"  "xue" "sywe"  "xun" "syun"
      ;; y
      "ya" "ya"  "yan" "yan"  "yang" "yang"  "yao" "yau"  "ye" "ye"
      "yi" "yi"  "yin" "yin"  "ying" "ying"  "yong" "yung"  "you" "you"
      "yu" "yu"  "yuan" "ywan"  "yue" "ywe"  "yun" "yun"
      ;; z- (pinyin z = Yale dz)
      "za" "dza"  "zai" "dzai"  "zan" "dzan"  "zang" "dzang"  "zao" "dzau"
      "ze" "dze"  "zei" "dzei"  "zen" "dzen"  "zeng" "dzeng"  "zi" "dz"
      "zong" "dzung"  "zou" "dzou"  "zu" "dzu"  "zuan" "dzwan"  "zui" "dzwei"
      "zun" "dzwen"  "zuo" "dzwo"
      ;; zh- (pinyin zh = Yale j)
      "zha" "ja"  "zhai" "jai"  "zhan" "jan"  "zhang" "jang"  "zhao" "jau"
      "zhe" "je"  "zhei" "jei"  "zhen" "jen"  "zheng" "jeng"  "zhi" "jr"
      "zhong" "jung"  "zhou" "jou"  "zhu" "ju"  "zhua" "jwa"  "zhuai" "jwai"
      "zhuan" "jwan"  "zhuang" "jwang"  "zhui" "jwei"  "zhun" "jwen"  "zhuo" "jwo"
     ))
  "Hash table mapping pinyin syllable bodies to Yale equivalents.")

(defvar yale-tone-map
  '(("a" "ā" "á" "ǎ" "à")
    ("e" "ē" "é" "ě" "è")
    ("i" "ī" "í" "ǐ" "ì")
    ("o" "ō" "ó" "ǒ" "ò")
    ("u" "ū" "ú" "ǔ" "ù")
    ("ü" "ǖ" "ǘ" "ǚ" "ǜ"))
  "Tone mark table for Yale: (base tone1 tone2 tone3 tone4).")

(defun yale-add-tone (syllable tone)
  "Add tone mark to a Yale SYLLABLE (already converted from pinyin body).
TONE is 1-5; tone 5 (neutral) returns the syllable unchanged."
  (if (= tone 5)
      syllable
    (let* ((vowel (cond
                   ((string-match "a" syllable) "a")
                   ((string-match "e" syllable) "e")
                   ((string-match "ou" syllable) "o")
                   ((string-match "ü" syllable) "ü")
                   ((string-match "[iou]" syllable)
                    (let ((last nil))
                      (dolist (v '("i" "o" "u"))
                        (when (string-match v syllable) (setq last v)))
                      last))
                   ;; vocalic r syllables: jr, shr, chr, r
                   ((string-match "r" syllable) "r"))))
      (if (null vowel)
          syllable
        (let* ((row (assoc vowel yale-tone-map))
               (marked (if row
                           (nth tone row)
                         ;; r has no tone-mark row; use combining chars
                         (let ((marks ["" "̄" "́" "̌" "̀"]))
                           (concat "r" (aref marks tone))))))
          (if row
              (replace-regexp-in-string vowel marked syllable t t)
            ;; replace the final r with the marked version
            (replace-regexp-in-string "r$" marked syllable t t)))))))

(defun yale (pinyin)
  "Convert CEDICT numeric pinyin (e.g. \"ni3 hao3\") to Yale with tone marks."
  (mapconcat
   (lambda (syllable)
     (if (not (string-match "^\\(.*\\)\\([1-5]\\)$" syllable))
         syllable
       (let* ((body (downcase (match-string 1 syllable)))
              (tone (string-to-number (match-string 2 syllable)))
              (body (replace-regexp-in-string "v" "ü" body))
              (yale-body (or (gethash body pinyin-to-yale-map) body)))
         (yale-add-tone yale-body tone))))
   (split-string pinyin " " t)
   " "))

;;-------------------------------------------------------gwoyeu romatzyh

(defvar pinyin-to-gr-map
  #s(hash-table test equal data
     (
      ;; bare finals
      "a"    ("a"    "ar"    "aa"    "ah")
      "ai"   ("ai"   "air"   "ae"    "ay")
      "an"   ("an"   "arn"   "aan"   "ann")
      "ang"  ("ang"  "arng"  "aang"  "anq")
      "ao"   ("au"   "aur"   "ao"    "aw")
      "e"    ("e"    "er"    "ee"    "eh")
      "ei"   ("ei"   "eir"   "eei"   "ey")
      "en"   ("en"   "ern"   "een"   "enn")
      "er"   ("el"   "erl"   "eel"   "ell")
      "ou"   ("ou"   "our"   "oou"   "ow")
      ;; b
      "ba"   ("ba"   "bar"   "baa"   "bah")
      "bai"  ("bai"  "bair"  "bae"   "bay")
      "ban"  ("ban"  "barn"  "baan"  "bann")
      "bang" ("bang" "barng" "baang" "banq")
      "bao"  ("bau"  "baur"  "bao"   "baw")
      "bei"  ("bei"  "beir"  "beei"  "bey")
      "ben"  ("ben"  "bern"  "been"  "benn")
      "beng" ("beng" "berng" "beeng" "benq")
      "bi"   ("bi"   "byi"   "bii"   "bih")
      "bian" ("bian" "byan"  "bean"  "biann")
      "biao" ("biau" "byau"  "beau"  "biaw")
      "bie"  ("bie"  "bye"   "biee"  "bieh")
      "bin"  ("bin"  "byn"   "biin"  "binn")
      "bing" ("bing" "byng"  "biing" "binq")
      "bo"   ("bo"   "bor"   "boo"   "boh")
      "bu"   ("bu"   "bwu"   "buu"   "buh")
      ;; ch (pinyin ch = zhuyin ㄔ)
      "cha"    ("cha"    "char"    "chaa"    "chah")
      "chai"   ("chai"   "chair"   "chae"    "chay")
      "chan"   ("chan"   "charn"   "chaan"   "chann")
      "chang"  ("chang"  "charng"  "chaang"  "chanq")
      "chao"   ("chau"   "chaur"   "chao"    "chaw")
      "che"    ("che"    "cher"    "chee"    "cheh")
      "chen"   ("chen"   "chern"   "cheen"   "chenn")
      "cheng"  ("cheng"  "cherng"  "cheeng"  "chenq")
      "chi"    ("chy"    "chyr"    "chyy"    "chyh")
      "chong"  ("chong"  "chorng"  "choong"  "chonq")
      "chou"   ("chou"   "chour"   "choou"   "chow")
      "chu"    ("chu"    "chwu"    "chuu"    "chuh")
      "chua"   ("chua"   "chuar"   "chuaa"   "chuah")
      "chuai"  ("chuai"  "chwai"   "choai"   "chuay")
      "chuan"  ("chuan"  "chwan"   "choan"   "chuann")
      "chuang" ("chuang" "chwang"  "choang"  "chuanq")
      "chui"   ("chuei"  "chwei"   "choei"   "chuey")
      "chun"   ("chuen"  "chwen"   "choen"   "chuenn")
      "chuo"   ("chuo"   "chwo"    "chuoo"   "chuoh")
      ;; c (pinyin c = ㄘ)
      "ca"   ("tsa"   "tsar"   "tsaa"   "tsah")
      "cai"  ("tsai"  "tsair"  "tsae"   "tsay")
      "can"  ("tsan"  "tsarn"  "tsaan"  "tsann")
      "cang" ("tsang" "tsarng" "tsaang" "tsanq")
      "cao"  ("tsau"  "tsaur"  "tsao"   "tsaw")
      "ce"   ("tse"   "tser"   "tsee"   "tseh")
      "cen"  ("tsen"  "tsern"  "tseen"  "tsenn")
      "ceng" ("tseng" "tserng" "tseeng" "tsenq")
      "ci"   ("tsy"   "tsyr"   "tsyy"   "tsyh")
      "cong" ("tsong" "tsorng" "tsoong" "tsonq")
      "cou"  ("tsou"  "tsour"  "tsoou"  "tsow")
      "cu"   ("tsu"   "tswu"   "tsuu"   "tsuh")
      "cuan" ("tsuan" "tswan"  "tsoan"  "tsuann")
      "cui"  ("tsuei" "tswei"  "tsoei"  "tsuey")
      "cun"  ("tsuen" "tswen"  "tsoen"  "tsuenn")
      "cuo"  ("tsuo"  "tswo"   "tsuoo"  "tsuoh")
      ;; d
      "da"   ("da"   "dar"   "daa"   "dah")
      "dai"  ("dai"  "dair"  "dae"   "day")
      "dan"  ("dan"  "darn"  "daan"  "dann")
      "dang" ("dang" "darng" "daang" "danq")
      "dao"  ("dau"  "daur"  "dao"   "daw")
      "de"   ("de"   "der"   "dee"   "deh")
      "dei"  ("dei"  "deir"  "deei"  "dey")
      "den"  ("den"  "dern"  "deen"  "denn")
      "deng" ("deng" "derng" "deeng" "denq")
      "di"   ("di"   "dyi"   "dii"   "dih")
      "dian" ("dian" "dyan"  "dean"  "diann")
      "diang"("diang" "dyang" "deang" "dianq")
      "diao" ("diau" "dyau"  "deau"  "diaw")
      "die"  ("die"  "dye"   "diee"  "dieh")
      "ding" ("ding" "dyng"  "diing" "dinq")
      "diu"  ("diou" "dyou"  "deou"  "diow")
      "dong" ("dong" "dorng" "doong" "donq")
      "dou"  ("dou"  "dour"  "doou"  "dow")
      "du"   ("du"   "dwu"   "duu"   "duh")
      "duan" ("duan" "dwan"  "doan"  "duann")
      "dui"  ("duei" "dwei"  "doei"  "duey")
      "dun"  ("duen" "dwen"  "doen"  "duenn")
      "duo"  ("duo"  "dwo"   "duoo"  "duoh")
      ;; f
      "fa"   ("fa"   "far"   "faa"   "fah")
      "fan"  ("fan"  "farn"  "faan"  "fann")
      "fang" ("fang" "farng" "faang" "fanq")
      "fei"  ("fei"  "feir"  "feei"  "fey")
      "fen"  ("fen"  "fern"  "feen"  "fenn")
      "feng" ("feng" "ferng" "feeng" "fenq")
      "fo"   ("fo"   "for"   "foo"   "foh")
      "fou"  ("fou"  "four"  "foou"  "fow")
      "fu"   ("fu"   "fwu"   "fuu"   "fuh")
      ;; g
      "ga"    ("ga"    "gar"    "gaa"    "gah")
      "gai"   ("gai"   "gair"   "gae"    "gay")
      "gan"   ("gan"   "garn"   "gaan"   "gann")
      "gang"  ("gang"  "garng"  "gaang"  "ganq")
      "gao"   ("gau"   "gaur"   "gao"    "gaw")
      "ge"    ("ge"    "ger"    "gee"    "geh")
      "gei"   ("gei"   "geir"   "geei"   "gey")
      "gen"   ("gen"   "gern"   "geen"   "genn")
      "geng"  ("geng"  "gerng"  "geeng"  "genq")
      "gong"  ("gong"  "gorng"  "goong"  "gonq")
      "gou"   ("gou"   "gour"   "goou"   "gow")
      "gu"    ("gu"    "gwu"    "guu"    "guh")
      "gua"   ("gua"   "gwa"    "goa"    "guah")
      "guai"  ("guai"  "gwai"   "goai"   "guay")
      "guan"  ("guan"  "gwan"   "goan"   "guann")
      "guang" ("guang" "gwang"  "goang"  "guanq")
      "gui"   ("guei"  "gwei"   "goei"   "guey")
      "gun"   ("guen"  "gwen"   "goen"   "guenn")
      "guo"   ("guo"   "gwo"    "guoo"   "guoh")
      ;; h
      "ha"    ("ha"    "har"    "haa"    "hah")
      "hai"   ("hai"   "hair"   "hae"    "hay")
      "han"   ("han"   "harn"   "haan"   "hann")
      "hang"  ("hang"  "harng"  "haang"  "hanq")
      "hao"   ("hau"   "haur"   "hao"    "haw")
      "he"    ("he"    "her"    "hee"    "heh")
      "hei"   ("hei"   "heir"   "heei"   "hey")
      "hen"   ("hen"   "hern"   "heen"   "henn")
      "heng"  ("heng"  "herng"  "heeng"  "henq")
      "hong"  ("hong"  "horng"  "hoong"  "honq")
      "hou"   ("hou"   "hour"   "hoou"   "how")
      "hu"    ("hu"    "hwu"    "huu"    "huh")
      "hua"   ("hua"   "hwa"    "hoa"    "huah")
      "huai"  ("huai"  "hwai"   "hoai"   "huay")
      "huan"  ("huan"  "hwan"   "hoan"   "huann")
      "huang" ("huang" "hwang"  "hoang"  "huanq")
      "hui"   ("huei"  "hwei"   "hoei"   "huey")
      "hun"   ("huen"  "hwen"   "hoen"   "huenn")
      "huo"   ("huo"   "hwo"    "huoo"   "huoh")
      ;; j (pinyin j = ㄐ)
      "ji"    ("ji"    "jyi"    "jii"    "jih")
      "jia"   ("jia"   "jya"    "jea"    "jiah")
      "jian"  ("jian"  "jyan"   "jean"   "jiann")
      "jiang" ("jiang" "jyang"  "jeang"  "jianq")
      "jiao"  ("jiau"  "jyau"   "jeau"   "jiaw")
      "jie"   ("jie"   "jye"    "jiee"   "jieh")
      "jin"   ("jin"   "jyn"    "jiin"   "jinn")
      "jing"  ("jing"  "jyng"   "jiing"  "jinq")
      "jiong" ("jiong" "jyong"  "jeong"  "jionq")
      "jiu"   ("jiou"  "jyou"   "jeou"   "jiow")
      "ju"    ("jiu"   "jyu"    "jeu"    "jiuh")
      "juan"  ("jiuan" "jyuan"  "jeuan"  "jiuann")
      "jue"   ("jiue"  "jyue"   "jeue"   "jiueh")
      "jun"   ("jiun"  "jyun"   "jeun"   "jiunn")
      ;; k
      "ka"    ("ka"    "kar"    "kaa"    "kah")
      "kai"   ("kai"   "kair"   "kae"    "kay")
      "kan"   ("kan"   "karn"   "kaan"   "kann")
      "kang"  ("kang"  "karng"  "kaang"  "kanq")
      "kao"   ("kau"   "kaur"   "kao"    "kaw")
      "ke"    ("ke"    "ker"    "kee"    "keh")
      "ken"   ("ken"   "kern"   "keen"   "kenn")
      "keng"  ("keng"  "kerng"  "keeng"  "kenq")
      "kong"  ("kong"  "korng"  "koong"  "konq")
      "kou"   ("kou"   "kour"   "koou"   "kow")
      "ku"    ("ku"    "kwu"    "kuu"    "kuh")
      "kua"   ("kua"   "kwa"    "koa"    "kuah")
      "kuai"  ("kuai"  "kwai"   "koai"   "kuay")
      "kuan"  ("kuan"  "kwan"   "koan"   "kuann")
      "kuang" ("kuang" "kwang"  "koang"  "kuanq")
      "kui"   ("kuei"  "kwei"   "koei"   "kuey")
      "kun"   ("kuen"  "kwen"   "koen"   "kuenn")
      "kuo"   ("kuo"   "kwo"    "kuoo"   "kuoh")
      ;; l
      "la"    ("lha"   "la"     "laa"    "lah")
      "lai"   ("lhai"  "lai"    "lae"    "lay")
      "lan"   ("lhan"  "lan"    "laan"   "lann")
      "lang"  ("lhang" "lang"   "laang"  "lanq")
      "lao"   ("lhau"  "lau"    "lao"    "law")
      "le"    ("lhe"   "le"     "lee"    "leh")
      "lei"   ("lhei"  "lei"    "leei"   "ley")
      "leng"  ("lheng" "leng"   "leeng"  "lenq")
      "li"    ("lhi"   "li"     "lii"    "lih")
      "lia"   ("lhia"  "lia"    "lea"    "liah")
      "lian"  ("lhian" "lian"   "lean"   "liann")
      "liang" ("lhiang""liang"  "leang"  "lianq")
      "liao"  ("lhiau" "liau"   "leau"   "liaw")
      "lie"   ("lhie"  "lie"    "liee"   "lieh")
      "lin"   ("lhin"  "lin"    "liin"   "linn")
      "ling"  ("lhing" "ling"   "liing"  "linq")
      "liu"   ("lhiou" "liou"   "leou"   "liow")
      "lo"    ("lo"    "lor"    "loo"    "loh")
      "long"  ("lhong" "long"   "loong"  "lonq")
      "lou"   ("lhou"  "lou"    "loou"   "low")
      "lu"    ("lhu"   "lu"     "luu"    "luh")
      "luan"  ("lhuan" "luan"   "loan"   "luann")
      "lun"   ("lhuen" "luen"   "loen"   "luenn")
      "luo"   ("lhuo"  "luo"    "luoo"   "luoh")
      "lü"    ("lhiu"  "liu"    "leu"    "liuh")
      "lüe"   ("lhiue" "liue"   "leue"   "liueh")
      "lün"   ("lhiun" "liun"   "leun"   "liunn")
      ;; m
      "ma"   ("mha"   "ma"    "maa"   "mah")
      "mai"  ("mhai"  "mai"   "mae"   "may")
      "man"  ("mhan"  "man"   "maan"  "mann")
      "mang" ("mhang" "mang"  "maang" "manq")
      "mao"  ("mhau"  "mau"   "mao"   "maw")
      "me"   ("me"    "mer"   "mee"   "meh")
      "mei"  ("mhei"  "mei"   "meei"  "mey")
      "men"  ("mhen"  "men"   "meen"  "menn")
      "meng" ("mheng" "meng"  "meeng" "menq")
      "mi"   ("mhi"   "mi"    "mii"   "mih")
      "mian" ("mhian" "mian"  "mean"  "miann")
      "miao" ("mhiau" "miau"  "meau"  "miaw")
      "mie"  ("mhie"  "mie"   "miee"  "mieh")
      "min"  ("mhin"  "min"   "miin"  "minn")
      "ming" ("mhing" "ming"  "miing" "minq")
      "miu"  ("mhiou" "miou"  "meou"  "miow")
      "mo"   ("mho"   "mo"    "moo"   "moh")
      "mou"  ("mhou"  "mou"   "moou"  "mow")
      "mu"   ("mhu"   "mu"    "muu"   "muh")
      ;; n
      "na"    ("nha"   "na"    "naa"   "nah")
      "nai"   ("nhai"  "nai"   "nae"   "nay")
      "nan"   ("nhan"  "nan"   "naan"  "nann")
      "nang"  ("nhang" "nang"  "naang" "nanq")
      "nao"   ("nhau"  "nau"   "nao"   "naw")
      "ne"    ("nhe"   "ne"    "nee"   "neh")
      "nei"   ("nhei"  "nei"   "neei"  "ney")
      "nen"   ("nhen"  "nen"   "neen"  "nenn")
      "neng"  ("nheng" "neng"  "neeng" "nenq")
      "ni"    ("nhi"   "ni"    "nii"   "nih")
      "nia"   ("nia"   "niar"  "niaa"  "niah")
      "nian"  ("nhian" "nian"  "nean"  "niann")
      "niang" ("nhiang""niang" "neang" "nianq")
      "niao"  ("nhiau" "niau"  "neau"  "niaw")
      "nie"   ("nhie"  "nie"   "niee"  "nieh")
      "nin"   ("nhin"  "nin"   "niin"  "ninn")
      "ning"  ("nhing" "ning"  "niing" "ninq")
      "niu"   ("nhiou" "niou"  "neou"  "niow")
      "nong"  ("nhong" "nong"  "noong" "nonq")
      "nou"   ("nhou"  "nou"   "noou"  "now")
      "nu"    ("nhu"   "nu"    "nuu"   "nuh")
      "nuan"  ("nhuan" "nuan"  "noan"  "nuann")
      "nun"   ("nhuen" "nuen"  "noen"  "nuenn")
      "nuo"   ("nhuo"  "nuo"   "nuoo"  "nuoh")
      "nü"    ("nhiu"  "niu"   "neu"   "niuh")
      "nüe"   ("nhiue" "niue"  "neue"  "niueh")
      ;; p
      "pa"   ("pa"   "par"   "paa"   "pah")
      "pai"  ("pai"  "pair"  "pae"   "pay")
      "pan"  ("pan"  "parn"  "paan"  "pann")
      "pang" ("pang" "parng" "paang" "panq")
      "pao"  ("pau"  "paur"  "pao"   "paw")
      "pei"  ("pei"  "peir"  "peei"  "pey")
      "pen"  ("pen"  "pern"  "peen"  "penn")
      "peng" ("peng" "perng" "peeng" "penq")
      "pi"   ("pi"   "pyi"   "pii"   "pih")
      "pian" ("pian" "pyan"  "pean"  "piann")
      "piao" ("piau" "pyau"  "peau"  "piaw")
      "pie"  ("pie"  "pye"   "piee"  "pieh")
      "pin"  ("pin"  "pyn"   "piin"  "pinn")
      "ping" ("ping" "pyng"  "piing" "pinq")
      "po"   ("po"   "por"   "poo"   "poh")
      "pou"  ("pou"  "pour"  "poou"  "pow")
      "pu"   ("pu"   "pwu"   "puu"   "puh")
      ;; q (pinyin q = ㄑ)
      "qi"    ("chi"    "chyi"    "chii"    "chih")
      "qia"   ("chia"   "chya"    "chea"    "chiah")
      "qian"  ("chian"  "chyan"   "chean"   "chiann")
      "qiang" ("chiang" "chyang"  "cheang"  "chianq")
      "qiao"  ("chiau"  "chyau"   "cheau"   "chiaw")
      "qie"   ("chie"   "chye"    "chiee"   "chieh")
      "qin"   ("chin"   "chyn"    "chiin"   "chinn")
      "qing"  ("ching"  "chyng"   "chiing"  "chinq")
      "qiong" ("chiong" "chyong"  "cheong"  "chionq")
      "qiu"   ("chiou"  "chyou"   "cheou"   "chiow")
      "qu"    ("chiu"   "chyu"    "cheu"    "chiuh")
      "quan"  ("chiuan" "chyuan"  "cheuan"  "chiuann")
      "que"   ("chiue"  "chyue"   "cheue"   "chiueh")
      "qun"   ("chiun"  "chyun"   "cheun"   "chiunn")
      ;; r
      "ran"  ("rhan"  "ran"   "raan"  "rann")
      "rang" ("rhang" "rang"  "raang" "ranq")
      "rao"  ("rhau"  "rau"   "rao"   "raw")
      "re"   ("rhe"   "re"    "ree"   "reh")
      "ren"  ("rhen"  "ren"   "reen"  "renn")
      "reng" ("rheng" "reng"  "reeng" "renq")
      "ri"   ("rhy"   "ry"    "ryy"   "ryh")
      "rong" ("rhong" "rong"  "roong" "ronq")
      "rou"  ("rhou"  "rou"   "roou"  "row")
      "ru"   ("rhu"   "ru"    "ruu"   "ruh")
      "ruan" ("rhuan" "ruan"  "roan"  "ruann")
      "rui"  ("rhuei" "ruei"  "roei"  "ruey")
      "run"  ("rhuen" "ruen"  "roen"  "ruenn")
      "ruo"  ("rhuo"  "ruo"   "ruoo"  "ruoh")
      ;; s
      "sa"   ("sa"   "sar"   "saa"   "sah")
      "sai"  ("sai"  "sair"  "sae"   "say")
      "san"  ("san"  "sarn"  "saan"  "sann")
      "sang" ("sang" "sarng" "saang" "sanq")
      "sao"  ("sau"  "saur"  "sao"   "saw")
      "se"   ("se"   "ser"   "see"   "seh")
      "sei"  ("sei"  "seir"  "seei"  "sey")
      "sen"  ("sen"  "sern"  "seen"  "senn")
      "seng" ("seng" "serng" "seeng" "senq")
      "si"   ("sy"   "syr"   "syy"   "syh")
      "song" ("song" "sorng" "soong" "sonq")
      "sou"  ("sou"  "sour"  "soou"  "sow")
      "su"   ("su"   "swu"   "suu"   "suh")
      "suan" ("suan" "swan"  "soan"  "suann")
      "sui"  ("suei" "swei"  "soei"  "suey")
      "sun"  ("suen" "swen"  "soen"  "suenn")
      "suo"  ("suo"  "swo"   "suoo"  "suoh")
      ;; sh
      "sha"    ("sha"    "shar"    "shaa"    "shah")
      "shai"   ("shai"   "shair"   "shae"    "shay")
      "shan"   ("shan"   "sharn"   "shaan"   "shann")
      "shang"  ("shang"  "sharng"  "shaang"  "shanq")
      "shao"   ("shau"   "shaur"   "shao"    "shaw")
      "she"    ("she"    "sher"    "shee"    "sheh")
      "shei"   ("shei"   "sheir"   "sheei"   "shey")
      "shen"   ("shen"   "shern"   "sheen"   "shenn")
      "sheng"  ("sheng"  "sherng"  "sheeng"  "shenq")
      "shi"    ("shy"    "shyr"    "shyy"    "shyh")
      "shong"  ("shong"  "shorng"  "shoong"  "shonq")
      "shou"   ("shou"   "shour"   "shoou"   "show")
      "shu"    ("shu"    "shwu"    "shuu"    "shuh")
      "shua"   ("shua"   "shwa"    "shoa"    "shuah")
      "shuai"  ("shuai"  "shwai"   "shoai"   "shuay")
      "shuan"  ("shuan"  "shwan"   "shoan"   "shuann")
      "shuang" ("shuang" "shwang"  "shoang"  "shuanq")
      "shui"   ("shuei"  "shwei"   "shoei"   "shuey")
      "shun"   ("shuen"  "shwen"   "shoen"   "shuenn")
      "shuo"   ("shuo"   "shwo"    "shuoo"   "shuoh")
      ;; t
      "ta"   ("ta"   "tar"   "taa"   "tah")
      "tai"  ("tai"  "tair"  "tae"   "tay")
      "tan"  ("tan"  "tarn"  "taan"  "tann")
      "tang" ("tang" "tarng" "taang" "tanq")
      "tao"  ("tau"  "taur"  "tao"   "taw")
      "te"   ("te"   "ter"   "tee"   "teh")
      "teng" ("teng" "terng" "teeng" "tenq")
      "ti"   ("ti"   "tyi"   "tii"   "tih")
      "tian" ("tian" "tyan"  "tean"  "tiann")
      "tiao" ("tiau" "tyau"  "teau"  "tiaw")
      "tie"  ("tie"  "tye"   "tiee"  "tieh")
      "ting" ("ting" "tyng"  "tiing" "tinq")
      "tong" ("tong" "torng" "toong" "tonq")
      "tou"  ("tou"  "tour"  "toou"  "tow")
      "tu"   ("tu"   "twu"   "tuu"   "tuh")
      "tuan" ("tuan" "twan"  "toan"  "tuann")
      "tui"  ("tuei" "twei"  "toei"  "tuey")
      "tun"  ("tuen" "twen"  "toen"  "tuenn")
      "tuo"  ("tuo"  "two"   "tuoo"  "tuoh")
      ;; w
      "wa"   ("ua"   "wa"    "waa"   "wah")
      "wai"  ("uai"  "wai"   "woai"  "way")
      "wan"  ("uan"  "wan"   "woan"  "wann")
      "wang" ("uang" "wang"  "woang" "wanq")
      "wei"  ("uei"  "wei"   "woei"  "wey")
      "wen"  ("uen"  "wen"   "woen"  "wenn")
      "weng" ("ueng" "weng"  "woeng" "wenq")
      "wo"   ("uo"   "wo"    "woo"   "woh")
      "wu"   ("u"    "wu"    "wuu"   "wuh")
      ;; x (pinyin x = ㄒ)
      "xi"    ("shi"    "shyi"    "shii"    "shih")
      "xia"   ("shia"   "shya"    "shea"    "shiah")
      "xian"  ("shian"  "shyan"   "shean"   "shiann")
      "xiang" ("shiang" "shyang"  "sheang"  "shianq")
      "xiao"  ("shiau"  "shyau"   "sheau"   "shiaw")
      "xie"   ("shie"   "shye"    "shiee"   "shieh")
      "xin"   ("shin"   "shyn"    "shiin"   "shinn")
      "xing"  ("shing"  "shyng"   "shiing"  "shinq")
      "xiong" ("shiong" "shyong"  "sheong"  "shionq")
      "xiu"   ("shiou"  "shyou"   "sheou"   "shiow")
      "xu"    ("shiu"   "shyu"    "sheu"    "shiuh")
      "xuan"  ("shiuan" "shyuan"  "sheuan"  "shiuann")
      "xue"   ("shiue"  "shyue"   "sheue"   "shiueh")
      "xun"   ("shiun"  "shyun"   "sheun"   "shiunn")
      ;; y
      "ya"   ("ia"    "ya"    "yea"   "yah")
      "yan"  ("ian"   "yan"   "yean"  "yann")
      "yang" ("iang"  "yang"  "yeang" "yanq")
      "yao"  ("iau"   "yau"   "yeau"  "yaw")
      "ye"   ("ie"    "ye"    "yee"   "yeh")
      "yi"   ("i"     "yi"    "yii"   "yih")
      "yin"  ("in"    "yn"    "yiin"  "yinn")
      "ying" ("ing"   "yng"   "yiing" "yinq")
      "yong" ("iong"  "yong"  "yeong" "yonq")
      "you"  ("iou"   "you"   "yeou"  "yow")
      "yu"   ("iu"    "yu"    "yeu"   "yuh")
      "yuan" ("iuan"  "yuan"  "yeuan" "yuann")
      "yue"  ("iue"   "yue"   "yeue"  "yueh")
      "yun"  ("iun"   "yun"   "yeun"  "yunn")
      ;; z (pinyin z = ㄗ)
      "za"   ("tza"   "tzar"   "tzaa"   "tzah")
      "zai"  ("tzai"  "tzair"  "tzae"   "tzay")
      "zan"  ("tzan"  "tzarn"  "tzaan"  "tzann")
      "zang" ("tzang" "tzarng" "tzaang" "tzanq")
      "zao"  ("tzau"  "tzaur"  "tzao"   "tzaw")
      "ze"   ("tze"   "tzer"   "tzee"   "tzeh")
      "zei"  ("tzei"  "tzeir"  "tzeei"  "tzey")
      "zen"  ("tzen"  "tzern"  "tzeen"  "tzenn")
      "zeng" ("tzeng" "tzerng" "tzeeng" "tzenq")
      "zi"   ("tzy"   "tzyr"   "tzyy"   "tzyh")
      "zong" ("tzong" "tzorng" "tzoong" "tzonq")
      "zou"  ("tzou"  "tzour"  "tzoou"  "tzow")
      "zu"   ("tzu"   "tzwu"   "tzuu"   "tzuh")
      "zuan" ("tzuan" "tzwan"  "tzoan"  "tzuann")
      "zui"  ("tzuei" "tzwei"  "tzoei"  "tzuey")
      "zun"  ("tzuen" "tzwen"  "tzoen"  "tzuenn")
      "zuo"  ("tzuo"  "tzwo"   "tzuoo"  "tzuoh")
      ;; zh (pinyin zh = ㄓ)
      "zha"    ("ja"    "jar"    "jaa"    "jah")
      "zhai"   ("jai"   "jair"   "jae"    "jay")
      "zhan"   ("jan"   "jarn"   "jaan"   "jann")
      "zhang"  ("jang"  "jarng"  "jaang"  "janq")
      "zhao"   ("jau"   "jaur"   "jao"    "jaw")
      "zhe"    ("je"    "jer"    "jee"    "jeh")
      "zhei"   ("jei"   "jeir"   "jeei"   "jey")
      "zhen"   ("jen"   "jern"   "jeen"   "jenn")
      "zheng"  ("jeng"  "jerng"  "jeeng"  "jenq")
      "zhi"    ("jy"    "jyr"    "jyy"    "jyh")
      "zhong"  ("jong"  "jorng"  "joong"  "jonq")
      "zhou"   ("jou"   "jour"   "joou"   "jow")
      "zhu"    ("ju"    "jwu"    "juu"    "juh")
      "zhua"   ("jua"   "jwa"    "joa"    "juah")
      "zhuai"  ("juai"  "jwai"   "joai"   "juay")
      "zhuan"  ("juan"  "jwan"   "joan"   "juann")
      "zhuang" ("juang" "jwang"  "joang"  "juanq")
      "zhui"   ("juei"  "jwei"   "joei"   "juey")
      "zhun"   ("juen"  "jwen"   "joen"   "juenn")
      "zhuo"   ("juo"   "jwo"    "juoo"   "juoh")
     ))
  "Hash table mapping pinyin syllable bodies to GR spellings.
Each value is a list of (tone1 tone2 tone3 tone4).")

(defun gr (pinyin)
  "Convert CEDICT numeric pinyin (e.g. \"ni3 hao3\") to Gwoyeu Romatzyh.
Tone 5 (neutral) uses the tone-1 form with a leading dot per GR convention."
  (mapconcat
   (lambda (syllable)
     (if (not (string-match "^\\(.*\\)\\([1-5]\\)$" syllable))
         syllable
       (let* ((body (downcase (match-string 1 syllable)))
              (tone (string-to-number (match-string 2 syllable)))
              (body (replace-regexp-in-string "v" "ü" body))
              (forms (gethash body pinyin-to-gr-map)))
         (if (null forms)
             body                        ; fallback: return pinyin body as-is
           (if (= tone 5)
               (concat "·" (nth 0 forms))   ; neutral tone: dot + tone-1 form
             (nth (1- tone) forms))))))
   (split-string pinyin " " t)
   " "))

;;-------------------------------------------------------------------phonetic buffer

(defvar phonetic-log-buffer "*字典*")

(defun phonetic-ensure-window ()
  (unless (get-buffer-window phonetic-log-buffer)
    (let ((win (condition-case nil
                   (split-window-right (/ (* (frame-width) 2) 3))
                 (error (cl-reduce (lambda (a b)
                                     (if (< (window-width a) (window-width b)) a b))
                                   (window-list))))))
      (set-window-buffer win (get-buffer-create phonetic-log-buffer))
      (with-current-buffer (get-buffer-create phonetic-log-buffer)
	(org-mode)
	(org-indent-mode -1)
	(read-only-mode 1)
	(setq-local org-modern-star nil)
	(setq-local org-modern-hide-stars t)
	(org-modern-mode 1)
	(face-remap-add-relative 'default :height 1.1)
	(face-remap-add-relative 'org-level-1 :height 2.5))
      (with-selected-window win))))

(defvar phonetic-notes-file "~/.emacs.d/zidian-notes.org")

(defun phonetic-notes-load (word)
  "Return the notes body under the `* WORD` heading in `phonetic-notes-file', or nil."
  (let ((file (expand-file-name phonetic-notes-file)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (when (re-search-forward (format "^\\* %s[ \t]*$" (regexp-quote word)) nil t)
          (forward-line 1)
          (let ((start (point))
                (end (if (re-search-forward "^\\* " nil t)
                         (line-beginning-position)
                       (point-max))))
            (string-trim (buffer-substring-no-properties start end))))))))

(defvar phonetic-images-dir "~/Documents/thinkout/media/zidian-images/")

(defun phonetic-images-find (word)
  "Return image file paths in `phonetic-images-dir' for WORD, e.g.
WORD.jpg, WORD2.png, WORD3.jpeg, sorted in that numeric order."
  (let ((dir (expand-file-name phonetic-images-dir)))
    (when (file-directory-p dir)
      (let* ((pattern (format "\\`%s\\([0-9]*\\)\\.\\(jpg\\|jpeg\\|png\\|gif\\)\\'"
                               (regexp-quote word)))
             (files (directory-files dir t pattern)))
        (sort files
              (lambda (a b)
                (let* ((suffix-a (substring (file-name-base a) (length word)))
                       (suffix-b (substring (file-name-base b) (length word)))
                       (num-a (if (string-empty-p suffix-a) 0 (string-to-number suffix-a)))
                       (num-b (if (string-empty-p suffix-b) 0 (string-to-number suffix-b))))
                  (< num-a num-b))))))))

;;-----------------------------------------------------------------------------main zidian buffer

(defun phonetic-fold-deep-headings ()
  "Fold (collapse) all org headings at level 3 or deeper in the current buffer.
Level 1 and 2 headings are left expanded."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\*\\{3,\\} " nil t)
      (beginning-of-line)
      (outline-hide-subtree)
      (end-of-line))))

(defun phonetic-lookup ()
  "Show all romanisations for the Chinese word at point, plus Japanese if available."
  (interactive)
  (unless my-cedict-table (my-cedict-build-table))
  (unless my-canto-table (my-canto-build-table))
  (let* ((word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
		 (string (char-after (point)))))
         (entries (gethash word my-cedict-table))
         (japanese (my-kanji-lookup-word word)))
    (phonetic-ensure-window)
    (with-current-buffer (get-buffer-create phonetic-log-buffer)
      (let ((inhibit-read-only t))
	(erase-buffer)
	;;(when (= (buffer-size) 0) (org-mode))
	;; Heading
	(let* ((trads (and (= (length word) 1)
			   (gethash word my-simp-to-trad-cache)))
               (heading (if (and trads (not (equal trads (list word))))
			    (format "* _%s_ %s\n" word (mapconcat #'identity trads " "))
			  (format "* _%s_\n" word))))
	  (insert heading)
	  ;;(setq my-han-scale-amount 2)
	  (let ((scale (expt text-scale-mode-step 2)))
	    (when my-han-scale-cookie
	      (face-remap-remove-relative my-han-scale-cookie))
	    (setq my-han-scale-cookie
		  (face-remap-add-relative 'han-characters :height scale))))
	;; Mandarin section
	;;(insert "~Chinese~\n")
	(if (not entries)
	    (insert "  /no results/\n")
	  (let ((seen (make-hash-table :test 'equal)) ; (trad raw . defs) -> t, for exact-duplicate lines only
		(order nil))
	    ;; Every distinct CEDICT line becomes its own displayed entry,
	    ;; except "variant of ..." cross-references, which aren't real
	    ;; definitions and are skipped entirely. We also skip a line if
	    ;; it's a literal duplicate (same traditional form, reading,
	    ;; AND definition text) of one already seen.
	    (dolist (item entries)
	      (let ((trad  (car item))
		    (entry (cdr item)))
		(when (string-match "\\[\\([^]]+\\)\\] /\\(.*\\)" entry)
		  (let* ((raw  (match-string 1 entry))
			 (defs (match-string 2 entry))
			 (key  (list trad raw defs))
			 (is-variant (string-match-p "\\`[a-zA-Z]* ?variant of " defs)))
		    (unless (or is-variant (gethash key seen))
		      (puthash key t seen)
		      (push (list trad raw defs) order))))))
	    ;; `order' was built with `push', so it's naturally in the
	    ;; reverse of entries' (file) order already - which is what
	    ;; we want displayed, so no `nreverse' here.
	    (let ((index 0))
	      (dolist (item order)
		(let* ((trad (nth 0 item))
		       (raw  (nth 1 item))
		       (defs (nth 2 item))
		       ;; (meaning (car (split-string defs "/" t)))
		       (meaning (mapconcat #'identity (split-string defs "/" t) "; "))
		       ;; Flag which traditional variant this sense comes
		       ;; from whenever it differs from the word looked up,
		       ;; so e.g. senses from 矇 are visibly distinct from
		       ;; those belonging to 蒙 itself.
		       (meaning (if (string= trad word)
				    meaning
				  (format "(%s) %s" trad meaning)))
		       (py  (my-pinyin-convert raw))
		       (ya  (yale raw))
		       (zh  (my-pinyin-to-zhuyin raw))
		       (gr  (gr raw))
		       (canto-pairs (or (gethash word my-canto-table)
					(cl-mapcan (lambda (trad)
						     (copy-sequence (gethash trad my-canto-table)))
						   (or (gethash word my-simp-to-trad-cache)
                                                       (list word)))))
		       (jyut (cdr (assoc (downcase raw) canto-pairs))))
		  (cl-incf index)
		  (insert (format "** %d: %s\n" index meaning))
		  (insert (format "pronounce as _%s_\n***  ⬥\n+ /yale/              %s\n+ /zhuyin/        %s\n+ /romatzyh/    %s\n%s"
				  py ya zh gr
				  (if jyut
				      (format "+ /cantonese/    %s\n\n" jyut)
				    "\n"))))))))
	
	;; Notes section
	(let ((notes (phonetic-notes-load word)))
	  (when (and notes (not (string-empty-p notes)))
	    (insert "** ~notes~\n")
	    (insert notes "\n\n")))

	;; Images section
	(let ((images (phonetic-images-find word)))
	  (when images
	    (insert "** ~written forms~\n")
	    (insert (mapconcat (lambda (img) (format "[[file:%s]]" img)) images " "))
	    (insert "\n\n")))
	
	;; Japanese section
	(when japanese
	  (let* ((c       (char-to-string (car (string-to-list word))))
		 (trads   (or (gethash c my-simp-to-trad-cache) (list c)))
		 (jp-vars (delq nil
				(mapcar (lambda (trad)
					  (gethash trad my-jp-variant-reverse-table))
					trads))))
	    (when jp-vars
	      (insert (format "\n%s\n" (mapconcat #'identity jp-vars " ")))))
	  ;; (insert "** ~Japanese~\n")
	  (dolist (char (string-to-list word))
	    (let* ((c        (char-to-string char))
		   (trads    (or (gethash c my-simp-to-trad-cache) (list c)))
		   (jp-trad  (gethash c my-jp-variant-table))
		   (candidates (if (and jp-trad (not (member jp-trad trads)))
				   (append trads (list jp-trad))
				 trads))
		   (entries
		    (delq nil
			  (mapcar (lambda (cand)
				    (or (gethash cand my-kanji-table)
					(let ((jp (gethash cand my-jp-variant-table)))
					  (and jp (gethash jp my-kanji-table)))))
				  candidates))))
	      (dolist (entry entries)
		(let* ((on      (plist-get entry :on))
		       (on-hep  (plist-get entry :on-hep))
		       (meaning (plist-get entry :meaning))
		       (m-str   (replace-regexp-in-string "|" ", " meaning)))
		  (insert (format "** ~Japanese:~ %s\n" m-str))
		  (unless (string-empty-p on)
		    (insert (format "%s  %s\n" on on-hep)))))))
	  (insert "\n")
	  ;; Korean section
	  (let ((korean (my-korean-hanja-lookup word)))
	    (when korean
	      ;; (insert "** ~Korean~\n")
	      (dolist (entry korean)
		(let* ((meaning (car entry))
		       (hangul  (cdr entry))
		       (roman   (my-hangul-to-roman hangul)))
		  (insert (format "** ~Korean:~ %s\n" meaning))
		  (insert (format "%s  %s\n\n" hangul roman)))))))
      ;; Fold anything deeper than level 2 (currently just the notes
      ;; heading) so the buffer opens showing only meanings/readings
      ;; and the Japanese/Korean headings.
      (phonetic-fold-deep-headings)
      )
      (when-let (win (get-buffer-window phonetic-log-buffer))
	(set-window-point win (point-min)))
      (org-display-inline-images))))

(defun phonetic-lookup-prompt ()
  "Prompt in the minibuffer for a character/word and run `phonetic-lookup' on it.
Does this by staging the typed text into a temporary buffer with it
selected as the region, then calling `phonetic-lookup' unmodified."
  (interactive)
  (let ((word (read-string "Lookup: ")))
    (with-temp-buffer
      (insert word)
      (push-mark (point-min) t t)
      (goto-char (point-max))
      (phonetic-lookup))))

;;-------------------------------------------------------------------------add cedict entry

(defun my-cedict-add-entry (traditional simplified pinyin definitions)
  "Add a new entry to the CEDICT file and rebuild the lookup table.
PINYIN should be in CEDICT numeric format, e.g. \"ni3 hao3\".
DEFINITIONS should be a list of strings."
  (interactive
   (list
    (read-string "traditional: ")
    (read-string "simplified (RET to copy traditional): ")
    (read-string "pinyin (numeric, e.g. ni3 hao3): ")
    (let ((defs '()) (def nil))
      (while (not (string-empty-p
                   (setq def (read-string "definition (RET on blank to close): "))))
        (push def defs))
      (nreverse defs))))
  (let* ((simp (if (string-empty-p simplified) traditional simplified))
         (def-string (mapconcat #'identity definitions "/"))
         (file (expand-file-name my-cedict-file))
         (line (format "%s %s [%s] /%s/\n"
                       traditional simp pinyin def-string)))
    ;; ensure file ends with a newline before appending
    (when (and (file-exists-p file) (> (file-attribute-size (file-attributes file)) 0))
      (with-temp-buffer
        (insert-file-contents file nil
                              (1- (file-attribute-size (file-attributes file)))
                              (file-attribute-size (file-attributes file)))
        (unless (= (char-before (point-max)) ?\n)
          (write-region "\n" nil file t 'silent))))
    ;; append the new entry
    (with-temp-buffer
      (insert line)
      (write-region (point-min) (point-max) file t 'silent))
    (when my-cedict-table
      (let ((entry (format "[%s] /%s/" pinyin def-string)))
        (push (cons traditional entry) (gethash traditional my-cedict-table))
        (unless (string= traditional simp)
          (push (cons traditional entry) (gethash simp my-cedict-table)))))
    (when my-cedict-pinyin-table
      (let ((key (downcase pinyin)))
        (push (cons traditional simp)
              (gethash key my-cedict-pinyin-table))))
    (message "Added: %s" line)))

;;-------------------------------------------------------------------kanji/japanese lookup

(defvar my-kanji-table nil
  "Hash table mapping kanji characters to their entry plists.")

(defun my-kanji-build-table ()
  (setq my-kanji-table (make-hash-table :test 'equal :size 14000))
  (with-temp-buffer
    (insert-file-contents (expand-file-name my-kanji-file))
    (goto-char (point-min))
    (forward-line 1) ; skip header
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position) (line-end-position)))
             (fields (split-string line "\t" nil)))
        (when (>= (length fields) 6)
          (let ((char    (nth 0 fields))
                (on      (nth 1 fields))
                (on-hep  (nth 2 fields))
                (kun     (nth 3 fields))
                (kun-hep (nth 4 fields))
                (meaning (nth 5 fields)))
            (puthash char
                     (list :on on :on-hep on-hep
                           :kun kun :kun-hep kun-hep
                           :meaning meaning)
                     my-kanji-table))))
      (forward-line 1))))

(defvar my-simp-to-trad-cache nil
  "Hash table mapping simplified single characters to traditional forms.
Built lazily from the CEDICT file on first kanji lookup.")

(defun my-simp-to-trad-ensure ()
  (unless my-simp-to-trad-cache
    (setq my-simp-to-trad-cache (make-hash-table :test 'equal :size 20000))
    (with-temp-buffer
      (insert-file-contents (expand-file-name my-cedict-file))
      (goto-char (point-min))
      (while (re-search-forward "^\\([^ ]+\\) \\([^ ]+\\) \\[" nil t)
        (let ((trad (match-string 1))
              (simp (match-string 2)))
          (when (and (= (length simp) 1)
                     (not (string= trad simp))
                     (not (member trad (gethash simp my-simp-to-trad-cache))))
            (push trad (gethash simp my-simp-to-trad-cache))))))))

(defun my-kanji-lookup-word (word)
  "Return a formatted string of Japanese info for each kanji in WORD, or nil."
  (unless my-kanji-table (my-kanji-build-table))
  (my-simp-to-trad-ensure)
  (my-jp-variant-ensure)
  (let ((lines '()))
    (dolist (char (string-to-list word))
      (let* ((c        (char-to-string char))
             (trads    (or (gethash c my-simp-to-trad-cache) (list c)))
             (jp-trad  (gethash c my-jp-variant-table))
             (entry    (or (cl-some (lambda (t) (gethash t my-kanji-table)) trads)
                           (and jp-trad (gethash jp-trad my-kanji-table))
                           (gethash c my-kanji-table))))
        (when entry
          (let* ((on      (plist-get entry :on))
                 (on-hep  (plist-get entry :on-hep))
                 (kun     (plist-get entry :kun))
                 (kun-hep (plist-get entry :kun-hep))
                 (meaning (plist-get entry :meaning))
                 (on-str  (if (string-empty-p on) ""
                            (format "  on:  %s  [%s]" on on-hep)))
                 (kun-str (if (string-empty-p kun) ""
                            (format "  kun: %s  [%s]" kun kun-hep)))
                 (m-str   (if (string-empty-p meaning) ""
                            (format "  %s" (replace-regexp-in-string "|" ", " meaning)))))
            (push (concat c "\n" on-str
                          (if (and (not (string-empty-p on-str))
                                   (not (string-empty-p kun-str))) "\n" "")
                          kun-str "\n" m-str)
                  lines)))))
    (when lines
      (mapconcat #'identity (nreverse lines) "\n"))))

(defvar my-jp-variant-table nil
  "Hash table mapping Japanese shinjitai to kyujitai.")

(defun my-jp-variant-ensure ()
  (unless my-jp-variant-table
    (setq my-jp-variant-table (make-hash-table :test 'equal :size 2000))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "~/.emacs.d/kanji-japanese-simplifications.txt"))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (fields (split-string line " " t)))
          (when (= (length fields) 2)
            (puthash (nth 0 fields) (nth 1 fields) my-jp-variant-table)))
        (forward-line 1)))))

(defvar my-jp-variant-reverse-table nil
  "Hash table mapping Japanese kyujitai to shinjitai.")

(defun my-jp-variant-ensure ()
  (unless my-jp-variant-table
    (setq my-jp-variant-table (make-hash-table :test 'equal :size 2000))
    (setq my-jp-variant-reverse-table (make-hash-table :test 'equal :size 2000))
    (with-temp-buffer
      (insert-file-contents (expand-file-name "~/.emacs.d/kanji-japanese-simplifications.txt"))
      (goto-char (point-min))
      (while (not (eobp))
        (let* ((line (buffer-substring-no-properties
                      (line-beginning-position) (line-end-position)))
               (fields (split-string line " " t)))
          (when (= (length fields) 2)
            (puthash (nth 0 fields) (nth 1 fields) my-jp-variant-table)
            (puthash (nth 1 fields) (nth 0 fields) my-jp-variant-reverse-table)))
        (forward-line 1)))))

;;-----------------------------------------------------------------------------------Cantonese

(defvar my-canto-file "~/.emacs.d/cccanto.txt")
(defvar my-canto-table nil)

(defun my-canto-build-table ()
  (setq my-canto-table (make-hash-table :test 'equal :size 120000))
  (with-temp-buffer
    (insert-file-contents (expand-file-name my-canto-file))
    (goto-char (point-min))
    (while (re-search-forward
            "^\\([^ ]+\\) \\([^ ]+\\) \\[\\([^]]+\\)\\] {\\([^}]+\\)}" nil t)
      (let ((trad (match-string 1))
            (simp (match-string 2))
            (pinyin (downcase (match-string 3)))
            (jyut (match-string 4)))
        (cl-pushnew (cons pinyin jyut)
                    (gethash trad my-canto-table) :test #'equal)
        (unless (string= trad simp)
          (cl-pushnew (cons pinyin jyut)
                      (gethash simp my-canto-table) :test #'equal))))))

;;------------------------------------------------------------------Revised Romanisation of Korean
 
(defvar my-kor-initial
  ;; index 0-18, the 19 choseong
  ["g" "kk" "n" "d" "tt" "r" "m" "b" "pp" "s" "ss" "" "j" "jj" "ch" "k" "t" "p" "h"]
  "Revised Romanisation initials (choseong), indexed 0–18.")
 
(defvar my-kor-vowel
  ;; index 0-20, the 21 jungseong
  ["a" "ae" "ya" "yae" "eo" "e" "yeo" "ye" "o" "wa" "wae" "oe"
   "yo" "u" "wo" "we" "wi" "yu" "eu" "ui" "i"]
  "Revised Romanisation vowels (jungseong), indexed 0–20.")
 
(defvar my-kor-final
  ;; index 0-27, the 28 jongseong slots (0 = none)
  ["" "g" "kk" "gs" "n" "nj" "nh" "d" "l" "lg" "lm" "lb" "ls"
   "lt" "lp" "lh" "m" "b" "bs" "s" "ss" "ng" "j" "ch" "k" "t" "p" "h"]
  "Revised Romanisation finals (jongseong), indexed 0–27.")
 
(defun my-hangul-syllable-to-roman (char)
  "Convert a single hangul syllable character to Revised Romanisation."
  (let ((code (- char #xAC00)))
    (if (< code 0)
        (char-to-string char)
      (let* ((ini  (/ code (* 21 28)))
             (vow  (/ (% code (* 21 28)) 28))
             (fin  (% code 28)))
        (concat (aref my-kor-initial ini)
                (aref my-kor-vowel   vow)
                (aref my-kor-final   fin))))))
 
(defun my-hangul-to-roman (hangul)
  "Convert a hangul string to Revised Romanisation syllable-by-syllable.
No assimilation rules are applied; each syllable is converted independently,
which is accurate for dictionary citation forms of single characters."
  (mapconcat #'my-hangul-syllable-to-roman
             (string-to-list hangul)
             ""))
 
;;------------------------------------------------------------------Korean hanja table
 
(defvar my-korean-hanja-table nil)
 
(defun my-korean-hanja-build-table ()
  (setq my-korean-hanja-table (make-hash-table :test 'equal :size 3000))
  (with-temp-buffer
    (insert-file-contents (expand-file-name my-korean-hanja-file))
    (goto-char (point-min))
    (forward-line 1)
    (while (not (eobp))
      (let* ((line (buffer-substring-no-properties
                    (line-beginning-position)
                    (line-end-position)))
             (fields (split-string line "\t" t)))  ; t = omit nulls
        (when (>= (length fields) 2)               ; >= instead of =
          (let* ((meaning-field (string-trim (nth 0 fields)))
                 (hanja         (string-trim (nth 1 fields)))
                 (tokens  (split-string meaning-field "[ \t/]+" t))
                 (reading (car (last tokens)))
                 (meaning (string-join (butlast tokens) " ")))
            (when (and reading (not (string-empty-p hanja)))
              (dolist (char-str (mapcar #'char-to-string (string-to-list hanja)))
                (push (cons meaning reading)
                      (gethash char-str my-korean-hanja-table)))))))
      (forward-line 1))))

(defun my-korean-hanja-lookup (word)
  "Return a list of (meaning . hangul-reading) pairs for each character in WORD.
Returns nil if no entries are found for any character."
  (unless my-korean-hanja-table
    (my-korean-hanja-build-table))
  (let ((results '()))
    (dolist (char (string-to-list word))
      (let* ((c       (char-to-string char))
             (entries (gethash c my-korean-hanja-table)))
        (when entries
          (dolist (entry (nreverse (copy-sequence entries)))
            (push entry results)))))
    (when results
      (nreverse results))))

(provide 'tongzhuo)
;;; tongzhuo.el ends here
