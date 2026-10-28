Emacs extension packages I use for teaching and lecture delivery.

# Tongzhuo classroom tool

<img width="1920" height="1048" alt="Screenshot From 2026-10-28 23-08-25" src="https://github.com/user-attachments/assets/1027c0a2-3f3d-4b59-b2a5-84305378389e" />

- imports CC-CEDICT (available at https://www.mdbg.net/chinese/dictionary?page=cedict) as a hash table on the first time the interactive command for quering the dictionary is called (this can be set to be done on emacs initialisation, this is currently commented out)
- calls on jieba.py (running as a background process) for Chinese word segmentation on Chinese text in a buffer
  - text can also be manually selected for search
- Chinese headword, pinyin and definition pulled and printed into a buffer called *chinese-vocab-check* that splits on a 2:1 ratio and fills the smaller window
  - numbered pinyin is converted to tone-mark pinyin automatically 
- the output buffer is set to org-mode and can be exported to pdf for custom vocabulary lists that covers all spontaneous word lookups covered in a class
- the interactive command for lookups is 'my-chinese-lookup
  - I use xah-fly-keys and have this set as  (define-key xah-fly-command-map (kbd "d") 'my-chinese-lookup) in my init.el

# Instant Cangjie code check
- imports Cangjie 5 code database as a hash table
- minibuffer requests a numbered pinyin string to search the database, which then searches CC-CEDICT for the corresponding character(s)
- these character(s) are then matched in the CJ5 database and the corresponding Cangjie codes are printed into a buffer called *cangjie-check* that splits on a 2:1 ratio and fills the smaller window
- the search query is converted into zhuyin fuhao (bopomofo) and printed alongside the headword in queries that are 2 or more characters. For single character queries there is no headword, only the zhuyin.
- this buffer is wiped before printing a new search result, but every search result is printed to an org file that has zhuyin formatted as headings. This is because cangjie checks are done by me for characters I forget how to decompose on the spot, and this output file serves as a revision tool.

# Other integrated features
- Japanese onyomi and Korean character pronunciation databases
- Cursive character database (top 1000 frequency list, with manual addition capability)
- vocabulary builder for in-class vocab mining and testing

<img width="1920" height="1048" alt="Screenshot From 2026-10-28 23-08-39" src="https://github.com/user-attachments/assets/8c448516-1bd4-4d48-94a9-95f332cf7ebd" />

