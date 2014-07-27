;; Copyright (c) 2014 juszczakn

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(defconst rest-url-string-decode-strings
  '(("%20" " "))
  "list of encodings and their decoded values")

(defun rest-url-string-split-params (params)
  (split-string params "&"))

(defun rest-url-string-params-decode (params)
  "take list of encoded params, return list decoded "
  (setf newparams '())
  (dolist (param params newparams)
    (dolist (decode rest-url-string-decode-strings param)
      ; decode every param
      (setf param (replace-regexp-in-string (regexp-quote (car decode)) (second decode) param)))
    (setf newparams (cons param newparams))))

(defun rest-url-string-extract ()
  "extract url and params"
  (let ((s (buffer-substring (point-at-bol) (point-at-eol))))
    (setf spl (split-string s "?"))
    (setf url (car spl)
          params (cdr spl))
    (when (not (eql nil params))
      (setf params (rest-url-string-split-params (car params))))
    (list url params)))

(defun rest-url-string-extract-decode ()
  "extract url and params, decode params"
  (setf url-list (rest-url-string-extract))
  (setf decoded-params (rest-url-string-params-decode (second url-list)))
  (list (car url-list) decoded-params))

(defun rest-url-string-extract-print ()
  "Extract URL and parameters, print them below the URL in the buffer"
  (interactive)
  (move-end-of-line nil)
  (setf elts (rest-url-string-extract))
  (newline)
  (insert (car elts))
  (dolist (elt (car (cdr elts)))
    (newline)
    (insert elt))
  (newline))

(defun rest-url-string-extract-decode-print ()
  "Extract URL and decode the parameters, print them below the URL in the buffer"
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (setf elts (rest-url-string-extract-decode))
    (newline)
    (insert (car elts))
    (dolist (elt (second elts))
      (newline)
      (insert elt))
    (print return-point)))

(defun rest-url-string-reencode-region (s)
  "re-encode and create URL that has been extracted"
  (setq s (replace-regexp-in-string (regexp-quote "\n") "&" s))
  (setq s (replace-regexp-in-string "\\(&\\).*\\'" "?" s nil nil 1))
  (replace-regexp-in-string "\\(&\\|\\?\\)$" "" s))

(defun rest-url-string-reencode-region-print (begin end)
  "reconstruct/reencode a given region that has been split by extract-print"
  (interactive "r")
  (save-excursion
    (setq str (rest-url-string-reencode-region (buffer-substring-no-properties begin end)))
    (dolist (encode rest-url-string-decode-strings)
      (setf str (replace-regexp-in-string (regexp-quote (second encode)) (car encode) str)))
    (move-beginning-of-line nil) (newline) (previous-line)
    (insert str)))

(provide 'rest-url-string)

;; Test URL
;http://www.acme.com/phonebook/UserDetails?firstName=John&lastName=Doe%20Mark
