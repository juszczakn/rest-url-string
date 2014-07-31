;;; rest-url-string.el --- For easy evaluation and printing of URL's using REST

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

;; 

;;; Code:

; List of all possible encodings. Might need later
;; '(("%09" "\t") ("%0A" "linefeed") ("%0D" "creturn") ("%20" " ")
;;   ("%22" "\"") ("%23" "#") ("%24" "$") ("%25" "%") ("%26" "&")
;;   ("%27" "'") ("%28" "(") ("%29" ")") ("%2A" "*") ("%2B" "+")
;;   ("%2C" ",") ("%2D" "-") ("%2E" ".") ("%2F" "/") ("%30" "0")
;;   ("%31" "1") ("%32" "2") ("%33" "3") ("%34" "4") ("%35" "5")
;;   ("%36" "6") ("%37" "7") ("%38" "8") ("%39" "9") ("%3A" ":")
;;   ("%3B" ";") ("%3C" "<") ("%3D" "=") ("%3E" ">") ("%3F" "?")
;;   ("%40" "@") ("%41" "A") ("%42" "B") ("%43" "C") ("%44" "D")
;;   ("%45" "E") ("%46" "F") ("%47" "G") ("%48" "H") ("%49" "I")
;;   ("%4A" "J") ("%4B" "K") ("%4C" "L") ("%4D" "M") ("%4E" "N")
;;   ("%4F" "O") ("%50" "P") ("%51" "Q") ("%52" "R") ("%53" "S")
;;   ("%54" "T") ("%55" "U") ("%56" "V") ("%57" "W") ("%58" "X")
;;   ("%59" "Y") ("%5A" "Z") ("%5B" "[") ("%5C" "\\") ("%5D" "]")
;;   ("%5E" "^") ("%5F" "_") ("%60" "`") ("%61" "a") ("%62" "b")
;;   ("%63" "c") ("%64" "d") ("%65" "e") ("%66" "f") ("%67" "g")
;;   ("%68" "h") ("%69" "i") ("%6A" "j") ("%6B" "k") ("%6C" "l")
;;   ("%6D" "m") ("%6E" "n") ("%6F" "o") ("%70" "p") ("%71" "q")
;;   ("%72" "r") ("%73" "s") ("%74" "t") ("%75" "u") ("%76" "v")
;;   ("%77" "w") ("%78" "x") ("%79" "y") ("%7A" "z") ("%7B" "{")
;;   ("%7C" "|") ("%7D" "}") ("%7E" "~") ("%A2" "¢") ("%A3" "£")
;;   ("%A5" "¥") ("%A6" "|") ("%A7" "§") ("%AB" "«") ("%AC" "¬")
;;   ("%AD" "¯") ("%B0" "º") ("%B1" "±") ("%B2" "ª") ("%B4" ",")
;;   ("%B5" "µ") ("%BB" "»") ("%BC" "¼") ("%BD" "½") ("%BF" "¿")
;;   ("%C0" "À") ("%C1" "Á") ("%C2" "Â") ("%C3" "Ã") ("%C4" "Ä")
;;   ("%C5" "Å") ("%C6" "Æ") ("%C7" "Ç") ("%C8" "È") ("%C9" "É")
;;   ("%CA" "Ê") ("%CB" "Ë") ("%CC" "Ì") ("%CD" "Í") ("%CE" "Î")
;;   ("%CF" "Ï") ("%D0" "Ð") ("%D1" "Ñ") ("%D2" "Ò") ("%D3" "Ó")
;;   ("%D4" "Ô") ("%D5" "Õ") ("%D6" "Ö") ("%D8" "Ø") ("%D9" "Ù")
;;   ("%DA" "Ú") ("%DB" "Û") ("%DC" "Ü") ("%DD" "Ý") ("%DE" "Þ")
;;   ("%DF" "ß") ("%E0" "à") ("%E1" "á") ("%E2" "â") ("%E3" "ã")
;;   ("%E4" "ä") ("%E5" "å") ("%E6" "æ") ("%E7" "ç") ("%E8" "è")
;;   ("%E9" "é") ("%EA" "ê") ("%EB" "ë") ("%EC" "ì") ("%ED" "í")
;;   ("%EE" "î") ("%EF" "ï") ("%F0" "ð") ("%F1" "ñ") ("%F2" "ò")
;;   ("%F3" "ó") ("%F4" "ô") ("%F5" "õ") ("%F6" "ö") ("%F7" "÷")
;;   ("%F8" "ø") ("%F9" "ù") ("%FA" "ú") ("%FB" "û") ("%FC" "ü")
;;   ("%FD" "ý") ("%FE" "þ") ("%FF" "ÿ") ("%21" "!"))

(eval-when-compile (require 'cl))

(defconst rest-url-string-decode-strings
  '(("%20" " ") ("%3A" ":") ("%5B" "[") ("%5C" "\\") ("%5D" "]")
    ("%3B" ";") ("%3C" "<") ("%3E" ">") ("%3F" "?")
    ("%2C" ",") ("%2A" "*")  ("+" " ") ("%2B" "+")
    ("%7C" "|") ("%7D" "}") ("%7E" "~") ("%A2" "¢") ("%A3" "£")
    ("%7B" "{") ("%22" "\"") ("%28" "(") ("%29" ")"))
  "list of encodings and their decoded values")

(defun rest-url-string-trim-string (s)
  (replace-regexp-in-string "\\`[ \t\n]*" ""
                            (replace-regexp-in-string "[ \t\n]*\\'" "" s)))

(defun rest-url-string-split-params (params)
  (split-string params "&"))

(defun rest-url-string-params-translate (params decode-p)
  "take list of params, return list encoded or decoded"
  (setf newparams '())
  ; from <value> to <new value>
  (setf from (if decode-p #'car #'second))
  (setf to (if decode-p #'second #'car))
  (dolist (param params)
    (setf param (rest-url-string-trim-string param))
    (when (not (eq "" param))
      (dolist (code rest-url-string-decode-strings param)
        ; decode every param
        (setf param (replace-regexp-in-string
                     (regexp-quote (funcall from code))
                     (funcall to code)
                     param)))
      (setf newparams (cons param newparams))))
  (nreverse newparams))

(defun rest-url-string-extract ()
  "extract url and params"
  (let* ((s (buffer-substring (point-at-bol) (point-at-eol)))
         (s (rest-url-string-trim-string s)))
    (setf spl (split-string s "?"))
    (setf url (car spl)
          params (cdr spl))
    (when (not (eql nil params))
      (setf params (rest-url-string-split-params (car params))))
    (list url params)))

(defun rest-url-string-extract-decode ()
  "extract url and params, decode params"
  (setf url-list (rest-url-string-extract))
  (setf decoded-params (rest-url-string-params-translate (second url-list) t))
  (list (car url-list) decoded-params))


(defun rest-url-string-reencode (s)
  "re-encode and create URL that has been extracted"
  ; if not all on one line, recombine and parse
  (let ((s (rest-url-string-trim-string s)))
    (if (string-match "\n" s)
        (progn
          (setf elts (split-string s "\n"))
          (setf params (rest-url-string-params-translate (cdr elts) nil))
          (setf newparams "")
          (dolist (param params)
            (setf newparams (concatenate 'string newparams "&" param)))
          (concatenate 'string (car elts) "?" (substring newparams 1)))
      s)))

;;;###autoload
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

;;;###autoload
(defun rest-url-string-extract-decode-print ()
  "Extract URL and decode the parameters, print them below the URL in the buffer"
  (interactive)
  (save-excursion
    (move-end-of-line nil)
    (setf elts (rest-url-string-extract-decode))
    (newline) (newline)
    (insert (car elts))
    (dolist (elt (second elts))
      (newline)
      (insert elt))
    (newline)))

;;;###autoload
(defun rest-url-string-reencode-print (begin end)
  "reconstruct/reencode a given region that has been split by extract-print"
  (interactive "r")
  (save-excursion
    (kill-region begin end) (yank)
    (setf str (rest-url-string-reencode (buffer-substring-no-properties begin end)))
    (move-beginning-of-line nil) (newline) (newline) (previous-line)
    (insert str) (newline)))

;;;###autoload
(defun rest-url-string-http-get-print (begin end)
  "make http get call. works on single line and regions"
  (interactive "r")
  ; Check for one-liners
  (unless (bolp)
    (save-excursion
      (move-beginning-of-line nil)
      (let ((s (buffer-substring (point) end))
            (reg "http://"))
        (when (string-match reg s)
          (setq begin (point))))))
  (setf url (rest-url-string-reencode
             (rest-url-string-trim-string
              (buffer-substring-no-properties begin end))))
  (setf response "")
  (with-current-buffer (url-retrieve-synchronously url)
    (progn
      (setf response (buffer-string))
      (kill-buffer)))
  (save-excursion
    (kill-region begin end) (yank)
    (move-end-of-line nil) (newline 2)
    (insert response)))

; Test URL
;http://www.acme.com/phonebook/UserDetails?firstName=John&lastName=Doe%20Mark

(provide 'rest-url-string)
;;; rest-url-string.el ends here
