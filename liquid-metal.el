;;; liquid-metal.el --- A mimetic poly-alloy of Quicksilver's scoring algorithm
;;
;; Copyright (c) 2011, Tim Felgentreff (tim -[at]- nada1 [*dot*] de)
;;
;; Based on the ECMAscript implementation by Ryan McGeary (ryanonjavascript -[at]- mcgeary [*dot*] org)
;;
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

;; Defaults
(defconst *liquid-metal-score-no-match* 0)
(defconst *liquid-metal-score-match* 100)
(defconst *liquid-metal-score-trailing* 80)
(defconst *liquid-metal-score-trailing-but-started* 90)
(defconst *liquid-metal-score-buffer* 85)

(defun liquid-metal (string abbreviation &optional started current-score)
  "Return the matching distance between string and abbreviation"
  (if (not (stringp string))
      (error "Argument was not a string: %s" string))
  (if (not (stringp abbreviation))
      (error "Argument was not a string: %s" abbreviation))

  (if (equal nil current-score)
      ;; Top invocation
      (let
          ((scores (liquid-metal string abbreviation nil '('marker))))
        (/ (reduce '+ (cdr scores)) (- (length scores) 1)))
    ;; Recursive invocation
    (let ((a (length abbreviation)))
      (cond ((= a 0)
             (if started
                 (append
                  current-score
                  (map 'list (lambda (x) *liquid-metal-score-trailing-but-started*) string))
               (append
                current-score
                (map 'list (lambda (x) *liquid-metal-score-trailing*) string))))
            ((> a (length string))
             (list 'marker *liquid-metal-score-no-match*))
            (t (let
                   ((index (position (aref (downcase abbreviation) 0) (downcase string))))
                 (if (equal nil index)
                     (list 'marker *liquid-metal-score-no-match*)
                   (liquid-metal (substring string (+ index 1))
                                 (substring abbreviation 1)
                                 (if (= index 0) t started)
                                 (append
                                  current-score
                                  (cond ((equal index nil)
                                         ;; No matching character
                                         (list *liquid-metal-score-dead*))
                                        ((and (> index 0)
                                              (string-match "^[[:blank:]-_]$"
                                                            (char-to-string (aref string (- index 1)))))
                                         ;; Matching after a delimiter
                                         (append (make-list (- index 1) *liquid-metal-score-buffer*)
                                                 (make-list 2 *liquid-metal-score-match*)))
                                        ((= (aref (upcase string) index) (aref string index))
                                         ;; Matching an upcase character
                                         (append (make-list index *liquid-metal-score-buffer*)
                                                 (list *liquid-metal-score-match*)))
                                        (t
                                         ;; Simple match
                                         (append (make-list index *liquid-metal-score-no-match*)
                                                 (list *liquid-metal-score-match*)))))))))))))

(provide 'liquid-metal)
