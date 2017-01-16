;;  -*-  indent-tabs-mode:nil; coding: utf-8 -*-
;;  Copyright (C) 2017
;;      "Mu Lei" known as "NalaGinrut" <NalaGinrut@gmail.com>
;;  Gat is free software: you can redistribute it and/or modify
;;  it under the terms of the GNU General Public License published
;;  by the Free Software Foundation, either version 3 of the License,
;;  or (at your option) any later version.

;;  Gat is distributed in the hope that it will be useful,
;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;  GNU General Public License for more details.

;;  You should have received a copy of the GNU General Public License
;;  along with this program.
;;  If not, see <http://www.gnu.org/licenses/>.

(define-module (gat cmd)
  #:use-module (gat irregex)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-26)
  #:use-module ((rnrs) #:select (define-record-type))
  #:export (cmd wcmd print-cmd-result))

(define-record-type cmd-result
  (fields pipe status contents))

(define (parse-from-pipe pp read-only?)
  (let lp ((ret '()))
    (cond
     ((eof-object? (peek-char pp))
      (if read-only?
          (make-cmd-result #f (close-pipe pp) ret)
          (make-cmd-result pp #f ret)))
     (else
      (lp (cons (irregex-split " " (read-line pp)) ret))))))

(define-syntax-rule (%cmd read-only? args ...)
  (let* ((cmd (string-join (map object->string `(args ...)) " "))
         (result (parse-from-pipe (open-pipe cmd OPEN_BOTH) read-only?)))
    (when (not (zero? (status:exit-val (cmd-result-status result))))
      (format #t "[ERROR] ~a~%" cmd)
      (throw 'gat (format #f "cmd error: ~a" (cmd-result-status result))))
    result))

(define-syntax-rule (wcmd args ...) (%cmd #f args ...))
(define-syntax-rule (cmd args ...) (%cmd #t args ...))

(define* (print-cmd-result cr #:key (port (current-output-port)) (width 10)
                           (right-align? #t))
  (define (align) (if right-align? "@" ""))
  (let ((contents (cmd-result-contents cr))
        (fmt (string-append "~{~" (number->string width) (align) "a~^ ~}~%")))
    (for-each
     (lambda (line)
       (format port fmt line))
     contents)))

(define (cmd-result-column-ref cr columns)
  (define (get-colums contents cols) ; cols is a list contains columns
    (map (lambda (x) (map (cut list-ref <> x) contents)) cols))
  (let ((contents (cmd-result-contents cr))
        (cn (sort columns <)))
    (get-colums contens cn)))

(define (cmd-result-line-ref cr lines)
  (define (get-lines contents lines)
    (map (lambda (x) (map (cut list-ref <> x) contents)) lines))
  (let ((contents (cmd-result-contents cr))
        (ln (sort lines <)))
    (get-lines contens ln)))
