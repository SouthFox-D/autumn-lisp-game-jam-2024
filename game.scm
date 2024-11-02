;;; Copyright (C) 2024 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Example game showing off several common game programming things.
;;;
;;; Code:

(use-modules (scheme base)
             (dom document)
             (dom element)
             (dom event)
             (dom window)
             (math vector)
             (hoot ffi)
             (hoot debug)
             (hoot hashtables)
             (srfi srfi-9)
             (ice-9 match))


(define *template* '())
(define (wrap-template template)
  `(div (@ (id "container"))
    (button (@ (click ,(lambda (event)
                            (set! *template* template-click)
                            (render))))
                "Click")
    (button (@ (click ,(lambda (event)
                            (set! *template* template-task)
                            (render))))
                "Task")
    (button (@ (click ,(lambda (event)
                            (set! *template* template-ascii-art)
                            (render))))
                "Ascii art")
    (div (@ (id "application")) ,(template))))

(define (render)
  (let ((old (get-element-by-id "container")))
    (unless (external-null? old) (remove! old))
  (append-child! (document-body) (sxml->dom (wrap-template *template*)))))

(define (sxml->dom exp)
  (match exp
    ;; The simple case: a string representing a text node.
    ((? string? str)
     (make-text-node str))
    ;; An element tree.  The first item is the HTML tag.
    (((? symbol? tag) . body)
     ;; Create a new element with the given tag.
     (let ((elem (make-element (symbol->string tag))))
       (define (add-children children)
         ;; Recursively call sxml->dom for each child node and
         ;; append it to elem.
         (for-each (lambda (child)
                     (append-child! elem (sxml->dom child)))
                   children))
       (match body
         ;; '@' denotes an attribute list.  Child nodes follow.
         ((('@ . attrs) . children)
          ;; Set attributes.
          (for-each (lambda (attr)
                      (match attr
                        ;; Attributes are (symbol string) tuples.
                        (((? symbol? name) (? string? val))
                         (set-attribute! elem
                                         (symbol->string name)
                                         val))
                        (((? symbol? name) (? boolean? val))
                         (set-attribute! elem
                                         (symbol->string name)
                                         val))
                        (((? symbol? name) (? procedure? proc))
                         (add-event-listener! elem
                                       (symbol->string name)
                                       (procedure->external proc)))))
                    attrs)
          (add-children children))
         ;; No attributes, just a list of child nodes.
         (children (add-children children)))
       elem))))

;; Click
(define *clicks* 0)
(define (template-click)
  `(div
    (p ,(number->string *clicks*) " clicks")
    (button (@ (click ,(lambda (event)
                            (set! *clicks* (+ *clicks* 1))
                            (render))))
                "Click me!")))

;; Task
(define-record-type <task>
  (make-task name done?)
  task?
  (name task-name)
  (done? task-done? set-task-done!))

(define *tasks* '())
(define (add-task! task)
  (set! *tasks* (cons task *tasks*)))
(define (remove-task! task)
  (set! *tasks* (delq task *tasks*)))

(define (template-task)
  (define (task-template task)
    `(li (input (@ (type "checkbox")
                   (change ,(lambda (event)
                              (let* ((checkbox (event-target event))
                                     (checked? (element-checked? checkbox)))
                                (set-task-done! task checked?)
                                (render))))
                   (checked ,(task-done? task))))
         (span (@ (style "padding: 0 1em 0 1em;"))
               ,(if (task-done? task)
                    `(s ,(task-name task))
                    (task-name task)))
         (a (@ (href "#")
               (click ,(lambda (event)
                         (remove-task! task)
                         (render))))
            "remove")))
  `(div
    (h2 "Tasks")
    ;; Tasks are stored in reverse order.
    (ul ,@(map task-template (reverse *tasks*)))
    (input (@ (id "new-task")
              (placeholder "Write more task")))
    ;; Add new task on click
    (button (@ (click ,(lambda (event)
                         (let* ((input (get-element-by-id "new-task"))
                                (name (element-value input)))
                           (unless (string=? name "")
                             (add-task! (make-task name #f))
                             (set-element-value! input "")
                             (render))))))
            "Add task")))


;; render ascii art
(define (template-ascii-art)
  (define ink-script '(
                       "(\\                         "
                       "\\'\\                        "
                       " \\'\\     __________        "
                       " / '|   ()_________)       "
                       " \\ '/    \\ ~~~~~~~~ \\      "
                       "   \\       \\ ~~~~~~   \\    "
                       "   ==).      \\__________\\  "
                       "  (__)       ()__________) "
                       ))
  `(pre ,(string-join ink-script "\n")))

;; Lib
(define (sub-string-list string-list start len)
  (define (sub-string-list-iter string-list start len part)
    (if (string? (car string-list))
        (if (> (string-length (car string-list)) start)
            (append part
                    (if (= start 0 )
                        '()
                        (substring (car string-list) 0 start))
                    (list (substring (car string-list) start (+ start len)))
                    (if (= (+ start len) (string-length (car string-list)))
                        '()
                        (list (substring (car string-list) (+ start len) (string-length (car string-list)))))
                    (cdr string-list))
            (sub-string-list-iter (cdr string-list)
                                  (- start (string-length (car string-list)))
                                  len
                                  (append part (list (car string-list)))))
        (sub-string-list-iter (cdr string-list)
                              (- start (string-length (car (last-pair (car string-list)))))
                              len
                              (append part (list (car string-list))))))
  (sub-string-list-iter string-list start len '()))

(define (find-string-list-len string-list)
  (define (find-string-list-len-iter string-list offset)
    (if (string? (car string-list))
        (if (string-index (car string-list) #\newline)
            (+ (string-index (car string-list) #\newline) offset)
            (find-string-list-len-iter (cdr string-list) (+ (string-length (car string-list)) offset)))
        (find-string-list-len-iter (cdr string-list) (+ (string-length (car (last-pair (car string-list)))) offset) )))
  (find-string-list-len-iter string-list 0))

(define (gen-update-list string-length x y len)
  (define (up-iter string-length x y llen part)
    (if (= 0 llen)
        part
        (up-iter string-length
                   x
                   (+ 1 y)
                   (- llen 1)
                   (append part
                           (list (list
                                  (+ (* y string-length) x )
                                  (+ len (+ (* y string-length) x))
                                  ))))))
  (up-iter (+ 1 string-length) x y len '()))

;; Main
(set! *template* template-task)

(define *update-list* '())
(define dt (/ 1000.0 60.0))

(define-record-type <timeout-function>
  (make-timeout-function interval countdown func)
  timeout-function?
  (interval timeout-function-interval set-timeout-function-interval!)
  (countdown timeout-function-countdown set-timeout-function-countdown!)
  (func timeout-function-func))

(define (plus-clicks)
  (set! *clicks* (+ *clicks* 1)))

(set! *update-list* (append *update-list* (list (make-timeout-function 60 60 plus-clicks))))

(define (update)
  (for-each (lambda (i)
              (dprint "countdown" (timeout-function-countdown i))
              (if (= (timeout-function-countdown i) 0)
                  (begin
                    ((timeout-function-func i))
                    (set-timeout-function-countdown! i (timeout-function-interval i))
                    (if (equal? *template* template-click)
                        (render)))
                  (set-timeout-function-countdown! i (- (timeout-function-countdown i) 1))))
            *update-list*)
  (timeout update-callback dt))

(define update-callback (procedure->external update))
(render)
(timeout update-callback dt)
