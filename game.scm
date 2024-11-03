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
             (ice-9 match)
             (math))


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

;; Lib
(define (sub-string-list string-list start len func)
  (define (sub-string-list-iter string-list start len func part)
    (if (string? (car string-list))
        (if (> (string-length (car string-list)) start)
            (append part
                    (if (= start 0 )
                        '()
                        (list (substring (car string-list) 0 start)))
                    (list (func (substring (car string-list) start len)))
                    (if (= len (string-length (car string-list)))
                        '()
                        (list (substring (car string-list) len (string-length (car string-list)))))
                    (cdr string-list))
            (sub-string-list-iter (cdr string-list)
                                  (abs (- start (string-length (car string-list))))
                                  (abs (- len (string-length (car string-list))))
                                  func
                                  (append part (list (car string-list)))))
        (sub-string-list-iter (cdr string-list)
                              (- start (string-length (car (last-pair (car string-list)))))
                              (- len (string-length (car (last-pair (car string-list)))))
                              func
                              (append part (list (car string-list))))))
  (sub-string-list-iter string-list start len func '()))

(define (find-string-list-len string-list)
  (define (find-string-list-len-iter string-list offset)
    (if (string? (car string-list))
        (if (string-index (car string-list) #\newline)
            (+ (string-index (car string-list) #\newline) offset 1)
            (find-string-list-len-iter (cdr string-list) (+ (string-length (car string-list)) offset)))
        (find-string-list-len-iter (cdr string-list) (+ (string-length (car (last-pair (car string-list)))) offset) )))
  (find-string-list-len-iter string-list 0))

(define (gen-update-list string-length x y xlen ylen)
  (define (up-iter string-length x y xlen llen part)
    (if (= 0 llen)
        part
        (up-iter string-length
                   x
                   (+ 1 y)
                   xlen
                   (- llen 1)
                   (append part
                           (list (list
                                  (+ (* y string-length) x )
                                  (+ xlen (+ (* y string-length) x))
                                  ))))))
  (up-iter string-length x y xlen ylen '()))

(define (gen-string-list string-list update-list func)
  (let loop ((string-list string-list)
             (update-list update-list))
    (if (equal? update-list '())
        string-list
        (loop (sub-string-list string-list
                               (car (car update-list))
                               (cadr (car update-list))
                               func)
              (cdr update-list)))))

;; render
(define *template* '())
(define (wrap-template template)
  `(div (@ (id "container"))
    (div
     (span ,(number->string *wood*) " wood ")
     (span ,(number->string *stone*) " stone ")
     (span ,(number->string *seed*) " seed "))
    (button (@ (click ,(lambda (event)
                         (set! *template* template-map)
                         (render))))
            "Map")
    (div (@ (id "application")) ,(template))))

(define (render)
  (let ((old (get-element-by-id "container")))
    (unless (external-null? old) (remove! old))
  (append-child! (document-body) (sxml->dom (wrap-template *template*)))))

;; woods
(define *gather* #f)
(define (template-woods)
  (define wood-script '(
                        "  888888888/'                     888888888/'    "
                        " 88\\*888;*/88     888888888/'    88\\*888;*/88  "
                        "888'\\*88|/8888   88\\*888;*/88   888'\\*88|/8888"
                        "8888{888}88_/8  888'\\*88|/8888  8888{888}88_/8  "
                        "88\\88\\. /_'88   8888{888}88_/8  88\\88\\. /_'88  "
                        "   \\_} {        88\\88\\. /_'88      \\_} {      "
                        "     { }           \\_} {             { }        "
                        "     } {             { }             } {         "
                        "     { }             } {      o      { }         "
                        "_o__//..\\\\____O______{ }______|_____//..\\\\_  "
                        "                    //..\\\\                     "
                        ))
  `(pre ,(string-append (string-join wood-script "\n") "\n")
    ,(if *gather*
        `(button (@ (click ,(lambda (event)
                             (set! *gather* #f)
                             (render))))
                "Stop gather")
        `(button (@ (click ,(lambda (event)
                             (set! *gather* #t)
                             (render))))
                "Start gather"))))

;; Map
(define (template-map)
  (define (blod string)
    `(b (@ (click ,(lambda (event)
                     (set! *template* template-woods)
                     (render)))
           (style "cursor: pointer;")
           (title "Woods"))
      ,string ))
  (define (farm string)
    `(b (@ (click ,(lambda (event)
                     (set! *template* template-farm)
                     (render)))
           (style "cursor: pointer;")
           (title "Farm"))
      ,string ))
  (define map-script '(
                       "______________________________________"
                       "| 88            88      88    88     |"
                       "|8888  88      8888    8888  88888   |"
                       "| ||  8888      ||      ||    ||     |"
                       "|      ||     88      88     88      |"
                       "|      8888  8888    88888  88888    |"
                       "|       ||    ||      ||     ||      |"
                       "|                                    |"
                       "|                                    |"
                       "|                                    |"
                       "|                             _      |"
                       "|                            /%\\     |"
                       "|                            | |     |"
                       "|                                    |"
                       ":____________________________________:"
                       ))
  `(pre ,@(gen-string-list (gen-string-list (list (string-join map-script "\n"))
                                            (gen-update-list
                                             (find-string-list-len (list (string-join map-script "\n"))) 1 1 36 6)
                                            blod)
                           (gen-update-list
                            (find-string-list-len (list (string-join map-script "\n"))) 29 10 3 3)
                           farm)))

;; Plant
(define-record-type <plant>
  (make-plant name time watering?)
  plant?
  (name plant-name)
  (time plant-time set-plant-time!)
  (watering? plant-watering? set-plant-watering!))

(define *plants* '())
(define (add-plant! plant)
  (set! *plants* (cons plant *plants*)))
(define (remove-plant! plant)
  (set! *plants* (delq plant *plants*)))

(define (template-plant)
  (define (plant-template plant)
    `(li
      (span (@ (style "padding: 0 1em 0 1em;"))
            ,(string-append (plant-name plant) " "(number->string (plant-time plant))))
      ,(if (plant-watering? plant)
           `(span "")
           (if *well-build?*
           `(a (@ (href "#")
                  (click ,(lambda (event)
                      (set-plant-time! plant (ceiling (/ (plant-time plant) 2)))
                      (set-plant-watering! plant #t)
                      (render)))) "watering ")
           `(span "")))
      (a (@ (href "#")
            (click ,(lambda (event)
                      (remove-plant! plant)
                      (render))))
         ,(if (= 0 (plant-time plant))
              "gather "
              "remove "))))
  (define (plant)
    (list-ref '("🍆" "🥔" "🥕" "🌽" "🌶️" "🫑" "🥒" "🥬" "🥦" "🧄" "🧅" "🥜" "🫘" "🌰" "🫚" "🫛" "🍄‍"
                "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" "🎃" ) (random-int 34)))
  `(div
    (h2 "Fields")
    (ul ,@(map plant-template (reverse *plants*)))
    ,(if *fields-clean?*
        `(button (@ (click ,(lambda (event)
                              (when (> *seed* 0)
                                (set! *seed* (- *seed* 1))
                                (add-plant! (make-plant (plant) (+ 300 (random-int 300)) #f))
                                (render)))))
          "Plant")
        `(button (@ (click ,(lambda (event)
                              (when (and (> *wood* 30) (> *stone* 10))
                                (set! *wood* (- *wood* 30))
                                (set! *stone* (- *wood* 10))
                                (set! *seed* (+ *seed* 7))
                                (set! *fields-clean?* #t)
                                (render)))))
          "Clear the fields (wood x30 stone x10)")
        )))

(define *workshop-clean?* #f)
(define *cart-build?* #f)
(define *reaping-hook-build?* #f)
(define *well-build?* #f)
(define (template-workshop)
  `(div
    (h2 "Workshop")
    ,(if *workshop-clean?*
        `(ul
          (li
           ,(if *cart-build?*
                `(span "Cart")
                `(button (@ (click ,(lambda (event)
                                      (when (> *wood* 100)
                                        (set! *wood* (- *wood* 100))
                                        (set! *cart-build?* #t)
                                        (render)))))
                  "Cart (wood x100)")))
          (li
           ,(if *reaping-hook-build?*
                `(span "Reaping hook")
                `(button (@ (click ,(lambda (event)
                                      (when (and (> *wood* 20) (> *stone* 20))
                                        (set! *wood* (- *wood* 20))
                                        (set! *stone* (- *stone* 20))
                                        (set! *reaping-hook-build?* #t)
                                        (render)))))
                  "Reaping hook (wood x20 stone x20)")))
          (li
           ,(if *well-build?*
                `(span "Well")
                `(button (@ (click ,(lambda (event)
                                      (when (and (> *wood* 50) (> *stone* 200))
                                        (set! *wood* (- *wood* 50))
                                        (set! *stone* (- *stone* 200))
                                        (set! *well-build?* #t)
                                        (render)))))
                  "Well (wood x50 stone x200)"))))
        `(button (@ (click ,(lambda (event)
                              (when (and (> *wood* 200) (> *stone* 50))
                                (set! *wood* (- *wood* 200))
                                (set! *stone* (- *stone* 50))
                                (set! *workshop-clean?* #t)
                                (render)))))
          "Clear the workshop (wood x200 stone x50)"))))

;; Farm
(define (template-farm)
  (define (blod string)
    `(b (@ (click ,(lambda (event)
                     (set! *template* template-woods)
                     (render)))
           (style "cursor: pointer;")
           (title "Woods"))
      ,string ))
  (define map-script '(
                       "                                          __      "
                       "                   ,-_                  (`  ).    "
                       "                   |-_'-,              (     ).   "
                       "                   |-_'-'           _(        '`. "
                       "          _        |-_'/        .=(`(      .     )"
                       "         /;-,_     |-_'        (     (.__.:-`-_.' "
                       "        /-.-;,-,___|'          `(       ) )       "
                       "       /;-;-;-;_;_/|\\_ _ _ _ _   ` __.:'   )      "
                       "          x_( __`|_P_|`-;-;-;,|        `--'       "
                       "          |\\ \\    _||   `-;-;-'                   "
                       "          | \\`   -_|.      '-'                    "
                       "          | /   /-_| `                            "
                       "          |/   ,'-_|  \\                           "
                       "          /____|'-_|___\\                          "
                       "   _..,____]__|_\\-_'|_[___,.._                    "
                       "  '                          ``'--,..,.           "
                       "                                                  "
                       "  :--:---:---:---:---:---:--:                     "
                       "  | #.|%& #.|%& #.|%& #.|%& |                     "
                       "  | #.|%& #.|%& #.|%& #.|%& |                     "
                       "  | #.|%& #.|%& #.|%& #.|%& |             _       "
                       "  | #.|%& #.|%& #.|%& #.|%& |            (O)      "
                       "  | #.|%& #.|%& #.|%& #.|%& |             ¨       "
                       "  | #.|%& #.|%& #.|%& #.|%& |                     "
                       "  | #.|%& #.|%& #.|%& #.|%& |                     "
                       "  | #.|%& #.|%& #.|%& #.|%& |                     "
                       "  | #.|%& #.|%& #.|%& #.|%& |                     "
                       "  :--:---:---:---:---:---:--:                     "
                       ))
  `(pre ,(string-append (string-join map-script "\n") "\n")
    ,(template-plant)
    ,(template-workshop)))


;; Main
(set! *template* template-map)

(define *wood* 0)
(define *stone* 0)
(define *seed* 0)
(define *fields-clean?* #f)
(define (template-main)
  `(div
    (span ,(number->string *wood*) " wood ")
    (span ,(number->string *stone*) " stone ")
    (span ,(number->string *seed*) " seed ")))

(define *update-list* '())

(define dt 1000.0)

(define-record-type <timeout-function>
  (make-timeout-function interval countdown func)
  timeout-function?
  (interval timeout-function-interval set-timeout-function-interval!)
  (countdown timeout-function-countdown set-timeout-function-countdown!)
  (func timeout-function-func))

(define *cart-build?* #f)
(define *reaping-hook-build* #f)
(define (plus-res)
  (if *gather*
      (begin
        (if *cart-build?*
            (set! *wood* (+ *wood* 12))
            (set! *wood* (+ *wood* 5)))
        (if *cart-build?*
            (set! *stone* (+ *stone* 7))
            (set! *stone* (+ *stone* 3)))
        (if *reaping-hook-build?*
            (when (> 30 (random-int 100))
                   (set! *seed* (+ *seed* 1)))
            (when (> 5 (random-int 100))
                   (set! *seed* (+ *seed* 1)))))))

(define (update-plant-time)
  (for-each (lambda (i)
              (unless (= (plant-time i) 0)
                (set-plant-time! i (- (plant-time i) 1))
                (render)))
            *plants*))

(set! *update-list* (append *update-list* (list (make-timeout-function 1 1 plus-res))))
(set! *update-list* (append *update-list* (list (make-timeout-function 1 1 update-plant-time))))

(define (update)
  (for-each (lambda (i)
              (if (= (timeout-function-countdown i) 0)
                  (begin
                    ((timeout-function-func i))
                    (set-timeout-function-countdown! i (timeout-function-interval i))
                    (render))
                  (set-timeout-function-countdown! i (- (timeout-function-countdown i) 1))))
            *update-list*)
  (timeout update-callback dt))

(define update-callback (procedure->external update))
(render)
(timeout update-callback dt)
