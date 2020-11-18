#lang racket

(define hashVar (hash))
(define pos 0)
(define lastWordPos 0)

(define (check text)
  (set! pos 0)
  (set! lastWordPos 0)
  (set! hashVar (hash-clear hashVar))
  (checkCode text)
)

(define (checkCode text)
  (set! lastWordPos pos)
   (define start (getWord text ""))
   ;(printf "~a\n\n" start)
   (cond [(not(eq? (string-length start) 0))
     (cond [(string=? start "#define")
            (set! lastWordPos pos)
            (define var (getWord text "")) 
            (cond [(and (not (string=? var "define" ))  (eq?(hash-has-key? hashVar var) #f) )
                   (set! lastWordPos pos)
                   (define val (string->number (getWord text "")))
                   (set! hashVar (hash-set hashVar var val))
                   (checkCode text)
                   ]
                  [else
                   (printf "ОШИБКА: Идентификатор должен быть уникален и не пересекаться с ключевыми словами ~a  | ПОЗ: ~a\n" var lastWordPos)
                   ]
                  )
            ]
           [else
            (printf "ОШИБКА: Команда должна начинаться с #define   | ПОЗ: ~a\n" lastWordPos)
            ]
           )
     ]
   )
  hashVar
)

(define (getWord text word)
  (define pos_prev pos)
  ;(printf "WORD = ~a\n" word)
  (cond [ (= (string-length text) pos_prev)
           word
          ]
  [else
   (cond [(= (string-length word) 0)
          (cond [(char-whitespace? (string-ref text pos_prev))
            (set! pos (+ pos_prev 1))
            (getWord text  "")
              ]
            [else
               (set! pos (+ pos_prev 1))
              (getWord text (string-append word (make-string 1 (string-ref text pos_prev))))
             ]    
           )
          ]
    [else   
     (cond [(char-whitespace? (string-ref text pos_prev))
            word
            ]
           [else
            (set! pos (+ pos_prev 1))
             (getWord text (string-append word (make-string 1 (string-ref text pos_prev))))
            ]
           )
     ]
   )
   ]
  )
)

(printf "НИКОЛАЕВ НИКИТА СЕРГЕЕВИЧ - ИКБО-13-17 - ПРАКТИКА 5 - ВАРИАНТ 9\n")
(define code_1 "#define x 5     #define k 6 define")
(define code_2 "#define x 5     #define k 6 #define l 7")
(define code_3 "#define x 5     #define x 6 #define l 7")
(printf "РЕЗУЛЬТАТ (ХЭШ-ТАБЛИЦА): ~a\n\n" (check code_1))
(printf "РЕЗУЛЬТАТ (ХЭШ-ТАБЛИЦА): ~a\n\n" (check code_2))
(printf "РЕЗУЛЬТАТ (ХЭШ-ТАБЛИЦА): ~a\n\n" (check code_3))
