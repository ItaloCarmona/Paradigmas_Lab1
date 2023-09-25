#lang racket
;Constructor de opciones
(define (option code message ChatbotCodeLink FlowCodeLink . Keyword)
  (list code
        message
        ChatbotCodeLink
        FlowCodeLink
        Keyword))

;Constructor flujo
(define (flow name . options)
  (list name options))

;Modificador flujo
(define (flow-add-option flow option)
  (let ((name (car flow))
        (options (cdr flow)))
    (list name (append options (list option)))))

;Constructor ChatBot
(define (chatbot name chatbot-message . flows)
  (list name
        chatbot-message
        flows))

;Modificador ChatBot
(define (chatbot-add-flow chatbot flow)
  (let ((flows (cddr chatbot)))
    (list (car chatbot)
          (cadr chatbot)
          (add-flow-to-list flows flow))))
;Función complementaria
(define (add-flow-to-list flows flow)
  (if (null? flows)
      (cons flow flows)
      (if (and (not (null? (car flows))) (equal? (caar flows) (car flow)))
          (cons (list (caar flows) (append (cdar flows) (cdr flow))) (cdr flows))
          (cons (car flows) (add-flow-to-list (cdr flows) flow)))))

; Función para guardar la fecha y hora en la que se creó un archivo
(define (current-date)
  (let ((date (seconds->date (current-seconds))))
    (string-append
     (number->string (date-year date)) "-"
     (number->string (date-month date)) "-"
     (number->string (date-day date)) " "
     (number->string (date-hour date)) ":"
     (number->string (date-minute date)) ":"
     (number->string (date-second date)))))

;Constructor sistema
(define (system name . chatbots)
  (list name
        (current-date)
        chatbots))

;Modificador sistema
(define (system-add-chatbot system chatbot)
  (list (car system)
        (cadr system)
        (cons chatbot (caddr system))))



#| Comprobaciones

(define op1 (option "1) Viajar" 1 2 "viajar" "turistear" "conocer"))
(define op2 (option "2) Estudiar" 1 3 "aprender" "perfeccionarme"))
(define op3 (option "3) Comer" 1 4 "Almorzar" "Cenar"))
(define op4 (option "4) America" 4 3 "aprender" "perfeccionarme"))
(define op5 (option "5) Europa" 4 3 "aprender" "perfeccionarme"))
(define op6 (option "6) Bailar" 4 3 "aprender" "perfeccionarme"))
(define op7 (option "7) Practicar" 4 3 "aprender" "perfeccionarme"))

(define f0 (flow "Inicio" op1 op2))
(define f2 (flow "Ubicación" op4 op5))
(define f3 (flow "Inicio1" op6 op7))

(define f1 (flow-add-option f0 op3))


(define cb0 (chatbot "Asistente" "Bienvenido. ¿Qué te gustaría hacer?" f1))

(define cb1 (chatbot "Viajes" "De dónde eres?"))

(define cb2 (chatbot-add-flow cb1 f2))
(define cb3 (chatbot-add-flow cb0 f3))

(define s0 (system "S0"))
(define s1 (system "S1" cb3))

(define s2 (system-add-chatbot s0 cb2))
(define s3 (system-add-chatbot s2 cb3)
|# 