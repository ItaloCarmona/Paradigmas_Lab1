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





