#lang racket


;; there could be a *lot* of good tests here...


(check-equal? (tidy-imag -0.9283+2.2938792e-17i) -0.9283)
(check-equal? (tidy-imag 0.9283+2.2938792e-17i) 0.9283)