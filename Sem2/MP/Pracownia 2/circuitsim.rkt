#lang racket
(require data/heap)
#;(provide sim? wire?
         (contract-out
          [make-sim        (-> sim?)]
          [sim-wait!       (-> sim? positive? void?)]
          [sim-time        (-> sim? real?)]
          [sim-add-action! (-> sim? positive? (-> any/c) void?)]

          [make-wire       (-> sim? wire?)]
          [wire-on-change! (-> wire? (-> any/c) void?)]
          [wire-value      (-> wire? boolean?)]
          [wire-set!       (-> wire? boolean? void?)]

          [bus-value (-> (listof wire?) natural?)]
          [bus-set!  (-> (listof wire?) natural? void?)]

          [gate-not  (-> wire? wire? void?)]
          [gate-and  (-> wire? wire? wire? void?)]
          [gate-nand (-> wire? wire? wire? void?)]
          [gate-or   (-> wire? wire? wire? void?)]
          [gate-nor  (-> wire? wire? wire? void?)]
          [gate-xor  (-> wire? wire? wire? void?)]

          [wire-not  (-> wire? wire?)]
          [wire-and  (-> wire? wire? wire?)]
          [wire-nand (-> wire? wire? wire?)]
          [wire-or   (-> wire? wire? wire?)]
          [wire-nor  (-> wire? wire? wire?)]
          [wire-xor  (-> wire? wire? wire?)]

          [flip-flop (-> wire? wire? wire? void?)]))

; ----------------------
; Assesment: we cant mix different simulations

; ----- SIMULATION -----
(struct event (time action) #:transparent)
(struct sim (time actions) #:mutable #:transparent)

(define (make-sim)
    (sim 0 
        (make-heap (lambda (e1 e2) 
            (<= (event-time e1) (event-time e2))))))

(define (sim-wait! S time)
    (define (call-all) (cond
        [(= 0 (heap-count (sim-actions S))) (void)]
        [(< (sim-time S) (event-time (heap-min (sim-actions S)))) 
            (void)]
        [else (begin
            ((event-action (heap-min (sim-actions S))))
            (heap-remove-min! (sim-actions S))
            (call-all))]))

    (if (= time 0) (void) (begin
    (call-all)
    (set-sim-time! S (+ (sim-time S) 1))
    (sim-wait! S (- time 1)))))

(define (sim-add-action! S time action)
    (heap-add! 
        (sim-actions S) 
        (event (+ time (sim-time S)) action)))

(define (sim-add-action-now! S action)
    (sim-add-action! S 0 action))

; ----- WIRE -----
(struct wire (value actions sim) #:mutable #:transparent)

(define (make-wire S)
    (wire #f '() S))

(define (wire-on-change! wire action-procedure)
    (set-wire-actions! 
        wire
        (cons 
            action-procedure
            (wire-actions wire))))

(define (wire-set! wire new_value)
    (let ((old_value (wire-value wire)))
        (set-wire-value! wire new_value)
        (if (not (equal? old_value new_value))
            (for-each (lambda (action) 
                    ;(sim-add-action-now! (wire-sim wire) action))       ;;todo which is correct?
                    (action))
                (wire-actions wire))
            (void))))

; ------ GATES -----
(define gate-not-delay  1)
(define gate-and-delay  1)
(define gate-nand-delay 1)
(define gate-or-delay   1)
(define gate-nor-delay  1)
(define gate-xor-delay  2)

(define (gate-not in out)
    (define (_gate-not)
        (let ((new-value (not (wire-value in))))           ; todo that should be calculated in lambda
             (after-delay 
                (wire-sim in)
                gate-not-delay
                (lambda ()
                    (wire-set! out new-value)))))
    (wire-on-change! in _gate-not)
    (_gate-not))

(define (gate-and in1 in2 out)
    (define (_gate-and)
        (let ((new-value (and (wire-value in1) (wire-value in2))))
             (after-delay 
                (wire-sim in1)
                gate-and-delay
                (lambda ()
                    (wire-set! out new-value)))))
    (wire-on-change! in1 _gate-and)
    (wire-on-change! in2 _gate-and)
    (_gate-and))

(define (gate-nand in1 in2 out)
    (define (_gate-nand)
        (let ((new-value (not (and (wire-value in1) (wire-value in2)))))
             (after-delay 
                (wire-sim in1)
                gate-nand-delay
                (lambda ()
                    (wire-set! out new-value)))))
    (wire-on-change! in1 _gate-nand)
    (wire-on-change! in2 _gate-nand)
    (_gate-nand))

(define (gate-or in1 in2 out)
    (define (_gate-or)
        (let ((new-value (or (wire-value in1) (wire-value in2))))
             (after-delay 
                (wire-sim in1)
                gate-or-delay
                (lambda ()
                    (wire-set! out new-value)))))
    (wire-on-change! in1 _gate-or)
    (wire-on-change! in2 _gate-or)
    (_gate-or))

(define (gate-nor in1 in2 out) 
    (define (_gate-nor)
        (let ((new-value (not (or (wire-value in1) (wire-value in2))))) 
             (after-delay 
                (wire-sim in1)
                gate-nor-delay
                (lambda ()
                    (wire-set! out new-value)))))
    (wire-on-change! in1 _gate-nor)
    (wire-on-change! in2 _gate-nor)
    (_gate-nor))

(define (gate-xor in1 in2 out)
    (define (_gate-xor)
        (let ((new-value (xor (wire-value in1) (wire-value in2))))
             (after-delay 
                (wire-sim in1)
                gate-xor-delay
                (lambda ()
                    (wire-set! out new-value)))))
    (wire-on-change! in1 _gate-xor)
    (wire-on-change! in2 _gate-xor)
    (_gate-xor))

(define (after-delay S delay action)
    (sim-add-action! S delay action))

; ----- SYNTACTIC ICING (WIRE) -----


; ----- BUS -----
(define (bus-set! wires value)
  (match wires
    ['() (void)]
    [(cons w wires)
     (begin
       (wire-set! w (= (modulo value 2) 1))
       (bus-set! wires (quotient value 2)))]))

(define (bus-value ws)
  (foldr (lambda (w value) (+ (if (wire-value w) 1 0) (* 2 value)))
         0
         ws))

; ----- ?SIMULATION? -----
#;(define (flip-flop out clk data)
  (define sim (wire-sim data))
  (define w1  (make-wire sim))
  (define w2  (make-wire sim))
  (define w3  (wire-nand (wire-and w1 clk) w2))
  (gate-nand w1 clk (wire-nand w2 w1))
  (gate-nand w2 w3 data)
  (gate-nand out w1 (wire-nand out w3)))

; ----- TEST -----
(define SIM (make-sim))
(define w1 (make-wire SIM))
(define w2 (make-wire SIM))
(define w3 (make-wire SIM))
(gate-not w1 w2)
(gate-not w2 w3)
(gate-not w3 w1)
