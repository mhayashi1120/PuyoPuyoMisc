(use gauche.sequence)
(use srfi-1)

(debug-print-width #f)

(define (main args)
  (let1 file (cadr args)
    (let1 block (read-block-from-file file)
      (print-block block)
      (boot-puyopuyo block)))
  0)

(define (boot-puyopuyo block)
  (let loop ((rensa 1)
             (group (collect-groups block)))
    (when (pair? group)
      (let1 newblock (apply-groups block group)
        (print-block newblock rensa)
        (loop (+ rensa 1) (collect-groups newblock))))))

(define (apply-groups block groups)
  (map
   (lambda (group)
     (set! block (apply-group block group)))
   groups)
  block)

(define (apply-group block group)
  (map
   (lambda (cell)
     (let* ((x (car cell))
            (y (cdr cell)))
       (set-cell! (list-ref block y) x #\space)))
   group)
  (map-block
   (lambda (x pseudo-y color)
     (let1 y (- (length block) pseudo-y 1)
       (when (eq? color #\space)
         (let ((line (list-ref block y)))
           (receive (newcolor orig-y)
               (falling-to block x y)
             (let1 line (list-ref block y)
               (set-cell! line x newcolor))
             (when orig-y
               (let1 line (list-ref block orig-y)
                 (set-cell! line x #\space))))))))
   (reverse block))
  block)

(define (falling-to block x y)
  (let1 color (color-at block x y)
    (cond
     ((not (eq? color #\space))
      (values color y))
     ((> y 0)
      (falling-to block x (- y 1)))
     (else
      (values #\space #f)))))

(define (map-block proc block)
  (map-with-index
   (lambda (y line)
     (map-with-index
      (lambda (x color)
        (proc x y color))
      line))
   block))

(define (set-cell! line x color)
  (if (= x 0)
    (set! (car line) color)
    (set-cell! (cdr line) (- x 1) color)))

(define (collect-groups block)
  (let ((done '())
        (grouped '()))
    (map-block
     (lambda (x y color)
       (unless (member (cons x y) done)
         (let1 group (collect-group done block x y)
           (set! done (append (cons (cons x y) done) group))
           (when (and (pair? group)
                      (>= (length group) 4))
             (set! grouped (cons group grouped))))))
     block)
    grouped))

(define (collect-group history block x y)
  (let1 color1 (color-at block x y)
    (if (member color1 '(#\G #\Y #\R))
      (fold
       (lambda (pair res)
         (cond
          ((member (cons x y) history)
           res)
          ((equal? color1 (color-at block (car pair) (cdr pair)))
           (let1 newres (cons (cons (car pair) (cdr pair)) res)
             (let1 newhist (cons (cons x y) history)
               (lset-union equal? 
                           newres
                           (collect-group newhist block (car pair) (cdr pair))))))
          (else
           res)))
       '()
       (list
        (cons (- x 1) y)
        (cons x (- y 1))
        (cons (+ x 1) y)
        (cons x (+ y 1))))
      '())))

(define (color-at block x y)
  (cond
   ((< y 0) #f)
   ((<= (length block) y)
    #f)
   (else
    (let1 line (list-ref block y)
      (cond
       ((< x 0) #f)
       ((<= (length line) x)
        #f)
       (else
        (list-ref line x)))))))

(define *spell-list*
  '(
    "ファイヤー!"
    "アイスストーム!"
    "ダイアキュート!"
    "ブレインダムド!"
    "じゅげむ!"
    "ばよえ〜ん!"))

(define (print-block block :optional (rensa #f))
  (when rensa
    (let1 spell (list-ref *spell-list* (- (min rensa (length *spell-list*)) 1))
      (print (format #`",|spell| ,|rensa| 連鎖"))))
  (map 
   (lambda (line)
     (print (apply string line)))
   block)
  (print (make-string 10 #\-)))

(define (read-block-from-file file)
  (with-input-from-file file
    (lambda ()
      (let loop ((res '())
                 (l (read-line)))
        (if (eof-object? l)
          (reverse res)
          (loop (cons (string->list l) res) (read-line)))))))
