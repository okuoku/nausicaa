(library (srfi private implementation-features)
  (export
    OS-features
    implementation-features)
  (import
    (ikarus))
  
  (define OS-features
    (let*          ;; TODO? more
        ([alist '(["linux" linux posix]
                  ["solaris" solaris posix]
                  ["darwin" darwin posix]
                  ["cygwin" cygwin posix] ;; correct?
                  ["gnu" gnu])]
         [hi (host-info)]
         [hi-len (string-length hi)]
         [contains? 
          (lambda (str)
            (define str-len (string-length str))
            (and (>= hi-len str-len)
                 (let loop ([i 0])
                   (and (<= (+ i str-len) hi-len)
                        (or (string-ci=? str (substring hi i (+ i str-len)))
                            (loop (+ 1 i)))))))]
         [features 
          (apply append
                 (map cdr (filter (lambda (x) (contains? (car x))) 
                                  alist)))])          
      (unless (positive? (length features))
        (error "(library (srfi private implementation-features))"
               "Unknown host-info. Please report your's to maintainer person."
               hi))
      features))
  
  (define implementation-features
    '(ikarus))
)
