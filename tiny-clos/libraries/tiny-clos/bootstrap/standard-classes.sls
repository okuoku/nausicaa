; *************************************************************************
; Copyright (c) 1992 Xerox Corporation.  
; All Rights Reserved.  
;
; Use, reproduction, and preparation of derivative works are permitted.
; Any copy of this software or of any derivative work must include the
; above copyright notice of Xerox Corporation, this paragraph and the
; one after it.  Any distribution of this software or derivative works
; must comply with all applicable United States export control laws.
;
; This software is made available AS IS, and XEROX CORPORATION DISCLAIMS
; ALL WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE
; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
; PURPOSE, AND NOTWITHSTANDING ANY OTHER PROVISION CONTAINED HEREIN, ANY
; LIABILITY FOR DAMAGES RESULTING FROM THE SOFTWARE OR ITS USE IS
; EXPRESSLY DISCLAIMED, WHETHER ARISING IN CONTRACT, TORT (INCLUDING
; NEGLIGENCE) OR STRICT LIABILITY, EVEN IF XEROX CORPORATION IS ADVISED
; OF THE POSSIBILITY OF SUCH DAMAGES.
; *************************************************************************
;
; port to R6RS -- 2007 Christian Sloma
; 

(library (clos bootstrap standard-classes)

  (export <class>
          <top>
          <object>
          <procedure-class>
          <entity-class>
          <generic>
          <method>
          bootstrap-make)

  (import (rnrs)
          (clos private allocation)
          (clos private core-class-layout)
          (clos slot-access)
          (clos introspection)
          (clos std-protocols make)
          (clos std-protocols allocate-instance)
          (clos std-protocols initialize)
          (clos std-protocols class-initialization))

  (define <class>  
    (really-allocate-instance 'ignore core-class-slot-count))

  (define <top>    
    (really-allocate-instance <class> core-class-slot-count))

  (define <object> 
    (really-allocate-instance <class> core-class-slot-count))

  (define bootstrap-initialize
    (begin 

      (set-instance-class-to-self! <class>)
      (register-class-of-classes!  <class>)

      (lambda (inst init-args)
        (let ((class (class-of inst)))
          (cond
            ((or (eq? class <class>)
                 (eq? class <entity-class>))
             (class-initialize inst init-args
                               class-compute-precedence-list
                               class-compute-slots
                               class-compute-getter-and-setter))
            ((eq? class <generic>)
             (generic-initialize inst init-args))
            ((eq? class <method>)
             (method-initialize inst init-args))
            (else
             (error 'bootstrap-initialize 
                    "cannot initialize instance of class ~a" class)))))))

  (define bootstrap-allocate-instance 
    (begin 

      (bootstrap-initialize <top>
        (list 'direct-supers (list)
              'direct-slots  (list)))

      (bootstrap-initialize <object>
        (list 'direct-supers (list <top>)
              'direct-slots  (list)))

      (bootstrap-initialize <class>
        (list 'direct-supers (list <object>)
              'direct-slots  core-class-slot-names))

      (lambda (class)
        (let ((class-of-class (class-of class)))
          (cond
            ((eq? class-of-class <class>)
             (class-allocate-instance class))
            ((eq? class-of-class <entity-class>)
             (entity-class-allocate-instance class))
            (else
             (error 'bootstrap-allocate-instance
                    "cannot allocate instance for class ~a" class)))))))

  (define (bootstrap-make class . init-args)
    (class-make class init-args
                bootstrap-allocate-instance
                bootstrap-initialize))

  (define <procedure-class>
    (bootstrap-make <class>
      'direct-supers (list <class>)
      'direct-slots  (list)))

  (define <entity-class>
    (bootstrap-make <class>
      'direct-supers (list <procedure-class>)
      'direct-slots  (list)))

  (define <generic>
    (bootstrap-make <entity-class>
      'direct-supers (list <object>)
      'direct-slots  (list 'methods)))

  (define <method>
    (bootstrap-make <class>
      'direct-supers (list <object>)
      'direct-slots  (list 'specializers 
                           'procedure)))

  ) ;; library (clos bootstrap standard-classes)
