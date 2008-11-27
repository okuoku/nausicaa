;;; Copyright (c) 2007 Christian Sloma (port to R6RS)
;;; Copyright (c) 1992 Xerox Corporation.  All Rights Reserved.
;;;
;;; Use,   reproduction,  and  preparation   of  derivative   works  are
;;; permitted.  Any copy of this software or of any derivative work must
;;; include  the  above  copyright  notice of  Xerox  Corporation,  this
;;; paragraph and the  one after it.  Any distribution  of this software
;;; or derivative  works must comply  with all applicable  United States
;;; export control laws.
;;;
;;; This  software  is  made  available  AS IS,  and  XEROX  CORPORATION
;;; DISCLAIMS  ALL  WARRANTIES, EXPRESS  OR  IMPLIED, INCLUDING  WITHOUT
;;; LIMITATION THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;; A  PARTICULAR  PURPOSE,  AND  NOTWITHSTANDING  ANY  OTHER  PROVISION
;;; CONTAINED  HEREIN,  ANY LIABILITY  FOR  DAMAGES  RESULTING FROM  THE
;;; SOFTWARE  OR ITS  USE IS  EXPRESSLY DISCLAIMED,  WHETHER  ARISING IN
;;; CONTRACT, TORT  (INCLUDING NEGLIGENCE) OR STRICT  LIABILITY, EVEN IF
;;; XEROX CORPORATION IS ADVISED OF THE POSSIBILITY OF SUCH DAMAGES.

(library (clos private core-class-layout)

  (export core-class-slot-names
          core-class-slot-count)

  (import (rnrs))

  (define core-class-slot-names
    '(
      ;;The following  closure types are  values in the slots.   Each of
      ;;them will have the slot identifier in its closure's environment.
      ;;
      ;; initializer-fn : instance         -> unspecified
      ;;	Given an instance: intialise a slot.
      ;;
      ;; getter-fn      : instance         -> value
      ;;	Given an instance: return the value of a slot.
      ;;
      ;; setter-fn      : instance * value -> unspecified
      ;;	Given an instance and a value: store the value
      ;;	in a slot.
      ;;

      ;;List of direct super classes for this class.
      direct-supers		;;; (class ...)

      ;;List of direct slots for this class.
      direct-slots		;;; ((slot-name slot-option ...) ...)

      ;;List of this  class and all super-classes in  the order in which
      ;;they are searched for methods.
      precedence-list		;;; (class ...)

      ;;List of slots (direct slots + slots of super-classes).
      slots			;;; ((slot-name slot-option ...) ...)

      ;;How many fields must be allocated for instances of this class.
      number-of-fields		;;; integer

      ;;Initializers for the fields.
      field-initializers	;;; (initializer-fn ...)

      ;;Getter  and  setter  functions  for  the  slots  of  this  class
      ;;searchable by slot-name.
      getters-and-setters	;;; ((slot-name getter-fn setter-fn) ...)

      ;;Name of the class.
      definition-name ;; symbol or #f

      )) ;; core-class-slot-names

  (define core-class-slot-count (length core-class-slot-names))

  ) ;; library (clos private core-class-layout)

