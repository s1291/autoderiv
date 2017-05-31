
module autoderiv
Integer, parameter :: sp  = selected_real_kind(p = 6)
Integer, parameter :: dp  = selected_real_kind(p = 15)
Integer, parameter :: ldp = selected_real_kind(p = 31)
Integer, parameter :: pr = dp ! set the current precision to work with
!Define the derived data type *****************************************
TYPE :: AD
    REAL(pr) :: v, dv = 1.0
END TYPE
!**********************************************************************
! Overloading the plus (+) operator.
INTERFACE OPERATOR(+)
    MODULE PROCEDURE varPlusVar, varPlusNb, nbPlusVar
END INTERFACE
! OVerloading the minus (-) operator.
INTERFACE OPERATOR (-)
    MODULE PROCEDURE varMinusVar, varMinusNb, nbMinusVar
END INTERFACE
! Overloading the multiplication (*) operator.
INTERFACE OPERATOR (*)
    MODULE PROCEDURE varTimesVar, varTimesNb, nbTimesVar
END INTERFACE
! Overloading the division (/) operator.
INTERFACE OPERATOR (/)
    MODULE PROCEDURE varOverVar, varOverNb, nbOverVar
END INTERFACE
! Overloading the assignment (=).
INTERFACE ASSIGNMENT (=)
    MODULE PROCEDURE varEqvar, varEqNb
END INTERFACE
INTERFACE OPERATOR (**)
    MODULE PROCEDURE varPowVar, varPowNb, nbPowVar
END INTERFACE
! Overloading the exponential function EXP().
INTERFACE EXP
    MODULE PROCEDURE expOfVar
END INTERFACE
!Overloading the neperian logarithm function LOG().
INTERFACE  lOG
  MODULE PROCEDURE logOfVar
END INTERFACE
! Overloading the cosine function COS().
INTERFACE COS
    MODULE PROCEDURE cosOfVar
END INTERFACE
! Overloading the sine function SIN().
INTERFACE SIN
    MODULE PROCEDURE sinOfVar
END INTERFACE
! Overloading the tangent function TAN().
INTERFACE TAN
    MODULE PROCEDURE tanOfVar
END INTERFACE

CONTAINS
!For the sum operator --------------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varPlusVar(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X,Y
Z%v = X%V  + Y%V
Z%dv= X%dv + Y%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varPlusNb(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X
REAL(pr), INTENT(IN) :: Y
Z%v = X%V  + Y
Z%dv= X%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION nbPlusVar(X,Y) RESULT(Z)
REAL(pr), INTENT(IN) :: X
TYPE(AD), INTENT(IN) :: Y
Z%v = X  + Y%V
Z%dv= Y%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!-------------------------------------------------------------------------
!For the soustraction operator -------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varMinusVar(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X,Y
Z%v = X%V  - Y%V
Z%dv= X%dv - Y%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varMinusNb(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X
REAL(pr), INTENT(IN) :: Y
Z%v = X%V  - Y
Z%dv= X%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION nbMinusVar(X,Y) RESULT(Z)
REAL(pr), INTENT(IN) :: X
TYPE(AD), INTENT(IN) :: Y
Z%v = X  - Y%V
Z%dv= -Y%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!-------------------------------------------------------------------------
! For the multiplication operator.
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varTimesVar(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X,Y
Z%v = X%V * Y%V
Z%dv= X%dv*Y%v + X%v*Y%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varTimesNb(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X
REAL(pr), INTENT(IN) :: Y
Z%v = X%V  * Y
Z%dv= X%dv * Y
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION nbTimesVar(X,Y) RESULT(Z)
REAL(pr), INTENT(IN) :: X
TYPE(AD), INTENT(IN) :: Y
Z%v = X * Y%V
Z%dv= X *Y%dv
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!--------------------------------------------------------------------------
! For The division operator -----------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varOverVar(X,Y) RESULT(Z)
    TYPE(AD), INTENT(IN) :: X,Y
Z%v = X%V / Y%V
Z%dv= (X%dv*Y%v - X%v*Y%dv )/ (Y%dv)**2
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    TYPE(AD) FUNCTION varOverNb(X,Y) RESULT(Z)
    TYPE(AD), INTENT(IN) :: X
REAL(pr), INTENT(IN) :: Y
Z%v = X%V / Y
Z%dv= (X%dv)/Y
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION nbOverVar(X,Y) RESULT(Z)
    REAL(pr), INTENT(IN) :: X
    TYPE(AD), INTENT(IN) :: Y
Z%v = X / Y%V
Z%dv= -(X*Y%dv )/ (Y%dv)**2
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! For the assignment --------------------------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE varEqVar(X,Y)
    TYPE(AD), INTENT(OUT) :: X
    TYPE(AD), INTENT(IN)  :: Y
    X%v  = Y%v
    X%dv = Y%dv
END SUBROUTINE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SUBROUTINE varEqNb(X,Y)
    TYPE(AD), INTENT(OUT) :: X
    REAL(pr), INTENT(IN)  :: Y
    X%V  = Y
    X%dv = 0.0
END SUBROUTINE!~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! For the exponentiation (**) operator.----------------------------------------
!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varPowVar(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X,Y
Z%V  = (X%v)**(Y%v)
Z%dv = (Y%dv*LOG(X%v)+Y%v*X%dv/X%v)*((X%v)**Y%v)
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION varPowNb(X,Y) RESULT(Z)
TYPE(AD), INTENT(IN) :: X
REAL(pr), INTENT(IN) :: Y
Z%V  = (X%v)**Y
<<<<<<< HEAD
Z%dv = Y *(X%v)**(Y-1.0)*(X%dv)
=======
Z%dv = Y *(X%V)**(Y-1.0)*(X%dv)
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TYPE(AD) FUNCTION nbPowVar(X,Y) RESULT(Z)
REAL(pr), INTENT(IN) :: X
TYPE(AD), INTENT(IN) :: Y
Z%V  = X**(Y%v)
Z%dv = (Y%dv*LOG(X))*(X**Y%v)
END FUNCTION!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

!---------------------- FUNCTIONS----------------------------------------------
!************** EXP ******************
TYPE(AD) FUNCTION expOfVar(X) RESULT(Y)
TYPE(AD), INTENT(IN) :: X
Y%V = EXP(X%V)
Y%dv= X%dv*Exp(X%V)
END FUNCTION!*************************
!************** COS ******************
TYPE(AD) FUNCTION cosOfVar(X) RESULT(Y)
TYPE(AD), INTENT(IN) :: X
Y%V = COS(X%V)
Y%dv= -X%dv*SIN(X%V)
END FUNCTION!*************************
!************** SIN ******************
TYPE(AD) FUNCTION sinOfVar(X) RESULT(Y)
TYPE(AD), INTENT(IN) :: X
Y%V = SIN(X%V)
Y%dv= X%dv*COS(X%V)
END FUNCTION!*************************
!************** TAN ******************
TYPE(AD) FUNCTION tanOfVar(X) RESULT(Y)
TYPE(AD), INTENT(IN) :: X
Y%V = TAN(X%V)
Y%dv= X%dv/COS(X%V)**2
END FUNCTION!*************************
!************** LOG ******************
TYPE(AD) FUNCTION logOfVar(X) RESULT(Y)
TYPE(AD), INTENT(IN) :: X
Y%V = LOG(X%V)
Y%dv= X%dv/LOG(X%V)
END FUNCTION!*************************
end module
