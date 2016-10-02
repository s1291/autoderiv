

program main
    USE autoderiv
    Implicit none
    TYPE(AD) :: x, y
    TYPE(AD), EXTERNAL :: F
    x%v = 1.0
    y = F(x)
   WRITE(*,*) "f(x) = x^2 + 3/x"
   WRITE(*,'(X,A,F0.7)') "x  = ", x%v
   WRITE(*,10) x%v, y%v
   WRITE(*,11) x%v, y%dv
10 FORMAT(X, 'f(', F0.1, ') = ', 2F0.7)
11 FORMAT(X, "f'(", F0.1, ') = ', 2F0.7)
end program main
FUNCTION F(X)
    USE autoderiv
    TYPE(AD) :: F
    TYPE(AD) :: X
F = X**2.0_pr + 3.0_pr/X
END FUNCTION
