program heat1d
  implicit none

  !-----------------------------------------------------------------
  ! Одномерная нестационарная теплопроводность, явная схема FTCS:
  !
  !     dT/dt = alpha * d2T/dx2 ,   x in [0, L]
  !
  ! Граничные условия (Dirichlet): T(0) = Thot, T(L) = Tcold
  !-----------------------------------------------------------------

  integer, parameter :: nx    = 21       ! число узлов сетки
  integer, parameter :: nt    = 2000     ! число шагов по времени
  real,    parameter :: L     = 1.0      ! длина стержня, м
  real,    parameter :: alpha = 1.0e-4   ! коэфф. температуропроводности, м^2/с
  real,    parameter :: Thot  = 100.0    ! температура на левом конце
  real,    parameter :: Tcold = 0.0      ! температура на правом конце

  real, dimension(nx) :: T, Tnew
  real :: dx, dt, r
  integer :: i, n

  ! --- шаг сетки по пространству ---
  dx = L / real(nx - 1)

  ! --- шаг по времени из условия устойчивости FTCS: r = alpha*dt/dx^2 <= 0.5 ---
  r  = 0.4
  dt = r * dx**2 / alpha

  ! --- начальное условие: стержень холодный, кроме заданных концов ---
  T      = 0.0
  T(1)   = Thot
  T(nx)  = Tcold

  ! --- основной цикл по времени ---
  do n = 1, nt
    do i = 2, nx - 1
      Tnew(i) = T(i) + r * (T(i+1) - 2.0*T(i) + T(i-1))
    end do

    Tnew(1)  = Thot     ! граничные условия неизменны во времени
    Tnew(nx) = Tcold

    T = Tnew
  end do

  ! --- вывод результата ---
  print *, "r =", r, " dt =", dt, " с;  шагов:", nt
  print *, "   x,        T(x)"
  do i = 1, nx
    print '(F8.4, 4X, F10.4)', (i-1)*dx, T(i)
  end do

end program heat1d
