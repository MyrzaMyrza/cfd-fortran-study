program 1d_thermal_conductivity
  implicit none
integer :: nx, nt   !целые цисла
real :: alpha, dt   !вещественные числа
real, parameter :: pi = 3.14159   !константа, менять нельзя

real, allocate :: T(:)   !массив с неопределенным размером

allocate(T(nx))   !память под nx элементов

module heat_params
	implicit none
	real, parameter :: L = 1.0
	integer, parameter :: nx = 21
end module heat_params

program main
	use heat_params
	implicit none
end program main

T(1) = 100.0
T(nx) = 0.0

end program 1d_thermal_conductivity

 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
  ! Initial condition: u(x,0) = sin(pi*x)
  do i = 1, nx
    u(i) = sin(pi * (i - 1) * dx)
  end do

  ! Time-stepping loop
  do n = 1, nt
    ! Update interior points using finite difference method
    do i = 2, nx - 1
      u_new(i) = u(i) + alpha * dt / dx**2 * (u(i + 1) - 2 * u(i) + u(i - 1))
    end do

    ! Apply boundary conditions (Dirichlet)
    u_new(1) = u(1)
    u_new(nx) = u(nx)

    ! Update the solution for the next time step
    u = u_new
  end do

  ! Output the final temperature distribution
  print *, "Final temperature distribution:"
  do i = 1, nx
    print *, (i - 1) * dx, u(i)
  end do

end program 1d_thermal_conductivity
