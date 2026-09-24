module heat_params
	implicit none
	real, parameter :: L = 1.0
	integer, parameter :: nx = 21
end module heat_params

program thermal_conductivity_1d
  use heat_params
  implicit none

  integer :: nt
  real :: alpha, dt, dx
  real, parameter :: pi = 3.14159

  real, allocatable :: T(:)

  allocate(T(nx))
  T = 0.0
  T(1) = 100.0
  T(nx) = 0.0
  dx = L / (nx - 1)
  alpha = 0.01

  print *, "T(1) = ", T(1)
  print *, "T(nx) = ", T(nx)
  print *, "dx = ", dx
  print *, "alpha = ", alpha

end program thermal_conductivity_1d
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

