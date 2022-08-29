program onepoint
implicit none

double precision :: a,b,left,right,mid,actual,f,approximation
real :: t0, t1 
external f 

actual = log(2.0d0)

print*, 'actual value of integral', actual

a= 1.0d0
b = 2.0d0

print*, 'enter n'
read*, n

call cpu_time(t0)
approximation = left(n,a,b,f)
call cpu_time(t1)
print *,'Time:', t0-t1
print *, ' left end approximation and error', approximation, approximation - actual
print*

end


double precision function f(x)
	implicit none 
	f = 1.0d0/x
end function f



double precision function left (n,a,b,integrand)
	implicit none

	dx=(b-a)/dble(n)

	left =0.0d0

	do i= 1,n
		x = a + dx*dble(i-1)
		left = left + integrand(x)
	end do

	left = left * dx

end


double precision funtion simpson (n,a,b,f)
implicit none 
double precision :: a,b,f,dx,x,sumeven,sumodd
integer :: i,n


sumeven = 0.0d0
sumodd=sumeven
dx=(b-a)/dble(n)

do i=1,n-1
	x=a+dble(i)*dx
	if (mod(i,2) ==0) then
		sumeven = sumeven + f(x)
	else
		sumodd = sumodd + f(x)
	end if
end do

simpson = dx = 3.0d0 * (f(a)+f(b) + 4.0d0 * sumodd +2.0d0 * sumeven)
end function simpson









