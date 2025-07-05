program gaussian_integral
    implicit none
    ! Evaluate the integral of a 1D Gaussian function numerically using trapezoid rule
    ! f(x) = exp(-(x-mu)^2/(2*sigma^2))
    ! Input variables
    real :: mu, sigma ! Gaussian parameters
    integer :: bins ! Number of trapezoids
    real :: upper, lower ! Bounds of integration
    real :: integral
    ! Derivative variables

    print *, "Enter mean"
    read(*,*) mu
    print *, "Enter standard deviation"
    read(*,*) sigma
    if (sigma <= 0) then
        print *, "Standard deviation is not a positive number"
        stop
    endif

    print *, "Enter lower and upper bounds of integration"
    read(*,*) lower, upper
    ! print *, "Enter upper bound of integration"
    ! read(*,*) upper
    if (upper < lower) then
        print *, "Upper bound must be greater than lower bound"
        stop
    endif

    print *, "Enter number of trapezoids -- the higher the more precise"
    read(*,*) bins

    call compute_gaussian_integral()
    print *, "Integral of the Gaussian function is ", integral

contains
    subroutine compute_gaussian_integral()
        real :: bin_width
        real :: width
        integer :: i

        width = upper - lower
        bin_width = width / bins
        integral = 0.0
        do i = 1, bins
            integral = integral + trapezoid(gaussian(lower + bin_width * i - 1), gaussian(lower + bin_width * i), bin_width)
        enddo
    end subroutine compute_gaussian_integral

    function trapezoid(width1, width2, height) result(area)
        real :: width1, width2, height
        real :: area

        area = (width1 + width2) * height / 2
    end function

    function gaussian(x) result(y)
        real :: x ! input
        real :: y ! output

        y = exp(-((x - mu)**2) / (2 * sigma ** 2))
    end function

end program gaussian_integral

