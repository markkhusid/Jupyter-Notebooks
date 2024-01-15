!
! The Sieve of Eratosthenes
!
module m_eratos
    implicit none
contains
    integer function nprimes(n) 
        integer, intent(in) :: n
        logical, allocatable :: tab(:)
        integer :: i
        allocate(tab(2:n), source = .true.)
        do i = 2, int(sqrt(real(n)))
            if (tab(i)) tab(i**2::i) = .false.
        end do
        nprimes = count(tab)
    end function nprimes    
end module m_eratos
