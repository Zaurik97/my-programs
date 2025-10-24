program main
    implicit none
    
    integer :: i, nbin, Ndati, ind, fuori
    real :: dx, xmin, xmax, dato
    real, dimension(:), allocatable :: histo
    real, dimension(100) :: randomValues
    character(len=10) :: file_name
    
    ! Numero di dati casuali da generare
    Ndati = 1000
    
    
    ! File per i dati
    file_name = 'DATI'
    
    ! Apertura del file di output
    open(unit=1, file=file_name, status='replace')
    
    ! Generazione dei dati casuali e scrittura su file
    do i = 1, Ndati
        dato = 0.0
        do ind = 1, 100
            call random_number(dato)
            randomValues(ind) = dato
        end do
        
        write(1, *) sum(randomValues)/100
    end do
    
    ! Chiusura del file
    close(1)
    
    ! Riapertura del file in lettura
    open(unit=1, file=file_name, status='old')
    
    ! Parametri per l'istogramma
    xmin = 0.0
    xmax = 1.0
    nbin = 100
    
    ! Allocazione dell'istogramma
    allocate(histo(nbin))
    histo = 0
    fuori = 0
    
    ! Calcolo delle frequenze
    dx = (xmax - xmin) / nbin
    
    do i = 1, Ndati
        read(1, *) dato
        ind = int((dato - xmin) / dx) + 1
        if (ind > nbin .or. ind < 1) then
            fuori = fuori + 1
        else
            histo(ind) = histo(ind) + 1
        end if
    end do
    
    ! Scrittura su file dell'istogramma
    open(unit=2, file='fort.2', status='replace')
    
    do i = 1, nbin
        write(2, *) i, xmin + (i - 1) * dx + dx / 2, real(histo(i)) / Ndati * nbin
    end do
    
    ! Chiusura del file dell'istogramma
    close(2)
    
    ! Stampa del numero di dati fuori intervallo
    write(*, '(A,I0)') "# fuori: ", fuori
    
    ! Deallocazione dell'istogramma
    deallocate(histo)
    
end program main