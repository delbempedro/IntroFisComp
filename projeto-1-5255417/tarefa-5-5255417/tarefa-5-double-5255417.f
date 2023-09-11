      program doublelogaritmo

       implicit real*8 (a-h,o-z)

       write(*,*) 'Insira o valor de x:'
       read(*,*) x

       if(x.gt.1.0d0) then

         write(*,*) 'ln() do programa:', -aln(1.0d0/x)
         write(*,*) 'ln() intrínseco do fortran:',dlog(x)

       else

         write(*,*) 'ln() do programa:', aln(x)
         write(*,*) 'ln() intrínseco do fortran:',dlog(x)

       end if

       end program


       real*8 function aln (x)
       implicit real*8 (a-h,o-z)

       eprec = 1.0e-15
       n  = 1
       elemento = 1.0d0
       aln  = 0.0d0

       do while(abs(elemento).gt.eprec)
         
         elemento = ((1.0d0-x)**n)/n
         aln = aln - elemento
         n = n + 1

       end do

       return

       end function
