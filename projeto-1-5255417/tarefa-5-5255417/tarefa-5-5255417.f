       program logaritmo

       write(*,*) 'Insira o valor de x:'
       read(*,*) x

       if(x.gt.1.) then

         write(*,*) 'ln() do programa:', -aln(1./x)
         write(*,*) 'ln() intrínseco do fortran:',log(x)

       else

         write(*,*) 'ln() do programa:', aln(x)
         write(*,*) 'ln() intrínseco do fortran:',log(x)
  
       end if

       end program


       real function aln (x)

       eprec = 1e-5
       n  = 1.
       elemento = 1.
       aln  = 0.

       do while(abs(elemento).gt.eprec)
         
         elemento = ((1.-x)**n)/n
         aln = aln - elemento
         n = n + 1

       end do

       return

       end function

