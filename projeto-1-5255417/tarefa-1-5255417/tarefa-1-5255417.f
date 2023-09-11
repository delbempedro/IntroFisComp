      program raizes

      real a,b,c,delta,raiz1,raiz2
      
      write(*,*) 'Insira os coeficientes a,b,c'
      
      read(*,*) a,b,c

      delta = (b**2 - 4*a*c)**0.5

      if (delta.gt.0) then
        raiz1 = (-b-delta)/(2*a)
        raiz2 = (-b+delta)/(2*a)
        write(*,*) 'Existem 2 raízes reais que são:',raiz1,raiz2
      else if (delta.eq.0) then
        raiz1 = -b/(2*a)
        write(*,*) 'Existe 1 raiz real que é:',raiz1
      else
        write(*,*) 'Não existem raízes reais'
      end if

      end program
