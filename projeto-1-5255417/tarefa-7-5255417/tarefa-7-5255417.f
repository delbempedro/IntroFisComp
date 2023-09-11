      program areacirculo
      
      real anormal
      integer*16 itotal

      write(*,*) 'Quantas dimensões tem esfera?'
      read(*,*) id
      
      identro = 0
      itotal = 1000000000

      do i=1, itotal
        
        raio = anorma(id)

        if (raio.lt.1) then
          identro = identro + 1

        end if

      end do

      dentro = identro
      total = itotal

      volume = dentro/total*(2.**id)

      write(*,10) 'A esfera de ',id,' dimensões tem volume ',volume
10    format(a12,i1.1,a23,f6.4)

      end program

      real function anorma(id)
       
      soma = 0

      do i=1, id
 
        soma = soma + rand(0)**2

      end do

      anorma = sqrt(soma)
      return

      end function
