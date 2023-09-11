      program dimensoesesferas
      
      implicit real*8 (a-h,o-z)

      write(*,*) 'Insira o raio da esfera:'
      read(*,*) r

      write(*,*) 'Insira a dimensão da esfera:'
      read(*,*) id

      pi = 4d0*datan(1d0)
      
      do i=2, id
        
        d = i

        v = pi**(d/2d0 )/gama(d/2.0+1.0)*( r**(d) )
      
        open(unit=1,file='dimensoes-esferas')
        
        write(1,*) i,v

      end do

      close(1)

      end program

      real*8 function gama(x)
      implicit real*8 (a-h,o-z)
      
      pi = 4d0*datan(1d0) 
      nx = x     
      if (nx/2.0.eq.x/2.0) then

        gama = 1.0

      else

        gama = sqrt(pi)

      end if

      do while(x.gt.1.0)
        
        x = x-1.0
        gama = gama*x

      end do

      end function
