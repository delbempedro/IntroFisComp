      program integral

      implicit real*8 (a-h,o-z)
      dimension val(10)
        
c     define os valores de h que serao utilizados
      val(1) = 1.0d0/12.0d0
      val(2) = 1.0d0/24.0d0
      val(3) = 1.0d0/48.0d0
      val(4) = 1.0d0/96.0d0
      val(5) = 1.0d0/192.0d0
      val(6) = 1.0d0/384.0d0
      val(7) = 1.0d0/768.0d0
      val(8) = 1.0d0/1536.0d0
      val(9) = 1.0d0/3027.0d0
      val(10) = 1.0d0/6144.0d0

c     abre o arquivo de saida
      open(unit=1,file='saida-5255417')
      
c     escreve o cabecalho da tabela
      write(1,*)'|          h                trapezio             Simpso
     1n  Boole |'
      
c     loop que imprime os valores da tabela das integrais para cada h
      do i=1,10
      
c       define como v o valor de h para o loop atual
        v = val(i)
      
c       define o valor real da integral
        ri = 0.01561624581

c       define o valor inicial das integrais para cada metodo
        trap = 0.0d0
        simp = 0.0d0
        boll = 0.0d0
      
c       define o valor inicial de h
        h = val(i)

c       define o do pra somar os valores da integral de -h ate h para o
c       metodo do trapezio e de simpson
        do while(h.lt.1.0d0)

          trap = trap + t(h,val(i))
          simp = simp + s(h,val(i))

          h = h + 2*val(i)

        end do

c       define o valor inicial de h
        h = val(i)

c       define o do pra somar os valores da integral de -2h ate 2h para o
c       metodo de boole
        do while(h.lt.1.0d0)

          bool = bool + b(x,h)

          h = h + 4*val(i)

        end do

c       escreve os valores de cada integral para o valor corrente de h
        write(1,1) v,abs(trap-ri),abs(simp-ri),abs(bool-ri)
      
      end do
      
c     escreve o fim da tabela
      write(1,*)'EXATO: 0.01561624581'
      
c     fecha o arquivo de saida
      close(1)
      
c     formata as escritas
1     format(4('|',f20.10),'|')
      
      end program

c     define a integral de x
      real*8 function f(x,h)
      implicit real*8 (a-h,o-z)

      pi = 4.0d0*datan(1.0d0)

      f = dexp( -(x+h) )*dcos( 2.0d0*pi*(x+h) )

      end function

c     define a regra do trapezio
      real*8 function t(x,h)
      implicit real*8 (a-h,o-z)

      t = h/2.0d0*(f(x,-h)+2.0d0*f(x,0.0d0)+f(x,h))

      end function

c     define a regra de Simpson
      real*8 function s(x,h)
      implicit real*8 (a-h,o-z)

      s = h/3.0d0*(f(x,-h)+4.0d0*f(x,0.0d0)+f(x,h))

      end function

c     define a regra de Boole
      real*8 function b(x,h)
      implicit real*8 (a-h,o-z)

      b = 2.0d0*h/45.0d0*(7*f(x,0.0d0)+32.0d0*f(x,h)+12.0d0*f(x,2.0d0*h
     1)+32.0d0*f(x,3.0d0*h)+7*f(x,4.0d0*h))

      end function
