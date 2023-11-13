      program periodopendulo

      implicit real*8(a-h,o-z)

c     define o valor de pi
      realpi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      l = 9.8d0
      m = 1.0d0

c     defini qual o espacamento de "tempo" entre as
c     incrementacoes em theta
      deltat = 0.04d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="periodo")
      open(unit=2,file="periodo-analitico")
      open(unit=3,file="tempos")

c     inicia o loop para thetas diferentes
      do i=1,6

c           inicia o valor de theta e omega
            theta = realpi*i/6.0d0
            theta0 = theta
            omega = 0.0d0

c           (re)inicia o tempo e o pcontrolador
            tempo = 0.0d0
            pcontrolador = 0

c           inicia o loop de oscilacao ate que o pcontrolador
c           seja igual a 11
            do while(pcontrolador.lt.11)

c                 salva o valor de theta antes de altera-lo
                  omegaant = omega

c                 define o tempo atual
                  tempo = tempo + deltat

c                 incrementa theta e omega se acordo com o metodo de euler
                  omega = omega - (g/l)*dsin(theta)*deltat
                  theta = theta + omega*deltat

c                 incrementa um em pcontrolador se a velocidade mudar
                  if(omega*omegaant.lt.0.0d0)then
                        pcontrolador = pcontrolador + 1
                  end if
                  write(3,*)tempo,theta

            end do

c           define o peiodo como tempo/5, pois ocorrerao 5 oscilacoes
            tempo = tempo/5.0d0

c           escreve o theta(tempo) atual no arquivo e se theta passar,
c           em modulo, de 2pi - faz a carrecao adequada
            if(abs(theta).ge.2.0d0*realpi) then
                  write(1,*)tempo,mod(theta0,2.0d0*realpi)
            else
                  write(1,*)tempo,theta0
            end if

c           define o epson
            epson = 0.001d0

c           define o valor inicial de h
            h = (theta0-epson)-(-theta0+epson)/2000.0d0
            hi = h

c           (re)inicia o periodo
            periodo = 0.0d0

c           define o do pra somar os valores da integral
            do while(h.lt.1.0d0)

                  periodo = periodo + b(h,theta0,hi)

                  h = h + 4*hi

            end do
            periodo = dsqrt(2.0d0*g/l)*( periodo + dsqrt(epson)/dsqrt(ds
     1in(theta0)) )

            write(2,*)periodo,theta0

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)
      close(3)

      end program

c     define a integral que define o periodo
      real*8 function f(theta,theta0,h)
      implicit real*8 (a-h,o-z)

      f = 1.0d0/dsqrt(dcos(theta+h)-dcos(theta0))

      end function

c     define a regra de Simpson
      real*8 function s(x,x0,h)
      implicit real*8 (a-h,o-z)

      s = h/3.0d0*(f(x,x0,-h)+4.0d0*f(x,x0,0.0d0)+f(x,x0,h))

      end function

c     define a regra de Boole
      real*8 function b(x,x0,h)
      implicit real*8 (a-h,o-z)

      b = 2.0d0*h/45.0d0*(7.0d0*f(x,x0,0.0d0)+32.0d0*f(x,x0,h*1.0d0)+12.
     10d0*f(x,x0,h*2.0d0)+32.0d0*f(x,x0,3.0d0*h)+7.0d0*f(x,x0,4.0d0*h))

      end function