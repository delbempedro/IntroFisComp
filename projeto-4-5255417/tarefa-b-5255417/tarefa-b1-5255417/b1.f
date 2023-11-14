      program periodopendulo

      implicit real*8(a-h,o-z)

c     define o valor de pi
      pi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      r = 9.8d0
      am = 1.0d0

c     defini qual o espacamento de "tempo" entre as
c     incrementacoes em theta
      deltat = 0.04d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="periodo")
      open(unit=2,file="periodo-analitico")

c     inicia o loop para thetas diferentes
      do i=1,30

c           inicia o valor de theta e omega
            theta = 0.1d0*i
            theta0 = theta
            omega = 0.0d0

c           (re)inicia o tempo e o pcontrolador
            tempo = 0.0d0
            pcontrolador = 0

c           inicia o loop de oscilacao ate que o pcontrolador
c           seja igual a 100
            do while(pcontrolador.lt.100)

c                 salva o valor de theta antes de altera-lo
                  omegaant = omega

c                 define o tempo atual
                  tempo = tempo + deltat

c                 incrementa theta e omega se acordo com o metodo de euler
                  omega = omega - (g/r)*dsin(theta)*deltat
                  theta = theta + omega*deltat

c                 incrementa um em pcontrolador se a velocidade mudar
                  if(omega*omegaant.lt.0.0d0)then
                        pcontrolador = pcontrolador + 1
                  end if

            end do

c           define o peiodo como tempo/50, pois ocorrerao 50 oscilacoes
            tempo = tempo/50.d0

c           escreve o theta(tempo) atual no arquivo e se theta passar,
c           em modulo, de 2pi - faz a carrecao adequada
            if(abs(theta).ge.2.0d0*pi) then
                  write(1,*)tempo,mod(theta0,2.0d0*pi)
            else
                  write(1,*)tempo,theta0
            end if

c           define o epson como 10% de theta0
            epson = theta0*0.000001d0

c           define o valor inicial de h
            h = (theta0-epson)/1000000.0d0
            hi = h

c           (re)inicia o periodo
            periodo = 0.0d0

c           define o do pra somar os valores da integral
            do while(h.le.(theta0-epson))

                  valor = b(h-hi,theta0,hi)
                  periodo = periodo + valor

                  h = h + 4*hi

            end do
            write(*,*)valor
            antperiodo = periodo
            periodo = 2.0d0*dsqrt(2.0d0*r/g)*(periodo+dsqrt(epson/dsin(t
     1heta0)))
            periodosemepson = 2.0d0*dsqrt(2.0d0*r/g)*dsqrt(periodo)
            termodoepson = 2.0d0*dsqrt(2.0d0*r/g)*dsqrt(epson/dsin(theta
     10))
            write(*,*)(tempo-termodoepson)/(2.0d0*dsqrt(2.0d0*r/g))-antp
     1eriodo
            write(*,*)"------------------------------------------------"
            write(2,*)periodo,theta0

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)

      end program

c     define a integral que define o periodo
      real*8 function f(theta,theta0,h)
      implicit real*8 (a-h,o-z)

      f = 1.0d0/dsqrt(dcos(theta+h)-dcos(theta0))

      end function

c     define a regra de Boole
      real*8 function b(x,x0,h)
      implicit real*8 (a-h,o-z)

      b = 2.0d0*h/45.0d0*(7.0d0*f(x,x0,0.0d0)+32.0d0*f(x,x0,h*1.0d0)+12.
     10d0*f(x,x0,h*2.0d0)+32.0d0*f(x,x0,3.0d0*h)+7.0d0*f(x,x0,4.0d0*h))

      end function