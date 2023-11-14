      program periodoangulopequeno

      implicit real*8(a-h,o-z)

c     define o valor de pi
      pi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      r = 9.8d0
      m = 1.0d0

c     defini qual o espacamento de "tempo" entre as
c     incrementacoes em theta
      deltat = 0.04d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="periodo")
      open(unit=2,file="periodo-analitico")

c     inicia o loop para thetas diferentes
      do i=1,20

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

c           define o epson
            epson = 0.1d0

c           define o valor inicial de h
            h = (theta0-epson)/12.0d0
            hi = h

c           (re)calcula o periodo
            periodo = 2.0d0*pi*dsqrt(r/g)*(1+(theta0**2.0d0)/16.0d0)

            write(2,*)periodo,theta0

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)

      end program