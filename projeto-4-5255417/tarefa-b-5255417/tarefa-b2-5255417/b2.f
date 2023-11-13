      program periodoangulopequeno

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
c           seja igual a 10
            do while(pcontrolador.lt.10)

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
            epson = 0.1d0

c           define o valor inicial de h
            h = (theta0-epson)/12.0d0
            hi = h

c           (re)calcula o periodo
            periodo = 2.0d0*realpi*dsqrt(l/g)*(1+(theta0**2.0d0)/16.0d0)

            write(2,*)periodo,theta0

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)

      end program