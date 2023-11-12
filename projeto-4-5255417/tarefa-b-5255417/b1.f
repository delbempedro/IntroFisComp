      program periodopendulo

      implicit real*8(a-h,o-z)

c     define o valor de pi
      realpi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      l = 9.8d0
      m = 1.0d0

c     define, arbitrariamente, as condicoes iniciais
      theta0 = 1.0d0
      fase = 0.0d0

c     defini qual o espacamento de "tempo" entre as
c     incrementacoes em theta
      deltat = 0.04d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="periodo")

c     inicia o loop para thetas diferentes
      do i=1,10

c     define o theta0
      theta0 = i*10

c     inicia o valor de theta e omega de acordo com a
c     solucao analitica
      theta = theta0*dsin(fase)
      omega = theta0*((g/l)**0.5d0)*dcos(fase)

c     (re)inicia o tempo
      tempo = 0.0d0

c     efetua a primeira iteracao

c           salva o valor de theta antes de altera-lo
            thetaant = theta
c           define o tempo atual
            tempo = tempo + deltat
c           incrementa theta e omega se acordo com o metodo de euler
            omega = omega - (g/l)*theta*deltat
            theta = theta + omega*deltat

c     fim da primeira iteracao

c           inicia o loop de oscilacao ate que theta seja zerado
            do while(theta/thetaant.lt.0.0d0)

c                 salva o valor de theta antes de altera-lo
                  thetaant = theta

c                 define o tempo atual
                  tempo = tempo + deltat

c                 incrementa theta e omega se acordo com o metodo de euler
                  omega = omega - (g/l)*theta*deltat
                  theta = theta + omega*deltat

c                 se theta passar, em modulo, de 2pi - faz a carrecao adequada
                  if(abs(theta).ge.2.0d0*realpi) then
                        theta = mod(theta,2.0d0*realpi)
                  end if

                  write(*,*)theta

            end do

c           escreve periodo(theta) atual no arquivo
            write(1,*)tempo,theta0

      end do

c     fecha os arquivos utilizados
      close(1)

      end program