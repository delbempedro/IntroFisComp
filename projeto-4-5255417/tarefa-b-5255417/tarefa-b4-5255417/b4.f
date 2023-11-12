      program amortecidoforcado

      implicit real*8(a-h,o-z)
      dimension amplitude(3)

c     define as amplitudes da forca
      amplitude(1) = 0
      amplitude(2) = 0.5d0
      amplitude(3) = 1.2d0

c     define o valor de pi
      realpi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      l = 9.8d0
      m = 1.0d0

c     define a constante de amortecimento e a frequencia da forca
      gamma = 0.05d0
      frequencia = 2.0d0/3.0d0

c     define, arbitrariamente, as condicoes iniciais
      theta0 = 1.0d0
      fase = 0.0d0

c     inicia o valor de theta e omega de acordo com a
c     solucao analitica
      theta = theta0*dsin(fase)
      omega = theta0*((g/l)**0.5d0)*dcos(fase)

c     defini o "tempo" de analise, qual o espacamento de "tempo"
c     entre as incrementacoes em theta e omega
      tempomax = 200.0d0
      deltat = 0.04d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="theta-livre")
      open(unit=4,file="omega-livre")
      open(unit=2,file="theta-forcado0.5")
      open(unit=5,file="omega-forcado0.5")
      open(unit=3,file="theta-forcado1.2")
      open(unit=6,file="omega-forcado1.2")

c     define o loop para cada amplitude
      do i=1,3

c           (re)define o tempo como 0
            tempo = 0.0d0

c           inicia o loop de oscilacao
            do while(tempo.lt.tempomax)

c                 define o tempo atual
                  tempo = tempo + deltat

c                 incrementa theta e omega se acordo com o metodo
c                 de euler amortecido
                  omega = omega - (g/l)*theta*deltat - gamma*omega*delta
     4t + amplitude(i)*dsin(frequencia*tempo)*deltat
                  theta = theta + omega*deltat

c                 se theta passar, em modulo, de 2pi - faz a carrecao adequada
                  if(abs(theta).ge.2.0d0*realpi) then
                        theta = mod(theta,2.0d0*realpi)
                  end if

c                 escreve o theta(tempo) e omega (theta)
c                 atual no arquivo
                  write(i,*)tempo,theta
                  write(i+3,*)tempo,omega

            end do

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)
      close(3)
      close(4)
      close(5)
      close(6)

      end program