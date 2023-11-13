      program deltatheta

      implicit real*8(a-h,o-z)
      dimension amplitude(2)

c     define as amplitudes da forca
      amplitude(1) = 0.5d0
      amplitude(2) = 1.2d0

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

c     inicia o valor de theta e omega de acordo com a
c     solucao analitica
      theta1 = 1.0d0
      omega1 = 0.0d0
      theta2 = 1.001d0
      omega2 = 0.0d0

c     defini o "tempo" de analise, qual o espacamento de "tempo"
c     entre as incrementacoes em theta e omega
      tempomax = 200.0d0
      deltat = 0.04d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="amplitude0.5")
      open(unit=2,file="amplitude1.2")

c     define o loop para cada amplitude
      do i=1,2

c           (re)define o tempo como 0
            tempo = 0.0d0

c           inicia o loop de oscilacao
            do while(tempo.lt.tempomax)

c                 define o tempo atual
                  tempo = tempo + deltat

c                 incrementa theta1 e omega1 se acordo com o metodo
c                 de euler amortecido
                  omega1 = omega1 - (g/l)*dsin(theta1)*deltat - gamma*om
     1ega1*deltat + amplitude(i)*dsin(frequencia*tempo)*deltat
                  theta1 = theta1 + omega1*deltat

c                 incrementa theta2 e omega2 se acordo com o metodo
c                 de euler amortecido
                  omega2 = omega2 - (g/l)*dsin(theta2)*deltat - gamma*om
     2ega2*deltat + amplitude(i)*dsin(frequencia*tempo)*deltat
                  theta2 = theta2 + omega2*deltat

c                 escreve o theta(tempo), com escala semi-logaritmica,
c                 atual, no arquivo
                  write(i,*)tempo,dlog(theta1-theta2)

            end do

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)

      end program