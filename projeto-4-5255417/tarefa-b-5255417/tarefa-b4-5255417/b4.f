      program amortecidoforcado

      implicit real*8(a-h,o-z)
      dimension amplitude(3)

c     define as amplitudes da forca
      amplitude(1) = 0
      amplitude(2) = 0.5d0
      amplitude(3) = 1.2d0

c     define o valor de pi
      pi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      r = 9.8d0
      m = 1.0d0

c     define a constante de amortecimento e a frequencia da forca
      gamma = 0.05d0
      frequencia = 2.0d0/3.0d0

c     inicia o valor de theta e omega
      theta = pi/6.0d0
      omega = 0.0d0

c     defini o "tempo" de analise, qual o espacamento de "tempo"
c     entre as incrementacoes em theta e omega
      tempomax = 100.0d0
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
                  omega = omega - (g/r)*dsin(theta)*deltat - gamma*omega
     4*deltat + amplitude(i)*dsin(frequencia*tempo)*deltat
                  theta = theta + omega*deltat

c                 escreve o theta(tempo) atual no arquivo e se theta passar,
c                 em modulo, de 2pi - faz a carrecao adequada
                  if(abs(theta).ge.2.0d0*pi) then
                        write(i,*)tempo,mod(theta,2.0d0*pi)
                  else
                        write(i,*)tempo,theta
                  end if 
c                 escreve o omega (theta) atual no arquivo
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