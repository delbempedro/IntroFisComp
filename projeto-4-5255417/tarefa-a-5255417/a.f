      program euler

      implicit real*8(a-h,o-z)

c     define o valor de pi
      realpi = 4.0d0*datan(1.0d0)

c     define o valores da gravidade, comprimento e massa
c     referentes ao pendulo
      g = 9.8d0
      r = 9.8d0
      m = 1.0d0

c     inicia o valor de theta e omega
      theta = realpi/6.0d0
      omega = 0.0d0

c     inicia o valor de theta analitico
      thetaanalitico = realpi/6.0d0

c     defini o "tempo" de analise, qual o espacamento de "tempo"
c     entre as incrementacoes em theta e omega e o tempo inical
      tempomax = 80.0d0
      deltat = 0.04d0
      tempo = 0.0d0

c     abre os arquivos onde serao salvas as informacoes
      open(unit=1,file="euler")
      open(unit=2,file="energia")
      open(unit=3,file="analitico")

c     inicia o loop de oscilacao
      do while(tempo.lt.tempomax)

c           define o tempo atual
            tempo = tempo + deltat

c           salva o theta antigo, antes de altera-lo
            thetaant = theta

c           incrementa theta e omega se acordo com o metodo de euler
            theta = theta + omega*deltat
            omega = omega - (g/r)*thetaant*deltat

c           calcula a energia
            energia = r*m*( ((omega**2)*r)/2.0d0 + g*(1+dcos(theta)) )

c           escreve o theta(tempo) atual no arquivo e se theta passar,
c           em modulo, de 2pi - faz a carrecao adequada
            if(abs(theta).ge.2.0d0*realpi) then
                  write(1,*)tempo,mod(theta,2.0d0*realpi)
            else
                  write(1,*)tempo,theta
            end if 

c           escreve o energia(tempo) atual no arquivo
            write(2,*)tempo,energia

c           escreve o theta(tempo) analiticoatual, no arquivo
            write(3,*)tempo,thetaanalitico*dcos(dsqrt(g/r)*tempo)

      end do

c     fecha os arquivos utilizados
      close(1)
      close(2)

      end program