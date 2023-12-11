      program Ncorpos

      implicit real*8(a-h,o-z)

c     declara os vetores que contem os raios e velocidades
c     de cada planeta
      dimension raios(11:19),raiosatual(11:19,2),velocidades(11:19),raio
     1santigo(11:19,2),raiosproximo(11:19,2)

c     define o valor de pi
      pi = 4.0d0*datan(1.0d0)

c     define o controlador das velocidades
      vel = 2.0d0

c     preenche os vetores de raio e velocidade
c     com o valores correspondentes
      raios(11) = 0.39d0
      raios(12) = 0.72d0
      raios(13) = 1.0d0
      raios(14) = 1.52d0
      raios(15) = 5.20d0
      raios(16) = 9.24d0
      raios(17) = 19.19d0
      raios(18) = 30.06d0
      raios(19) = 39.53d0
      velocidades(11) = vel*pi/dsqrt(raios(11))
      velocidades(12) = vel*pi/dsqrt(raios(12))
      velocidades(13) = vel*pi/dsqrt(raios(13))
      velocidades(14) = vel*pi/dsqrt(raios(14))
      velocidades(15) = vel*pi/dsqrt(raios(15))
      velocidades(16) = vel*pi/dsqrt(raios(16))
      velocidades(17) = vel*pi/dsqrt(raios(17))
      velocidades(18) = vel*pi/dsqrt(raios(18))
      velocidades(19) = vel*pi/dsqrt(raios(19))

c     define a resultante da multiplicacao da constante gravitacional
c     pela massa do sol
      gms = 4.0d0*(pi**2.0d0)

c     abre os arquivos das coordenadas de cada planeta
      open(unit=11,file='mercurio-b3-2')
      open(unit=12,file='venus-b3-2')
      open(unit=13,file='terra-b3-2')
      open(unit=14,file='marte-b3-2')
      open(unit=15,file='jupiter-b3-2')
      open(unit=16,file='saturno-b3-2')
      open(unit=17,file='urano-b3-2')
      open(unit=18,file='netuno-b3-2')
      open(unit=19,file='plutao-b3-2')

c     realiza a primeira interação de cada planeta
      do i=11,19

c           define as coordenadas iniciais
            xi = raios(i)
            yi = 0.0d0

c           define o raio inicial
            raio = dsqrt(xi**2.0d0 + yi**2.0d0)

c           define a velocidade em cada coordenada
            xvelocidade = 0.0d0
            yvelocidade = velocidades(i)

c           define o intervalo de tempo utilizado
c           de acordo com o raio do planeta
            deltat = 0.00001d0

c           define o tempo atual
            tempo = 0.0d0

c           escreve no arquivo a coordenada inicial
            write(i,*)xi,yi

c           realiza a interacao inicial em ambas as coordenadas
            raiosatual(i,1) = xi + xvelocidade*deltat
            raiosatual(i,2) = yi + yvelocidade*deltat

c           salva os valores antigos das coordenadas
            raiosantigo(i,1) = xi
            raiosantigo(i,2) = yi

      end do


c     realiza interacoes nas coordenadas
c     ate o tempo ser igual a 50 anos
      do while(tempo.lt.2.0d0)

c           realiza as interações de todos os planetas
            do i=11,19

c                 calcula o raio atual
                  raio = dsqrt(raiosatual(i,1)**2.0d0 + raiosatual(i,2)*
     1*2.0d0)

c                 escreve no arquivo a coordenada atual
                  write(i,*)raiosatual(i,1),raiosatual(i,2)

c                 realiza um interacao em cada coordenada
                  raiosproximo(i,1) = 2.0d0*raiosatual(i,1) - raiosantig
     1o(i,1) - gms*raiosatual(i,1)*(deltat**2.0d0)/(raio**3.0d0)
                  raiosproximo(i,2) = 2.0d0*raiosatual(i,2) - raiosantig
     1o(i,2) - gms*raiosatual(i,2)*(deltat**2.0d0)/(raio**3.0d0)

c                 salva os valores das coordenadas antes de altera-los
                  raiosantigo(i,1) = raiosatual(i,1)
                  raiosantigo(i,2) = raiosatual(i,2)

c                 atualiza os valores das coordenadas
                  raiosatual(i,1) = raiosproximo(i,1)
                  raiosatual(i,2) = raiosproximo(i,2)


            end do

c           atualiza o tempo
            tempo = tempo + deltat


      end do

c     fecha o arquivo
      close(11)
      close(12)
      close(13)
      close(14)
      close(15)
      close(16)
      close(17)
      close(18)
      close(19)

      end program