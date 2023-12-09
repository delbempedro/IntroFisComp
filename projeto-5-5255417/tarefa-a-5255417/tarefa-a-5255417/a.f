      program orbitas

      implicit real*8(a-h,o-z)

c     define o valor de pi
      pi = 4.0d0*datan(1.0d0)

c     define a resultante da multiplicacao da constante gravitacional
c     pela massa do sol
      gms = 4.0d0*(pi**2.0d0)

c     define as coordenadas iniciais
      xi = 1.0d0
      yi = 0.0d0

c     define a variavel que armazenara os coordenadas maximas
      xmax = xi
      ymax = yi

c     define o raio inicial
      raio = dsqrt(xi**2.0d0 + yi**2.0d0)

c     define a velocidade em cada coordenada
      xvelocidade = 0.0d0
      yvelocidade = 2.0d0*pi*raio

c     define o intervalo de tempo utilizado
      deltat = 0.00001d0

c     define o tempo final como 1 ano
      tempofinal = 1.0d0

c     define o tempo atual
      tempo = 0.0d0

c     abre o arquivo das coordenadas
      open(unit=1,file='posicao')

c     escreve no arquivo a coordenada inicial
      write(1,*)xi,yi

c     realiza a interacao inicial em ambas as coorednadas
      xatual = xi + xvelocidade*deltat
      yatual = yi + yvelocidade*deltat

c     salva os valores antigos das coordenadas
      xantigo = xi
      yantigo = yi

c     realiza interacoes nas coordenadas ate o
c     tempo ser igual ao tempo final
      do while(tempo.lt.tempofinal)

c           calcula o raio atual
            raio = dsqrt(xatual**2.0d0 + yatual**2.0d0)

c           escreve no arquivo a coordenada atual
            write(1,*)xatual,yatual

c           escreve no arquivo a coordenada atual
            write(1,*)xatual,yatual

c           realiza um interacao em cada coordenada
            xproximo = 2.0d0*xatual - xantigo - gms*xatual/(raio**3.0d0)
     1*(deltat**2.0d0)
            yproximo = 2.0d0*yatual - yantigo - gms*yatual/(raio**3.0d0)
     1*(deltat**2.0d0)

c           salva os valores das coordenadas antes de altera-los
            xantigo = xatual
            yantigo = yatual

c           atualiza os valores das coordenadas
            xatual = xproximo
            yatual = yproximo

c           atualiza as coordenadas maximas
            if(abs(xatual).gt.xmax)then
                  xmax = xatual
            end if
            if(abs(yatual).gt.ymax)then
                  ymax = yatual
            end if

c           atualiza o tempo
            tempo = tempo + deltat

      end do

c     define os semieixos maior e menor
      if(xmax.gt.ymax)then
            emaior = xmax
            emenor = ymax
      else
            emaior = ymax
            emenor = xmax
      end if

c     calcula a excentricidade
      excentricidade = dsqrt((emaior**2.0d0) - (emenor**2.0d0))/emaior
      write(*,1)'A excentricidade é: ',excentricidade

1     format(a21,f5.3)

c     fecha o arquivo
      close(1)

      end program