      program trescorpos

      implicit real*8(a-h,o-z)

c     define o valor de pi
      pi = acos(-1.0d0)

c     define raio e velocidade de jupiter e dos asteroides
      raiojupiter = 5.2d0
      veljupiter = 2.755d0
      raioa1 = 3.000d0
      raioa2 = 3.276d0
      raioa3 = 3.700d0
      vela1 = 3.628d0
      vela2 = 3.471d0
      vela3 = 3.267d0

c     define a resultante da multiplicacao da constante gravitacional
c     pela massa do sol (ms) e pela massa de jupiter (mj = ms/(10**3) )
      gms = 4.0d0*(pi**2.0d0)
      gmj = gms/(10**3)

c     abre os arquivos das coordenadas de cada planeta
      open(unit=1,file='asteroide1')
      open(unit=2,file='asteroide2')
      open(unit=3,file='asteroide3')
      open(unit=4,file='jupiter-b3')

c     define as coordenadas iniciais dos asteroides e de jupiter
      x1 = raioa1
      y1 = 0.0d0
      x2 = raioa2
      y2 = 0.0d0
      x3 = raioa3
      y3 = 0.0d0
      xj = raiojupiter
      yj = 0.0d0

c     define as distancias iniciais
      raioa1 = dsqrt(x1**2.0d0 + y1**2.0d0)
      raioa2 = dsqrt(x2**2.0d0 + y2**2.0d0)
      raioa3 = dsqrt(x3**2.0d0 + y3**2.0d0)
      raiojupitersol = dsqrt(xj**2.0d0 + yj**2.0d0)

c     define o intervalo de tempo utilizado de acordo com o raio
      deltat = 0.001d0
      
c     define o tempo atual
      tempo = 0.0d0

c     escreve no arquivo a coordenada inicial
      write(1,*)x1,y1
      write(2,*)x2,y2
      write(3,*)x3,y3
      write(4,*)xj,yj

c     realiza a interacao inicial em ambas as
c     coordenadas em ambos os planetas
      x1atual = x1
      y1atual = y1 + vela1*deltat
      x2atual = x2
      y2atual = y2 + vela2*deltat
      x3atual = x3
      y3atual = y3 + vela3*deltat
      xjatual = xj
      yjatual = yj + veljupiter*deltat

c     salva os valores antigos das coordenadas
      x1antigo = x1
      y1antigo = y1
      x2antigo = x2
      y2antigo = y2
      x3antigo = x3
      y3antigo = y3
      xjantigo = xj
      yjantigo = yj

c     (re)inicia o controlador de periodo
      icontrolador = 0

c     realiza interacoes nas coordenadas ate jupiter
c     cruzar o eixo x 20 vezes (10 periodos)
      do while(icontrolador.lt.40)

c           calcula o raio atual
            raioa1 = dsqrt(x1atual**2.0d0 + y1atual**2.0d0)
            raioa1jupiter = dsqrt((x1-xjatual)**2.0d0 + (y1-yjatual)**2.
     10d0)
            raioa2 = dsqrt(x2atual**2.0d0 + y2atual**2.0d0)
            raioa2jupiter = dsqrt((x2-xjatual)**2.0d0 + (y2-yjatual)**2.
     10d0)
            raioa3 = dsqrt(x3atual**2.0d0 + y3atual**2.0d0)
            raioa3jupiter = dsqrt((x3-xjatual)**2.0d0 + (y3-yjatual)**2.
     10d0)
            raiojupitersol = dsqrt(xjatual**2.0d0 + yjatual**2.0d0) 

c           escreve no arquivo a coordenada atual de cada planeta
            write(1,*)x1atual,y1atual
            write(2,*)x2atual,y2atual
            write(3,*)x3atual,y3atual
            write(4,*)xjatual,yjatual

c           realiza um interacao em cada coordenada de cada asteroide:
c                 asteroide1:
            x1proximo = 2.0d0*x1atual - x1antigo - gms*x1atual/(raioa1**
     13.0d0)*(deltat**2.0d0) - gmj*(x1atual-xjatual)/(raioa1jupiter**3.0
     2d0)*(deltat**2.0d0)
            y1proximo = 2.0d0*y1atual - y1antigo - gms*y1atual/(raioa1**
     13.0d0)*(deltat**2.0d0) - gmj*(y1atual-yjatual)/(raioa1jupiter**3.0
     2d0)*(deltat**2.0d0)
c                 asteroide2:
            x2proximo = 2.0d0*x2atual - x2antigo - gms*x2atual/(raioa2**
     13.0d0)*(deltat**2.0d0) - gmj*(x2atual-xjatual)/(raioa2jupiter**3.0
     2d0)*(deltat**2.0d0)
            y2proximo = 2.0d0*y2atual - y2antigo - gms*y2atual/(raioa2**
     13.0d0)*(deltat**2.0d0) - gmj*(y2atual-yjatual)/(raioa2jupiter**3.0
     2d0)*(deltat**2.0d0)
c                 asteroide3:
            x3proximo = 2.0d0*x3atual - x3antigo - gms*x3atual/(raioa3**
     13.0d0)*(deltat**2.0d0) - gmj*(x3atual-xjatual)/(raioa3jupiter**3.0
     2d0)*(deltat**2.0d0)
            y3proximo = 2.0d0*y3atual - y3antigo - gms*y3atual/(raioa3**
     13.0d0)*(deltat**2.0d0) - gmj*(y3atual-yjatual)/(raioa3jupiter**3.0
     2d0)*(deltat**2.0d0)

c           realiza um interacao em cada coordenada de jupiter
            xjproximo = 2.0d0*xjatual - xjantigo - gms*xjatual/(raiojupi
     1tersol**3.0d0)*(deltat**2.0d0)
            yjproximo = 2.0d0*yjatual - yjantigo - gms*yjatual/(raiojupi
     1tersol**3.0d0)*(deltat**2.0d0)

c           salva os valores das coordenadas antes de altera-los
            x1antigo = x1atual
            y1antigo = y1atual
            x2antigo = x2atual
            y2antigo = y2atual
            x3antigo = x3atual
            y3antigo = y3atual
            xjantigo = xjatual
            yjantigo = yjatual

c           atualiza os valores das coordenadas
            x1atual = x1proximo
            y1atual = y1proximo
            x2atual = x2proximo
            y2atual = y2proximo
            x3atual = x3proximo
            y3atual = y3proximo
            xjatual = xjproximo
            yjatual = yjproximo

c           atualiza o tempo
            tempo = tempo + deltat

c           verifica se o planeta cruzou o eixo x
c           incrementando o controlador em caso verdadeiro
            if((yjproximo*yjantigo).le.0.0d0)then
                  icontrolador = icontrolador + 1
            end if

      end do

c     fecha o arquivo
      close(1)
      close(2)
      close(3)
      close(4)

      end program