      program trescorpos

      implicit real*8(a-h,o-z)

c     define o valor de pi
      pi = 4.0d0*datan(1.0d0)

c     define raio e velocidade da terra e de jupiter
      raioterra = 1.0d0
      raiojupiter = 5.20d0
      velterra = 2.0d0*pi/dsqrt(raioterra)
      veljupiter = 2.0d0*pi/dsqrt(raiojupiter)

c     define a resultante da multiplicacao da constante gravitacional
c     pela massa do sol (ms), pela massa de jupiter (mj = ms/(10**3) )
c     e pela massa da terra (mt = ms/(3*(10**5)) ) 
      gms = 4.0d0*(pi**2.0d0)
      gmj = gms/(10**3)
      gmt = gms/(3*(10**5))

c     abre os arquivos das coordenadas de cada planeta
      open(unit=1,file='terra')
      open(unit=2,file='jupiter')

c     abre o arquivo onde serao salvas as distancias
c     de onde a terra esta a cada ano
      open(unit=3,file='distancia')

c     define as coordenadas iniciais da terra e de jupiter
      xt = raioterra
      yt = 0.0d0
      xj = raiojupiter
      yj = 0.0d0

c     define as distancias iniciais
      raioterrasol = dsqrt(xt**2.0d0 + yt**2.0d0)
      raiojupitersol = dsqrt(xj**2.0d0 + yj**2.0d0)
      raioterrajupiter = dsqrt((xt-xj)**2.0d0 + (yt-yj)**2.0d0)

c     define o intervalo de tempo utilizado de acordo com o raio
      deltat = 0.001d0
      
c     define o tempo atual
      tempo = 0.0d0

c     escreve no arquivo a coordenada inicial
      write(1,*)xt,yt
      write(2,*)xj,yj

c     realiza a interacao inicial em ambas as
c     coordenadas em ambos os planetas
      xtatual = xt
      ytatual = yt + velterra*deltat
      xjatual = xj
      yjatual = yj + veljupiter*deltat

c     salva os valores antigos das coordenadas
      xtantigo = xt
      ytantigo = yt
      xjantigo = xj
      yjantigo = yj

c     (re)inicia o controlador de periodo
      icontrolador = 0

c     realiza interacoes nas coordenadas ate jupiter
c     cruzar o eixo x 20 vezes (10 periodos)
      do while(icontrolador.lt.20)

c           calcula o raio atual
            raioterrasol = dsqrt(xtatual**2.0d0 + ytatual**2.0d0)
            raiojupitersol = dsqrt(xjatual**2.0d0 + yjatual**2.0d0)
            raioterrajupiter = dsqrt((xtatual-xjatual)**2.0d0 + (ytatual
     1-yjatual)**2.0d0)

c           escreve no arquivo a coordenada atual de cada planeta
            write(1,*)xtatual,ytatual
            write(2,*)xjatual,yjatual

c           realiza um interacao em cada coordenada da terra
            xtproximo = 2.0d0*xtatual - xtantigo - gms*xtatual/(raioterr
     1asol**3.0d0)*(deltat**2.0d0) - gmj*(xtatual-xjatual)/(raioterrajup
     2iter**3.0d0)*(deltat**2.0d0)
            ytproximo = 2.0d0*ytatual - ytantigo - gms*ytatual/(raioterr
     1asol**3.0d0)*(deltat**2.0d0) - gmj*(ytatual-yjatual)/(raioterrajup
     2iter**3.0d0)*(deltat**2.0d0)
c           realiza um interacao em cada coordenada de jupiter
            xjproximo = 2.0d0*xjatual - xjantigo - gms*xjatual/(raiojupi
     1tersol**3.0d0)*(deltat**2.0d0) - gmt*(xjatual-xtatual)/(raioterraj
     2upiter**3.0d0)*(deltat**2.0d0)
            yjproximo = 2.0d0*yjatual - yjantigo - gms*yjatual/(raiojupi
     1tersol**3.0d0)*(deltat**2.0d0) - gmt*(yjatual-ytatual)/(raioterraj
     2upiter**3.0d0)*(deltat**2.0d0)

c           salva os valores das coordenadas antes de altera-los
            xtantigo = xtatual
            ytantigo = ytatual
            xjantigo = xjatual
            yjantigo = yjatual

c           atualiza os valores das coordenadas
            xtatual = xtproximo
            ytatual = ytproximo
            xjatual = xjproximo
            yjatual = yjproximo

c           salva a posicao da terra a casa um ano e
c           calcula a variacao desta posicao a cada ano
            if(mod(icontrolador,2).eq.0)then
                  
c                 calcula a distancia entre a posição de dois anos consecutivos
                  if(icontrolador.eq.2)then
                        xant = xt
                        yant = yt
                        xatual = xtproximo
                        yatual = ytproximo
                        
                        distancia = dsqrt((xant-xatual)**2.0d0 + (yant -
     1 yatual)**2.0d0)
                  else
                        xprox = xtatual
                        yprox = ytatual
                        xant = xatual
                        yant = yatual
                        xatual = xprox
                        yatual = yprox

                        distancia = dsqrt((xant-xatual)**2.0d0 + (yant -
     1 yatual)**2.0d0)

                  end if

                  write(3,*)distancia

            end if

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

      end program