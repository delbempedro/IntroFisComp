      program raiz

      implicit real*8 (a-h,o-z)
c     valores exatos -7, 2, 9

c     tolerância para considerar a raiz exata
      tol = 10d-6

c     passo incementado na busca pela raiz
      ap = 0.1d0

c     define o valor inicial como -10
      raizbd = -10.0d0

c     abre o arquivo de saida
      open(unit=1,file='saida-5255417')

c     busca direta:
      write(1,*)'Busca Direta'
      do i=1,3

c           imprimi a indicação de qual raiz está sendo calculada
            write(1,*)"r",i

c           passo incementado na busca pela raiz
            ap = 0.1d0

c           inicializa o valor de raizbd para procurar a próxima raiz
            raizbd = raizbd + ap
            j = 1
            do while(j.eq.1)
c abs(f(raizbd)).ge.tol
                  aux = raizbd + ap
                  if(f(raizbd)*f(aux).lt.0.0d0) then

                        ap = ap/2.0d0
                        x = raizbd + ap
                        write(1,*)x
                        k = 1
                        do while(k.lt.6)

                              aux = raizbd + ap
                              if(f(raizbd)*f(aux).lt.0.0d0) then
                                    ap = ap/2.0d0
                                    x = raizbd + ap
                                    write(*,*)x
                                    k = k + 1
                              end if

                              raizbd = raizbd + ap

                        end do
                        j = 0

                  end if
            

                  raizbd = raizbd + ap

            end do

            write(1,*)raizbd

      end do

c     define o valor inicial como -10
      raixrn = -10.0d0

c     Newton-Raphson:
      write(1,*)'Newton-Raphson'
      do i=1,3

c           imprimi a indicação de qual raiz está sendo calculada
            write(1,*)"r",i

c           passo incementado na busca pela raiz
            ap = 0.1d0

c           atualiza o valor atual
            raizrn = raizrn + ap

            do while(abs(f(raizrn)).ge.tol)
                  
c                 atualiza o valor antigo
                  aux = raizrn

c                 atualiza o valor atual
                  x = rn(rairn,aux)
                  raizrn = x
                  write(1,*)raizrn

            end do

            write(1,*)raizrn

      end do

c     define o valor inicial como -10
      raixs = -10.0d0

c     Secante:
      write(1,*)'Secante'
      do i=1,3

c           imprimi a indicação de qual raiz está sendo calculada
            write(1,*)"r",i

c           passo incementado na busca pela raiz
            ap = 0.1d0

c           atualiza o valor atual
            raizs = raizs + ap

            do while(abs(f(raizs)).ge.tol)
                  
c                 atualiza o valor antigo
                  aux = raizs

c                 atualiza o valor atual
                  x = s(raizs,aux)
                  raizs = x
                  write(1,*)raizs

            end do

            write(1,*)raizs

      end do

1     format(a1,f1.0)

c     fecha o arquivo de saida
      close(1)

      end program

c     define a função
      real*8 function f(x)
      implicit real*8 (a-h,o-z)

      f = x**3.0d0 - 4.0d0*(x**2.0d0) - 59.0d0*x + 126.0d0

      end function

c     define o metodo de newton-raphson
      real*8 function rn(x,xa)
      implicit real*8 (a-h,o-z)

      rn = x - f(x)/( (f(x)-f(xa))/(x-xa) )

      end function

c     define o metodo da secante
      real*8 function s(x,xa)
      implicit real*8 (a-h,o-z)

      s = x - f(x)*( (x-xa)/(f(x)-f(xa)) )

      end function