      program raiz

      implicit real*8 (a-h,o-z)
      dimension vetbd(3,6), vetrn(3,6), vetsec(3,6)
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
            write(1,1)"r",i

c           passo incementado na busca pela raiz
            ap = 0.1d0

c           inicializa o valor de raizbd para procurar a próxima raiz
            raizbd = raizbd + ap
            j = 1
            do while(j.eq.1)

c                 verifica se a raiz está dentro do intervalo
                  aux = raizbd + ap
                  if(f(raizbd)*f(aux).le.0.0d0) then

c                       inicia o loop com as 6 interações da tabela
                        do k=1,6

c                             divide o intervalo pela metade e imprimi
c                             a raiz correspondente
                              ap = ap/2.0d0
                              aux = raizbd + ap
                              vetbd(i,k) = aux

c                             muda o intervalo analisado até que haja
c                             uma raiz no intervalo
                              do while(f(raizbd)*f(aux).gt.0.0d0)

                                    raizbd = raizbd + ap
                                    aux = raizbd + ap

                              end do


                        end do
                        j = 0

                  end if

                  raizbd = raizbd + ap

            end do

      end do

c     Newton-Raphson:
      write(1,*)'Newton-Raphson'
      do i=1,3

c           imprimi a indicação de qual raiz está sendo calculada
            write(1,1)"r",i

c           define o valor do passo
            ap = (i-1)*10.0d0

c           define o valor inicial
            raizrn = -10.0d0 + ap

c           define o contador de interações
            icount = 1

            do while(abs(f(raizrn)).ge.tol)

c                 atualiza o valor
                  x = rn(raizrn)
                  raizrn = x
                  vetrn(i,icount) = raizrn

c                 incrementa o contador
                  icount = icount + 1

            end do

      end do

c     Secante:
      write(1,*)'Secante'
      do i=1,3

c           imprimi a indicação de qual raiz está sendo calculada
            write(1,1)"r",i

c           define o valor do passo
            ap = (i-1)*10.0d0

c           define o valor inicial
            raizs = -10.0d0 + ap

c           define o valor anterior
            aux = -10.0d0 + (ap-1.0d0)

c           define o contador de interações
            icount = 1

            do while(abs(f(raizs)).ge.tol)

c                 passa o valor atual para x
                  x = s(raizs,aux)

c                 atualiza o valor antigo
                  aux = raizs

c                 atualiza o valor atual
                  raizs = x
                  vetrn(i,icount) = raizs

c                 incrementa o contador
                  icount = icount + 1

            end do

      end do

1     format(a,i1)

c     fecha o arquivo de saida
      close(1)

      end program

c     define a função
      real*8 function f(x)
      implicit real*8 (a-h,o-z)

      f = x**3.0d0 - 4.0d0*(x**2.0d0) - 59.0d0*x + 126.0d0

      end function

c     define o metodo de newton-raphson
      real*8 function rn(x)
      implicit real*8 (a-h,o-z)

      rn = x - f(x)/( 3.0d0*(x**2.0d0) - 8.0d0*x - 59.0d0)

      end function

c     define o metodo da secante
      real*8 function s(x,xa)
      implicit real*8 (a-h,o-z)

      s = x - (f(x)*(x-xa))/(f(x)-f(xa))

      end function