      program raiz

      implicit real*8 (a-h,o-z)
      dimension vetbd(3,6), vetrn(3,6), vetsec(3,6)

c     zera os vetores
      do i=1,3
            do j=1,6
                  vetbd(i,j)=0.0d0
                  vetrn(i,j)=0.0d0
                  vetsec(i,j)=0.0d0
            end do
      end do

c     tolerância para considerar a raiz exata
      tol = 10d-6

c     passo incementado na busca pela raiz
      ap = 0.1d0

c     define o valor inicial como -10
      raizbd = -10.0d0

c     abre o arquivo de saida
      open(unit=1,file='saida-5255417')

c     busca direta:
      do i=1,3

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
      do i=1,3

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
      do i=1,3

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
                  vetsec(i,icount) = raizs

c                 incrementa o contador
                  icount = icount + 1

            end do

      end do

c     imprimi o cabeçalho da tabela
      write(1,*)'|                Busca Direta                |         
     1      Newton-Raphson               |                   Secante    
     4              |'
      write(1,*)'|      r1      |      r2      |      r3      |      r1 
     3     |      r2      |      r3      |      r1      |      r2      |
     4      r3      |'

c     imprimi os resultados no arquivo
      do i=1,6

            write(1,1)vetbd(1,i),vetbd(2,i),vetbd(3,i),vetrn(1,i),vetrn(
     42,i),vetrn(3,i),vetsec(1,i),vetsec(2,i),vetsec(3,i)

      end do

c     imprimi os valores exatos      
      write(1,*)'Valores Exatos: -7, 2, 9'

c     formata as escritas
1     format(9('|',f14.8),'|')

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