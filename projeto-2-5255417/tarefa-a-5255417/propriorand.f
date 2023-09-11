      program randomnumbers

c     pede a potência de x
      write(*,*) "Qual é a potência de x desejada?"
      read(*,*) e

c     declara os números co-primos
      a = 2.0e0
      b = 343.0e0

c     declara o número de números aleatórios
      N = 100

c     declara o período da sequência
      am = 100

c     declara o número inicial
      x = 1
      
c     delcara a variável em que os números serão somados
      soma = 0

c     cria a relação de recorrência
      do i=0,N
        
        soma = soma + x**e
        x = mod(a*x+b,am)/2147483647
        write(*,*)x

      end do

c     declara a média dos números aleatórios
      media = soma/N

c     retorna a média de x de p-éssima potência
      write(*,*) "A média é:",media

      end program      


