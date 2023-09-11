      program randomnumbers

c     pede a potência de x
      write(*,*) "Qual é a potência de x desejada?"
      read(*,*) e

c     declara o número de números aleatórios
      N = 10 000 000
      
c     delcara a variável em que os números serão somados
      soma = 0

c     soma os números aleatórios
      do i=0,N
        
        soma = soma + rand()**e

      end do

c     declara a média dos números aleatórios
      amedia = soma/N

c     retorna a média de x de p-éssima potência
      write(*,1) "A média é: ",amedia
1     format(a14,f4.2)

      end program      


