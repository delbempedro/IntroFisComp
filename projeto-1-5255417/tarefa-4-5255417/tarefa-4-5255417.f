       program primes

       real  primos(1000000000)

       write(*,*) 'Insira o número limite desejado:'
       read(*,*) n

       ndeprimos = 0
       do i=2,n
         primeORno = 0
         do j=2, i-1
           
           a = i
           b = j

           if (i/j.eq.a/b) then
             primeORno = 1
           end if
           
         end do

         if (primeORno.eq.0) then
           ndeprimos = ndeprimos + 1
           primos(ndeprimos) = i
         end if

       end do
       
       open(unit=3,file='saida')

       write(3,*) 'O número de primos menores do que',n,'é',ndeprimos

       write(3,*) 'E estes são:'
       do i=1, ndeprimos
         
         inteiro = primos(i) 
         write(3,*) inteiro

       end do

       close(3)

       end program
