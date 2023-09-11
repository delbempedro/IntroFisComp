       program Sort
       
       real lista(1000000000)

       write(*,*) 'Insira o tamanho da lista:'
       read(*,*) N

       write(*,*) 'Insira o número de elementos a serem ordenados:'
       read(*,*) M

       open(unit=1,file='entrada-1-5255417')
       open(unit=2,file='saida-1-5255417')


       do i=1,N

         read(1,*) lista(i)

       end do

       do j = 1, M
         elemento = lista(j)
         indice = j
           do k = J+1, N
             if (elemento.gt.lista(k)) then
               elemento = lista(k)
               indice = k
             end if
           end do
         aux = lista(j)
         lista(j) = elemento
         lista(indice) = aux
       end do
       
       do l = 1, M
       
         write(2,*) lista(l)

       end do

       write(2,*) 'M =',M

       close(1)
       close(2)

       end program
