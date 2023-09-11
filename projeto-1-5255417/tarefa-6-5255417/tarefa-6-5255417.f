       program complexroots

       write(*,*) "Insira o expoente N:"
       read(*,*) n
       
       pi=4.*atan(1.)

       write(*,*) 'As raízes complexas são:'

       do i = 1, n
       
         rparte = (3.**(1./n))*cos(2.*pi*i/n)+2.
         cparte = (3.**(1./n))*sin(2.*pi*i/n)

         write(*,10) rparte,',',cparte,'i'
10       format(f5.2,a,f5.2,a)

       end do

       end program
