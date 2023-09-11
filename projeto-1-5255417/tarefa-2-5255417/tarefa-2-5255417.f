       program AreaVetores

       dimension v1(3),v2(3)
       
       write(*,*) 'Insira as dimensões do primeiro vetor:'
       read(*,*) (v1(j),j=1,3)
       write(*,*) 'Insira as dimensões do segundo vetor:'
       read(*,*) (v2(j),j=1,3)

       di = ( v1(2)*v2(3) - v1(3)*v2(2) )**2
       dj = ( v1(3)*v2(1) - v1(1)*v1(3) )**2
       dk = ( v1(1)*v2(1) - v1(2)*v1(1) )**2

       area = ( sqrt(di+dj+dk) )/2
       
       write(*,*) 'A área do triângulo é:',area

       end program
