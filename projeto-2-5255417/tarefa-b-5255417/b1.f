      program andarilho bebado

c     cria o vetor que armazenaŕa a posição dos andarilhos
      dimension histo(2000)

c     define o número de andarilhos
      m = 1000

c     define as probabilidades
      ap = 0.5e0

c     define o número de passos
      n = 1000

c     define as somas
      soma1 = 0
      soma2 = 0

c     define o loop dos andarilhos
      do i=1,m

c       define a posição do andarilho
        ix = 1000
c       define o loop de cada andarilho
        do j=1,n
        
c         define a direção do passo
          if(rand(0).lt.ap) then
 
            ix = ix+1

          else

            ix = ix-1

          end if

        end do

        histo(ix) = histo(ix) + 1
        soma1 = soma1 + ix-1000
        soma2 = soma2 + (ix-1000)**2

      end do

c     aloca a memória para salvar os dados do histograma
      open(unit=1,file='histograma')

c     inicia o loop para salvar as informações no histograma
      do i=0,2000
        
        j = histo(i)
        write(1,*) i-1000,j 

      end do

c     fecha a unidade de memória
      close(1)

c     define as médias
      media1 = soma1/m
      media2 = soma2/m

      write(*,*) "As médias de potência 1 e 2 são:",media1,media2

      end program
