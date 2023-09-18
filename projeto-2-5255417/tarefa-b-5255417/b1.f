      program andarilho bebado

c     cria o vetor que armazenará a posição dos andarilhos
      dimension ihisto(2000)

c     limpa o vetor que armazena a posição dos andarilhos
      do i=1, 2000

        ihisto(i) = 0

      end do

c     define o número de andarilhos
      m = 500000

c     define as probabilidades
      ap = 0.5

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

          if(rand(0).lt.ap) then

            ix = ix + 1

          else

            ix = ix - 1

          end if

        end do

        ihisto(ix) = ihisto(ix) + 1
        soma1 = soma1 + ix-1000
        soma2 = soma2 + (ix-1000)**2

      end do

c     aloca a memória para salvar os dados do histograma
      open(unit=1,file='histograma')

c     inicia o loop para salvar as informações no histograma
      do i=1,2000
        
        j = ihisto(i)
        write(1,*) i-1000,j

      end do

c     fecha a unidade de memória
      close(1)

c     define as médias
      amedia1 = soma1/m
      amedia2 = soma2/m

      write(*,*) "As médias de potência 1 e 2 são:",amedia1,amedia2

      end program
