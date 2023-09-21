      program andarilho bebado na rampa

c     cria os vetores que armazenarão a posição e passos dos andarilhos
c     e a variável que define o nome do arquivo de saída
      dimension ihisto(-1000:1000), ipx(0:10)
      character*30 name

c     limpa o vetor que armazena a posição dos andarilhos
      do i=-1000,1000

        ihisto(i) = 0

      end do

c     define o número de andarilhos
      m = 500000
      
c     define as probabilidades      
      write(*,*) "Qual é o valor de  desejado, onde p = 1/x?"
      read(*,*) x
      ap = 1.0e0/x

c     define o nome do arquivo
      name = 'histograma--5255417'

c     define o número de passos
      n = 1000

c     define as somas
      soma1 = 0
      soma2 = 0

c     define os passos possíveis
      ipx(0) = 1
      ipx(1:10) = -1
      um = 0
c     define o loop dos andarilhos
      do i=1,m

c       define a posição do andarilho
        ix = 1000
c       define o loop de cada andarilho
        do j=1,n
        
c         faz a divisão inteira de i por ap
          int = rand(0)/ap
      
c         adiciona o passo de acordo com o valor retornado por ipx()
          ix = ix + ipx(int)
          if (ipx(int).eq.-1) then
      um = um + 1
          end if

        end do

        ihisto(ix) = ihisto(ix) + 1
        soma1 = soma1 + ix
        soma2 = soma2 + (ix)**2

      end do
      write(*,*)um,1000**2
c     aloca a memória para salvar os dados do histograma
      open(unit=1,file=name)

c     inicia o loop para salvar as informações no histograma
      do i=-1000,1000
        
        j = ihisto(i)
        write(1,*) i,j 

      end do

c     fecha a unidade de memória
      close(1)

c     define as médias
      amedia1 = soma1/m
      amedia2 = soma2/m

      write(*,*) "As médias de potência 1 e 2 são:",amedia1,amedia2

      end program
