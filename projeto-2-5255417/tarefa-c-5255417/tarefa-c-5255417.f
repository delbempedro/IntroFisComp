      program andarilho bebado 2d

c     cria os vetores que armazenarão a posição e passos dos andarilhos
c     e a variável que define o nome do arquivo de saída
      dimension ihisto(-10000:10000,-10000:10000), ipx(0:3)
      character*30 name
c     define "n" como inteiro*8 para poder receber valores maiores de passos
      integer*8 n

c     limpa o vetor que armazena a posição dos andarilhos
      ihisto(-1000:1000,-1000:1000) = 0

c     define o número de andarilhos
      m = 1000

c     define as probabilidades      
      ap = 0.250e0

c     define o número de passos
      write(*,*) "Qual é o número de passos desejado?"
      read(*,*) n

c     define o nome do arquivo de saída
      name = 'histograma--5255417'

c     define as somas
      somax = 0
      somay = 0
      somar1 = 0
      somar2 = 0

c     aloca a memória para salvar os dados do histograma
      open(unit=1,file=name)

c     define o loop dos andarilhos
      do i=1,m

c       define a posição do andarilho
        ix = 0
        iy = 0
c       define os passos possíveis
        ipx(0) = 0
        ipx(1) = 0
        ipx(2) = 0
        ipx(3) = 0

c       define o loop de cada andarilho
        do j=1,n
                                                
c         define a direção do passo
          a = rand(0)

c         faz a divisão inteira de i por ap
          int = rand(0)/ap   

c         incrementa um passo na direção correspondente a "int"
          ipx(int) = ipx(int) + 1

        end do

c       define as coordenadas finais de acordo com quantos passos
c       foram dados em cada direção
        ix = ipx(0) - ipx(1)
        iy = ipx(2) - ipx(3)

        ihisto(ix,iy) = ihisto(ix,iy) + 1
        write(1,*) ix,iy

        s = ix**2 + iy**2
        somax = somax + ix
        somay = somay + iy
        somar1 = somar1 + sqrt(s)
        somar2 = somar2 + s

      end do

c     fecha a unidade de memória
      close(1)

c     define as médias
      amedia1 = somar1/m
      amedia2 = (somar2 - (somax**2+ somay**2) )/m

      write(*,*) "A média do raio é:",amedia1
      write(*,*) "e o delta ao quadrado é:",amedia2
      
      end program

