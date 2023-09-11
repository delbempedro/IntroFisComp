      program andarilho bebado 2d

c     cria io nome do arquivo de saída e os vetores que armazenarão a posição dos andarilhos
      character name*20
      dimension histo(2000,2000)

c     define o número de andarilhos
      m = 1000

c     define as probabilidades      
      ap = 0.250e0

c     define o número de passos
      write(*,*) "Qual é o número de passos desejado?"
      read(*,*) n

c     define o nome do arquivo de saída
      name = 'histograma1'
c     //char(n)

c     define as somas
      somax1 = 0
      somay1 = 0
      somar1 = 0
      somar2 = 0

c     define o loop dos andarilhos
      do i=1,m

c       define a posição do andarilho
        ix = 1000
        iy = 1000
c       define o loop de cada andarilho
        do j=1,n
                                                 
c         define a direção do passo
          a = rand(0)
          if(a.lt.ap) then

            ix = ix+1

          else if(a.lt.2.0e0*ap) then

            ix = ix-1

          else if(a.lt.3.0e0*ap) then

            iy = iy+1

          else

            iy = iy-1

          end if

        end do

        histo(ix,iy) = histo(ix,iy) + 1
        somax1 = somax1 + ix-1000
        somay1 = somay1 + iy-1000

        s = somax1**2 + somay1**2
        somar1 = somar1 + sqrt(s)
        somar2 = somar2 + s

      end do

c     faz o tratamento dos dados (transforma em inteiro)
      do i=1,2000
        
        do j=1,2000
          
          k = histo(i,j)
          histo(i,j) = k

        end do

      end do

c     aloca a memória para salvar os dados do histograma
      open(unit=1,file=name)

c     inicia o loop para salvar as informações no histograma
      write(1,*) 'x X y'
      do i=1,2000
        
        write(1,*) histo(i,:)

      end do

c     fecha a unidade de memória
      close(1)

c     define as médias
      media1 = somar1/m
      media2 = (somar2 - (somax1**2 + somay1**2) )/m

      write(*,*) "A média do raio é:",media1,"e o laplaciano é:",media2

      end program

