      program entropia andarilho bebado

c     cria io nome do arquivo de saída e os vetores que armazenarão a posição dos andarilhos
      character name*20
      dimension ihisto(2000,2000)
    
c     limpa o vetor que armazena a posição dos andarilhos
      do i=1, 2000
        do j=1,2000
    
          ihisto(i,j) = 0
          
        end do
      end do
    
c     define o número de andarilhos
      m = 1000
    
c     define as probabilidades      
      ap = 0.250e0
    
c     define o número de passos
      nmax = 10e6
    
c     define o nome do arquivo de saída
      name = 'histograma'
    
c     aloca a memória para salvar os dados do histograma
      open(unit=1,file=name)

c     escreve 1000 no arquivo que define nas coordenadas x e y de cada partícula
      do k=1, m

        write(1,*) 1000,1000

      end do

c     calcula a entropia a cada ordem 10 de passos
      ipassos = 1
      ipassosf = 10
      do while(ipassosf.le.nmax)

c     define o loop dos andarilhos
        do ipassos=1,passosf

c     define a posição do andarilho
          read(1,*) ix,iy

c     define o loop de cada andarilho
          do j=1,n
                                                      
c     define a direção do passo
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

          ihisto(ix,iy) = ihisto(ix,iy) + 1
          write(1,*) ix,iy

        end do

c     redefine as variáveis de iteração para continuar os passos de onde pararam
        ipassos = ipassos*10
        ipassosf = ipassosf*10

      entropia

      end do

c     fecha a unidade de memória
      close(1)
      
      end program

      function entropia()

      do while(ix.le.2000 and iy.le.2000)



      end do

      end function