      program entropia dos andarilhos bebados
            
c     cria do nome do arquivo de saída e os vetores que armazenarão a posição dos andarilhos
      character name*20
      dimension ihisto(-1000:1000,-1000:1000), ipx(0:3)
    
c     define o número de andarilhos
      n = 1000
    
c     define as probabilidades      
      ap = 0.250e0
    
c     define o número de passos
      nmax = 10e4
    
c     define o nome do arquivo de saída
      name = 'histograma-5255417'
    
c     aloca a memória para salvar os dados do histograma
      open(unit=1,file=name)

c     aloca memória para salvar os dados da entropia
      open(unit=2, file='gráfico-5255417')

c     escreve 0 no arquivo que define nas coordenadas x e y de cada partícula
      do k=1, n

        write(1,*) 0,0

      end do

c     calcula a entropia a cada ordem 10 de passos
      ipassos = 1
      ipassosf = 10
      do while(ipassosf.le.nmax)

c     limpa o vetor que armazena a posição dos andarilhos
        do i=-1000,1000
          do j=-1000,1000
          
            ihisto(i,j) = 0
                
          end do
        end do

c     define o loop dos andarilhos
      do i=1,n

c       define a posição do andarilho
        ix = 0
        iy = 0
c       define os passos possíveis
        ipx(0) = 0
        ipx(1) = 0
        ipx(2) = 0
        ipx(3) = 0
      
c       define o loop de cada andarilho
        do j=ipassos,ipassosf
                                                      
c         define a direção do passo
          a = rand(0)
      
c         faz a divisão inteira de i por ap
          int = rand(0)/ap   
      
c         incrementa um passo na direção correspondente a "int"
          ipx(int) = ipx(int) + 1
      
c         define as coordenadas finais de acordo com quantos passos
c         foram dados em cada direção
          ix = ipx(0) - ipx(1)
          iy = ipx(2) - ipx(3)
      
        end do
      
        ihisto(ix,iy) = ihisto(ix,iy) + 1
        write(1,*) ix,iy
      
            end do

c       escreve em um arquivo a relação entropia X passos
        an = n
        write(2,*) ipassosf, entropia(ihisto,an)

c       redefine as variáveis de iteração para continuar os passos de onde pararam
        ipassos = ipassosf
        ipassosf = ipassos*10

      end do

c     fecha as unidades de memória
      close(1)
      close(2)
      
      end program

      function entropia(ihisto, an)
      dimension ihisto(-1000:1000,-1000:1000)

c     inicia o loop percorrendo todo o "plano"
      i = -1000
      do while(i.eq.1000)
        j = -1000
        do while(j.eq.1000)

c         define o número de partículas inicial do retículado como 0
          aparticulas = 0

c         percorre cada retículado contabilizando quantas partículas existem no mesmo
          do k=i,i+10
            do l=j,j+10

c             soma à quantidade de a quantidade de partículas na posição (k,l)
              aparticulas = aparticulas + ihisto(k,l)

            end do
          end do

c         calcula a entropia devido a um retículado     
          if(aparticulas.gt.0) then

c             define a probabilidade das partículas estarem em
c             um retículado fazendo a razão das que nele estão
c             pelo total de partículas
              aprobabilidade = aparticulas/an

c             soma a probabilidade devido a um retículado
              entropia = entropia - aprobabilidade*log(aprobabilidade)
            
          end if

          j = j + 10

        end do

        i = i + 10

      end do

      end function