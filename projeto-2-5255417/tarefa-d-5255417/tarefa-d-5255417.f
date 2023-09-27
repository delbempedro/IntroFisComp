      program entropia dos andarilhos bebados
            
c     cria do nome do arquivo de saída e os vetores que armazenarão a posição dos andarilhos
      character name*20
      dimension ihisto(-1000:1000,-1000:1000), ipx(0:3)
    
c     define o número de andarilhos
      n = 1000
    
c     define as probabilidades      
      ap = 0.250e0
    
c     define o número de passos
      nmax = 1000

c     aloca memória para salvar os dados da entropia
      open(unit=1, file='gráfico-5255417')

c     calcula a entropia a cada passo
      ipassos = 1
      do while(ipassos.le.nmax)

c       escreve 0 em todas as coordenadas do plano
        ihisto(-1000:1000,-1000:1000) = 0

c       define o loop dos andarilhos
        do i=1,n

c         define a posição do andarilho
          ix = 0
          iy = 0

c         define os passos possíveis
          ipx(0) = 0
          ipx(1) = 0
          ipx(2) = 0
          ipx(3) = 0
      
c         define o loop de cada andarilho
          do j=0,ipassos
                                                      
c           faz a divisão inteira de i por ap
            int = rand(0)/ap   
      
c           incrementa um passo na direção correspondente a "int"
            ipx(int) = ipx(int) + 1
      
          end do

c           define as coordenadas finais de acordo com quantos passos
c           foram dados em cada direção
            ix = ipx(0) - ipx(1)
            iy = ipx(2) - ipx(3) 
      
          ihisto(ix,iy) = ihisto(ix,iy) + 1
      
        end do

c       escreve em um arquivo a relação entropia X passos
        an = n
        write(1,*) ipassos, entropia(ihisto,an)

c       incrementa a quantidade de passos
        ipassos = ipassos + 1

      end do

c     fecha a unidade de memória
      close(1)
      
      end program

      function entropia(ihisto, an)
      dimension ihisto(-1000:1000,-1000:1000)

c     define o tamanho do retículado
      iret = 10

c     inicia a variável entropia
      entropia = 0

c     inicia o loop percorrendo todo o "plano"
      i = -1000
      do while(i.lt.1000)
        j = -1000
        do while(j.lt.1000)

c         define o número de partículas inicial do retículado como 0
          aparticulas = 0

c         percorre cada retículado contabilizando quantas partículas existem no mesmo
          do k=i,i+iret
            do l=j,j+iret

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

          j = j + iret

        end do

        i = i + iret

      end do

      end function
