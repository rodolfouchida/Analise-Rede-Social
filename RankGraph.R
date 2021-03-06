source('~/Repository/Queue.R')            

fila <- Queue$new()

rank <- function(adj, j) {
  
  n <- list()
  v <- c("NULO","NULO")
  
  ## Coloca NULO em cada vetor de v�rtices 
  
  for(i in 1:length(adj)) {
    n[[i]] <- v 
  }  
  
  ## C�lculo da menor dist�ncia em rela��o a j
  
  fila$push(j)
  n[[j]] <- c(0,0)
  
  while(fila$size() > 0) {                  ## Enquanto a fila n�o esvaziar o algoritmo continua
    m1 <- fila$pop()                        ## Recebe o pr�ximo elemento da fila 
    for(i in 1:length(adj[[m1]])) {         ## Loop sobre todos os elementos da lista de adjac�ncia de m1 
      if(n[[adj[[m1]][i]]][2] == "NULO") {  ## Verifica se j� foi visitado, se nao foi ter� NULO na componente 1
        fila$push(adj[[m1]][i])             ## Coloca na fila   
        dist <- as.numeric(n[[m1]][1])           
        n[[adj[[m1]][i]]] <- c(dist + 1,m1) ## Coloca os valores de dist�ncia de j e o v�rtice anterior      
      }
    }
  }
  
  s <- NULL
  for(i in 1:length(n)){                    ## Soma todas as dist�ncias
    s <- c(s,n[[i]][1])    
    
  }            
  
  
  print(n)
  print(1/sum(s))                           ## Calcula a medida de centralidade para o v�rtice j
}    
    
        
           
    
    
    

    
    
      
  






  
        
  
  
  
  
  
  
  
  
