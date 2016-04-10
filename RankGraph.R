source('~/Repository/Queue.R')            

fila <- Queue$new()

rank <- function(adj, j) {
  
  n <- list()
  v <- c("NULO","NULO")
  
  ## Coloca NULO em cada vetor de vértices 
  
  for(i in 1:length(adj)) {
    n[[i]] <- v 
  }  
  
  ## Cálculo da menor distância em relação a j
  
  fila$push(j)
  n[[j]] <- c(0,0)
  
  while(fila$size() > 0) {                  ## Enquanto a fila não esvaziar o algoritmo continua
    m1 <- fila$pop()                        ## Recebe o próximo elemento da fila 
    for(i in 1:length(adj[[m1]])) {         ## Loop sobre todos os elementos da lista de adjacência de m1 
      if(n[[adj[[m1]][i]]][2] == "NULO") {  ## Verifica se já foi visitado, se nao foi terá NULO na componente 1
        fila$push(adj[[m1]][i])             ## Coloca na fila   
        dist <- as.numeric(n[[m1]][1])           
        n[[adj[[m1]][i]]] <- c(dist + 1,m1) ## Coloca os valores de distância de j e o vértice anterior      
      }
    }
  }
  
  s <- NULL
  for(i in 1:length(n)){                    ## Soma todas as distâncias
    s <- c(s,n[[i]][1])    
    
  }            
  
  
  print(n)
  print(1/sum(s))                           ## Calcula a medida de centralidade para o vértice j
}    
    
        
           
    
    
    

    
    
      
  






  
        
  
  
  
  
  
  
  
  
