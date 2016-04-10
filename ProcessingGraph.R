

EdgeList <- read.csv("EdgeList.csv" , header = F) ## Carrega o arquivo no ambiente
EdgeList[,1] <- EdgeList[,1] + 1  ## Tem um vértice com label 0, entao renomeamos deslocando +1
EdgeList[,2] <- EdgeList[,2] + 1
EdgeList <- as.matrix(EdgeList)


## Grafo direcionado, assimetria nas arestas 

graph <- graph_from_edgelist(EdgeList, directed = T)   ## Cria o objeto grafo
adjlist <- get.adjedgelist(graph)
adjmatrix <- as.matrix(get.adjacency(graph))




## Cria lista de adjacências

adj_list <- list()

for(i in 1:100) {
  v <- which(adjmatrix[i,] == 1)
  adj_list[[i]] <- v   
}





