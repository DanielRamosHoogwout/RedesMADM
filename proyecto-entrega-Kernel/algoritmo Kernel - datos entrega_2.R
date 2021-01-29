
NK <- function(A, gamma) {
  G <- graph_from_adjacency_matrix(A,mode=c("directed"),weighted=NULL)
  # Calculamos el máximo grado de difusión posible
  MIdg <- max(degree(G,mode="in"))
  MOdg <- max(degree(G,mode="out"))
  max_gamma <- 1/(min(MIdg,MOdg))
  # Comprobamos que gamma no es excesivamente elevado
  if(gamma > max_gamma) {
    return(paste("Grado de difusión demasiado alto. El valor debe estar entre 0 y",
                 round(max_gamma,3)))
  }
  K_ <- t(A) %*% A
  T_ <- A %*% t(A)
  output = list()
  output$K_g <- K_ %*% solve(diag(nrow(A)) - gamma*K_)
  output$T_g <- T_ %*% solve(diag(nrow(A)) - gamma*T_)
  return(output)
}

data <- as.matrix(read.table("../proyecto-entrega-Kernel/webPageLinks.txt"))
results <- NK(data,0.07)

Kg_results = data.frame()
for (i in 1:nrow(results$K_g)){
  Kg_newsum=sum(results$K_g[i,])
  Kg_results=rbind(Kg_results,Kg_newsum)
  rm(Kg_newsum)
}

Kg_results <- Kg_results %>%
  mutate(V=colnames(data), .before=X2.32558139534884)

auth <- as.data.frame(authority.score(G)$vector)
auth <- auth %>% mutate(V=colnames(data), .before=`authority.score(G)$vector`)

auth_k <- inner_join(Kg_results,auth)
View(auth_k)


