library(igraph)
library(tidyverse)

# Nodos del 1 al 4 son citados
# Nodos del 5 al 8 citan.

C<-matrix(c(0,0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 0, 0, 0, 0, 0, 0,
            0, 1, 1, 0, 0, 0, 0, 0,
            0, 0, 1, 1, 0, 0, 0, 0,
            0, 0, 1, 1, 0, 0, 0, 0),
          nrow=8,ncol=8,
          byrow=TRUE)

G=graph_from_adjacency_matrix(C)
plot(G)

M <- as_adjacency_matrix(G)
M <- matrix(M,nrow=8,ncol=8,
            dimnames=list(c("n5","n1","n2","n6","n3","n7","n4","n8"),
                          c("n5","n1","n2","n6","n3","n7","n4","n8")))

K <- t(C) %*% C
tt <- C %*% t(C)

MIdg <- max(degree(G,mode="in"))
MOdg <- max(degree(G,mode="out"))
gamma <- 1/(min(MIdg,MOdg))
gamma <- 0.207

# K---------------------------------------------------------------------
Kgamma <- K %*% solve(diag(8) - gamma*K)
Kgamma_results = data.frame()

for (i in 1:nrow(Kgamma)){
  Kgamma_newsum=sum(Kgamma[i,])
  Kgamma_results=rbind(Kgamma_results,Kgamma_newsum)
  rm(Kgamma_newsum)
}

Kgamma_results <- Kgamma_results %>%
  mutate(V=c("n5","n1","n2","n6","n3","n7","n4","n8"), .before=X184.936707487713)
View(Kgamma_results)

auth <- as.data.frame(authority.score(G)$vector)
auth <- auth %>% mutate(V=colnames(C), .before=`authority.score(G)$vector`)

#☻------------------------------------------------------------------------

gamma <- 0

# K---------------------------------------------------------------------
Kgamma <- K %*% solve(diag(8) - gamma*K)
Kgamma_results = data.frame()

for (i in 1:nrow(Kgamma)){
  Kgamma_newsum=sum(Kgamma[i,])
  Kgamma_results=rbind(Kgamma_results,Kgamma_newsum)
  rm(Kgamma_newsum)
}

Kgamma_results <- Kgamma_results %>%
  mutate(V=c("n5","n1","n2","n6","n3","n7","n4","n8"), .before=X2)
View(Kgamma_results)

auth <- as.data.frame(authority.score(G)$vector)
auth <- auth %>% mutate(V=colnames(C), .before=`authority.score(G)$vector`)



# T---------------------------------------------------------------------

Tgamma <- tt %*% solve(diag(8) - gamma*tt)
Tgamma_results = data.frame()

for (i in 1:nrow(Tgamma)){
  Tgamma_newsum=sum(Tgamma[i,])
  Tgamma_results=rbind(Tgamma_results,Tgamma_newsum)
  rm(Tgamma_newsum)
}
Tgamma_results <- Tgamma_results %>%
  mutate(V=c("n5","n1","n2","n6","n3","n7","n4","n8"), .before=X.6)

#-----------------------------------------------------------------------


NK <- function(A, gamma) {
  G <- graph_from_adjacency_matrix(A)
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

NK(C,0.207)
