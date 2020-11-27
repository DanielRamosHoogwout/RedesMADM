library(igraph)
library(tidyverse)

# Nodos del 1 al 4 son citados
# Nodos del 5 al 8 citan.

G=make_graph(c("n5","n1","n5","n2","n6","n2","n6","n3","n7","n3","n7","n4","n8","n3","n8","n4"),directed=TRUE)
plot(G)

M <- as_adjacency_matrix(G)
M <- matrix(M,nrow=8,ncol=8,
            dimnames=list(c("n5","n1","n2","n6","n3","n7","n4","n8"),
                          c("n5","n1","n2","n6","n3","n7","n4","n8")))

K <- t(M) %*% M
tt <- M %*% t(M)

MIdg <- max(degree(G,mode="in"))
MOdg <- max(degree(G,mode="out"))
gamma <- 1/(min(MIdg,MOdg))

# K---------------------------------------------------------------------
Kgamma <- K %*% solve(diag(8) - gamma*K)
Kgamma_results = data.frame()

for (i in 1:nrow(Kgamma)){
  Kgamma_newsum=sum(Kgamma[i,])
  Kgamma_results=rbind(Kgamma_results,Kgamma_newsum)
  rm(Kgamma_newsum)
}

Kgamma_results <- Kgamma_results %>%
  mutate(V=c("n5","n1","n2","n6","n3","n7","n4","n8"), .before=X0)
View(Kgamma_results)

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
