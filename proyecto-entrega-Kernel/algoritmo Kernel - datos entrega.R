library(igraph)
library(tidyverse)

# Nodos del 1 al 4 son citados
# Nodos del 5 al 8 citan.

data <- read.table("../../../Xarxes Socials i Econòmiques/Exercicis/webPageLinks.txt")

M <- data.matrix(data)

G <- graph_from_adjacency_matrix(M)

K <- t(M) %*% M
tt <- M %*% t(M)

MIdg <- max(degree(G,mode="in"))
MOdg <- max(degree(G,mode="out"))
gamma <- 1/(min(MIdg,MOdg))

# K---------------------------------------------------------------------
Kgamma <- K %*% solve(diag(ncol(data)) - gamma*K)
Kgamma_results = data.frame()

for (i in 1:nrow(Kgamma)){
  Kgamma_newsum=sum(Kgamma[i,])
  Kgamma_results=rbind(Kgamma_results,Kgamma_newsum)
  rm(Kgamma_newsum)
}

Kgamma_results <- Kgamma_results %>%
  mutate(V=colnames(data), .before=X2.33333333333333)
View(Kgamma_results)

# T---------------------------------------------------------------------

Tgamma <- tt %*% solve(diag(ncol(data)) - gamma*tt)
Tgamma_results = data.frame()

for (i in 1:nrow(Tgamma)){
  Tgamma_newsum=sum(Tgamma[i,])
  Tgamma_results=rbind(Tgamma_results,Tgamma_newsum)
  rm(Tgamma_newsum)
}
Tgamma_results <- Tgamma_results %>%
  mutate(V=colnames(data), .before=X0)
View(Tgamma_results)

#-----------------------------------------------------------------------
