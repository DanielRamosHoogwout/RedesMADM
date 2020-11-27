library(igraph)
library(MASS) #Lo utilizamos para invertir matrices
#Un grafo cualquiera dirigido
G = graph(c(1,2,1,3,2,4,3,2,3,4,4,5,5,6,6,4), directed = TRUE)
length(V(G))
#Matriz de adjacencia de G
A = as.matrix(as_adjacency_matrix(G))
#Transpuesta de A
At = t(A)
#K = A^t*A
K = At %*% A

#T = A*A^t
T = A %*% At

#Matriz identidad
I = diag(length(V(G)))

#Computamos factor de difusion
out_degree = max(degree(G, mode = c("out"))) #Grados que salen (+)
in_degree = max(degree(G, mode = c("in"))) #Grados que entran (-)
gamma = 1/(min(out_degree,in_degree))

#K_gamma = K(I-gamma*K)^-1
K_gamma = K %*% ginv(I-gamma * K)

#T_gamma = T(I-gamma*T)^-1
T_gamma = T %*% ginv(I-gamma * T)
