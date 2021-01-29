# Tutorial básico de iGraph

library(igraph)
library(tidyverse)

## Tipos de grafos -----------------------------------------------------------

# Grafo vacío
g_vacio <- graph.empty(n = 10, directed = TRUE)

# Grafo completo
g_complete <- graph.full(n = 10, directed = FALSE, loops = FALSE)

# Estrellas
g_star_out <- graph.star(n = 10, mode = "out")
g_star_in <- graph.star(n = 10, mode = "in")

# Anillos
g_ring <- graph.ring(n = 10)

# Un grafo a partir de una lista de aristas
edges <- c(1,2, 3,2, 2,4)
g <- graph(edges, n = max(edges), directed = TRUE)
# -----------------------------------------------------------------------------

## Información de la estructura del grafo

# Recuento de vértices
vcount(g)

# Recuento de aristas
ecount(g)

# ¿Cuáles son los vecinos del nodo 2 del grafo?
neighbors(g, V(g)[2], mode = 1)

# ¿Cuáles son las aristas que están conectadas con el nodo 2?
incident(g, V(g)[2], mode=c("all", "out", "in", "total")) # Basta 1 opción
incident(g, V(g)[2], mode=c("out"))

# Booleano para saber si es un grafo completamente conectado
is.directed(g)

# Booleano para determinar si dos nodos concretos están conectados
are.connected(g, V(g)[1], V(g)[3])

# Para obtener una lista de las aristas
get.edgelist(g)

# Para obtener la secuencia de vértices
V(g)

# Para obtener la secuencia de aristas
E(g, P=NULL, path=NULL, directed=T)

# -----------------------------------------------------------------------------

## Modificación de grafos

# Añadir un vértice (con una arista)
h <- g %>% add_vertices(1, attr = list()) %>% add_edges(c(4,5))
#------------------------------------------------------------------------------

## Visualización, guardado, importación y exportación de grafos

# Visualización
plot(h)

# Guardar el grafo en pdf
pdf("h.pdf")

# Importación de grafos
# i_1 <- read.graph("blablabla.txt", format="edgelist") # desde lista de aristas

# i_2 <- read.table("blablabla.txt", header=FALSE, as.is=T) # desde datatable
# i_2 <- graph_from_data_frame(d=i_2, directed=FALSE) 

# i_3 <- read.csv("blablabla.txt", header=TRUE,row.names=1,check.names=FALSE)
# i_3 <- as.matrix(i_3)
# i_3 <- graph.adjacency(i_3, mode = "directed", weighted=TRUE, diag = FALSE)

# Exortación de grafos
# write.graph(g, file = 'my_graph.txt', format = "edgelist")

# Advanced
# https://www.jessesadler.com/post/network-analysis-with-r/