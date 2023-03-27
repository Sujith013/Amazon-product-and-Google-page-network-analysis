#load the igraph library
library(igraph)

# *********************************************  Pre-Processing ******************************************

#View dataset
View(amazon)
View(google)

#dimensions of the dataset
dim(amazon)
dim(google)

#create a graph data frame (network)
amzNet = graph.data.frame(amazon[1:100,],directed = T)
gNet = graph.data.frame(google[1:100,],directed = T)

print(amzNet)
print(gNet)

#display the vertices
V(amzNet)
V(gNet)

#display the edges
E(amzNet)
E(gNet)

#Add the label and degree in the graph data frame
V(amzNet)$label = V(amzNet)$name
V(amzNet)$degree = degree(amzNet)

V(gNet)$label = V(gNet)$name
V(gNet)$degree = degree(gNet)

#check for loops and parallel edges in the graph
print(any_loop(amzNet))
print(any_loop(gNet))

print(any_multiple(amzNet))
print(any_multiple(gNet))

#Adjacency matrix
adj_amzMatrix = as_adjacency_matrix(amzNet)
adj_gMatrix = as_adjacency_matrix(gNet)

print(adj_amzMatrix)
print(adj_gMatrix)



# ********************************************* Calculations *******************************************


#betweenness centrality and edge betweenness
between_amz = betweenness(amzNet,v = V(amzNet),directed = TRUE,weights = NULL,
                          normalized = FALSE,cutoff = -1)
between_g = betweenness(gNet,v = V(gNet),directed = TRUE,weights = NULL,
                          normalized = FALSE,cutoff = -1)

print(between_amz)
print(between_g)

edge_btw_amz = edge_betweenness(amzNet,e=E(amzNet),directed=TRUE,weights=NULL,cutoff=-1)
edge_btw_g = edge_betweenness(gNet,e=E(gNet),directed=TRUE,weights=NULL,cutoff=-1)

print(edge_btw_amz)
print(edge_btw_g)


#degree centrality
amz_degree_in = degree(amzNet,v = V(amzNet),mode="in",loops=F,normalized=FALSE)
amz_degree_out = degree(amzNet,v = V(amzNet),mode="out",loops=F,normalized=FALSE)
amz_degree_all = degree(amzNet,v = V(amzNet),mode="all",loops=F,normalized=FALSE)

print(amz_degree_in)
print(amz_degree_out)
print(amz_degree_all)

g_degree_in = degree(gNet,v = V(gNet),mode="in",loops=F,normalized=FALSE)
g_degree_out = degree(gNet,v = V(gNet),mode="out",loops=F,normalized=FALSE)
g_degree_all = degree(gNet,v = V(gNet),mode="all",loops=F,normalized=FALSE)

print(g_degree_in)
print(g_degree_out)
print(g_degree_all)

#degree_distributions
degree.distribution(amzNet,cumulative = F)
degree.distribution(gNet,cumulative = F)

#Closeness centrality
amz_close = closeness(amzNet,vids = V(amzNet),mode="all",weights=NULL,normalized=F,cutoff=-1)
g_close = closeness(gNet,vids = V(gNet),mode="all",weights=NULL,normalized=F,cutoff=-1)

#Harmonic centrality
amz_harmonic = harmonic_centrality(amzNet,vids = V(amzNet),mode="out",weights=NULL,
                                   normalized=F,cutoff=-1)
g_harmonic = harmonic_centrality(gNet,vids = V(gNet),mode="out",weights=NULL,
                                 normalized=F,cutoff=-1)

print(amz_harmonic)
print(g_harmonic)




#  ************************************ VISUALIZATION *****************************************************

# ***** Plot 1*****

#create a graph data frame (network)
amzNet = graph.data.frame(amazon[1:60,],directed = T)
gNet = graph.data.frame(google[1:60,],directed = T)

#Add the label and degree in the graph data frame
V(amzNet)$label = V(amzNet)$name
V(amzNet)$degree = degree(amzNet)

V(gNet)$label = V(gNet)$name
V(gNet)$degree = degree(gNet)

#Plot the network
plot(amzNet,vertex.color='green',vertex.size=18,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=18,edge.arrow.size=0.5)

#Size of node depends on the degree
plot(amzNet,vertex.color='green',vertex.size=V(amzNet)$degree*2,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=V(gNet)$degree*2,edge.arrow.size=0.5)


#Size of node depends on the centrality
amz_close = closeness(amzNet,vids = V(amzNet),mode="all",weights=NULL,normalized=F,cutoff=-1)
g_close = closeness(gNet,vids = V(gNet),mode="all",weights=NULL,normalized=F,cutoff=-1)

plot(amzNet,vertex.color='green',vertex.size=amz_close*1000,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=amz_close*1000,edge.arrow.size=0.5)

#histogram for the number of nodes with a degree
hist(V(amzNet)$degree,col = 'green',ylab='number of nodes',xlab='degree of vertices',
     main = 'degree graph for the amazon network')
hist(V(gNet)$degree,col = 'red',ylab='number of nodes',xlab='degree of vertices',
     main='degree graph for the google network')

# **Top 5**
hist(sort(V(amzNet)$degree,decreasing = TRUE)[1:5],col = 'green',ylab='number of nodes',xlab='degree of vertices',
     main = 'degree graph for the amazon network')
hist(sort(V(gNet)$degree,decreasing = TRUE)[1:5],col = 'red',ylab='number of nodes',xlab='degree of vertices',
     main='degree graph for the google network')

barplot(sort(V(amzNet)$degree,decreasing = TRUE)[1:5],col = 'green',ylab='number of nodes',xlab='degree of vertices',
        main = 'degree graph for the amazon network')

barplot(sort(V(gNet)$degree,decreasing = TRUE)[1:5],col = 'green',ylab='number of nodes',xlab='degree of vertices',
        main = 'degree graph for the google network')

#maximum and minimum degree
print(max(V(amzNet)$degree))
print(max(V(gNet)$degree))

print(min(V(amzNet)$degree))
print(min(V(gNet)$degree))

# ****** PLOT - 2 ******

#create a graph data frame (network)
amzNet = graph.data.frame(amazon[60:110,],directed = T)
gNet = graph.data.frame(google[60:110,],directed = T)

#Add the label and degree in the graph data frame
V(amzNet)$label = V(amzNet)$name
V(amzNet)$degree = degree(amzNet)

V(gNet)$label = V(gNet)$name
V(gNet)$degree = degree(gNet)

#Plot the network
plot(amzNet,vertex.color='green',vertex.size=18,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=18,edge.arrow.size=0.5)

#Size of node depends on the degree
plot(amzNet,vertex.color='green',vertex.size=V(amzNet)$degree*2,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=V(gNet)$degree*2,edge.arrow.size=0.5)

#histogram for the number of nodes with a degree
hist(V(amzNet)$degree,col = 'green',ylab='number of nodes',xlab='degree of vertices',
     main = 'degree graph for the amazon network')
hist(V(gNet)$degree,col = 'red',ylab='number of nodes',xlab='degree of vertices',
     main='degree graph for the google network')

# **Top 5**
hist(sort(V(amzNet)$degree,decreasing = TRUE)[1:5],col = 'green',ylab='number of nodes',xlab='degree of vertices',
     main = 'degree graph for the amazon network')
hist(sort(V(gNet)$degree,decreasing = TRUE)[1:5],col = 'red',ylab='number of nodes',xlab='degree of vertices',
     main='degree graph for the google network')


#maximum and minimum degree
print(max(V(amzNet)$degree))
print(max(V(gNet)$degree))

print(min(V(amzNet)$degree))
print(min(V(gNet)$degree))

# ****** PLOT - 3 ******

#create a graph data frame (network)
amzNet = graph.data.frame(amazon[1000:1300,],directed = T)
gNet = graph.data.frame(google[1000:1300,],directed = T)

#Add the label and degree in the graph data frame
V(amzNet)$label = V(amzNet)$name
V(amzNet)$degree = degree(amzNet)

V(gNet)$label = V(gNet)$name
V(gNet)$degree = degree(gNet)

#Plot the network
plot(amzNet,vertex.color='green',vertex.size=18,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=18,edge.arrow.size=0.5)

#Size of node depends on the degree
plot(amzNet,vertex.color='green',vertex.size=V(amzNet)$degree*2,edge.arrow.size=0.5)
plot(gNet,vertex.color='red',vertex.size=V(gNet)$degree*2,edge.arrow.size=0.5)

#histogram for the number of nodes with a degree
hist(V(amzNet)$degree,col = 'green',ylab='number of nodes',xlab='degree of vertices',
     main = 'degree graph for the amazon network')
hist(V(gNet)$degree,col = 'red',ylab='number of nodes',xlab='degree of vertices',
     main='degree graph for the google network')

# **Top 5**
hist(sort(V(amzNet)$degree,decreasing = TRUE)[1:5],col = 'green',ylab='number of nodes',xlab='degree of vertices',
     main = 'degree graph for the amazon network')
hist(sort(V(gNet)$degree,decreasing = TRUE)[1:5],col = 'red',ylab='number of nodes',xlab='degree of vertices',
     main='degree graph for the google network')

#maximum and minimum degree
print(max(V(amzNet)$degree))
print(max(V(gNet)$degree))

print(min(V(amzNet)$degree))
print(min(V(gNet)$degree))



# ********************************  Shortest Path  ***********************************************


amz_shortest = shortest_paths(amzNet,1)
print(length(amz_shortest[[1]]))
print(amz_shortest)

g_shortest = shortest_paths(gNet,1)
print(length(g_shortest[[1]]))
print(g_shortest)

amz_path_length = c()

for(i in c(1:35))
{
  amz_path_length[i] = length(amz_shortest[[1]][[i]])-1
}

print(amz_path_length)

g_path_length=c()

for(i in c(1:34))
{
  g_path_length[i] = length(g_shortest[[1]][[i]])-1
}

print(g_path_length)

g_path_length = g_path_length[!g_path_length==-1]
amz_path_length = amz_path_length[!amz_path_length==-1]

boxplot(g_path_length,main="google network")

boxplot(amz_path_length,main="amazon network")



# *********************************** Page_rank Calculation **************************************


#create a graph data frame (network)
amzNet = graph.data.frame(amazon[1:100,],directed = T)
gNet = graph.data.frame(google[1:100,],directed = T)

amz_page_rank = page_rank(amzNet,vids=V(amzNet),directed=T,damping=0.85)
g_page_rank = page_rank(gNet,vids=V(gNet),directed=T,damping=0.85)

print(amz_page_rank$vector)
print(g_page_rank$vector)

amz_page_rank = page_rank(amzNet,vids=V(amzNet),directed=T,damping=0.15)
g_page_rank = page_rank(gNet,vids=V(gNet),directed=T,damping=0.15)

print(amz_page_rank$vector)
print(g_page_rank$vector)

barplot(sort(amz_page_rank$vector,decreasing = TRUE)[1:10],ylab='Page Rank',xlab='Vertex',
        main = 'Top 10 Page ranks',ylim=c(0.017,0.020))

barplot(sort(g_page_rank$vector,decreasing = TRUE)[1:10],ylab='Page Rank',xlab='Vertex',
        main = 'Top 10 Page ranks',ylim=c(0.013,0.016))

