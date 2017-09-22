# Subset conceptual model to selected target(s), action(s), and intermediary threats, stressors, and edges


library(igraph)
setwd("C:/Users/Tyler/Google Drive/NFWF Cross Realm/Data")  # directory where association matrix and coordinate files are stored


### Create network object from conceptual model
assoc.mat <- as.matrix(read.csv("conceptual_model_association_matrix.csv", header=T, row.names=1))  # read in association matrix of edge presence (value=1) or absence (value=0) for each node pair
coords <-as.matrix(read.csv("vertex_coords.csv"))[,2:3]   # spatial coordinates of nodes for drawing conceptual model
network <- graph_from_adjacency_matrix(assoc.mat, mode="directed")   # create network object
network <- set_vertex_attr(network, "type", index=V(network), value=c(rep("action",10),rep("threat",6), rep("stress",9), rep("target",5)))  # define type for each node
vertex.info <- vertex_attr(network, index = V(network))  # extract vertex info to dataframe for looking up vertex IDs and types
print(df <- data.frame(vertexID=c(1:length(vertex.info$name)), vertexName=vertex.info$name, vertexType=vertex.info$type))  # 


### Select target(s) and action(s) of interest
  # We will want to replace the following with code that reads action and target selections 
  # from the user interface and assigns them correct vertex IDs from the network;
  # As an example for now, I just punched in IDs for a couple randomly selected actions and targets
select.actions <- c(9,10)  # vertex IDs for selected actions
select.targets <- c(29,30)  # vertex IDs for selected targets


### Find all nodes and edges along directed paths between selected target and action
pathlist <- list()
for(i in select.actions) {
  for(j in select.targets) {
    paths <- all_simple_paths(network, i, j)  # find all paths between action i and target j
    pathlist <- c(pathlist, paths) 
  }
}
select.nodes <- sort(unique(as.vector(unlist(pathlist)))) # vector of unique node IDs along paths between selected actions and targets
edgelist <- c()
for(k in 1:length(pathlist)) {
  pathedges <- E(network, path=pathlist[[k]])  # find all edges along selected path
  edgelist <- c(edgelist, pathedges)
}
select.edges <- sort(unique(as.vector(unlist(edgelist))))  #  vector of unique edge IDs along paths between selected actions and targets


### Plot network subset (selected vertices and edges are darker/bolder)
label.color <- rep("grey70",length(V(network)))
label.color[select.nodes] <- "black"
label.font <- rep(1,length(V(network)))
label.font[select.nodes] <- 2
vertex.shape <- c(rep("palegreen",10),rep("khaki1",6), rep("rosybrown1",9), rep("lightcyan1",5))
vertex.color.light <- c(rep("palegreen",10),rep("khaki1",6), rep("rosybrown1",9), rep("lightcyan1",5))
vertex.color.dark <- c(rep("chartreuse4",10),rep("orange1",6), rep("orangered",9), rep("darkcyan",5))
vertex.color.all <- vertex.color.light
vertex.color.all[select.nodes] <- vertex.color.dark[select.nodes]
edge.color <- rep("grey80", length(E(network)))
edge.color[select.edges] <- "black"
edge.width <- rep(1, length(E(network)))
edge.width[select.edges] <- 2
plot.igraph(network, 
            layout=coords,
            margin=-0.3,
            vertex.label=rownames(assoc.mat), 
            vertex.color=vertex.color.all, 
            vertex.label.cex=0.5, 
            vertex.label.color=label.color,
            vertex.label.font=label.font,
            vertex.shape="crectangle",
            vertex.size=25,   
            vertex.size2=15,
            vertex.frame.color=NA,
            edge.color=edge.color,
            edge.arrow.size=0.4,
            edge.width=edge.width)
            





