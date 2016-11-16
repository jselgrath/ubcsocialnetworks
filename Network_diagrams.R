# Network Diagrams in R
# Stephanie Tomscha
# 20161117

###########################
install.packages("igraph")
library(igraph)

#######################
###Clear workspace
rm(list=ls())

setwd("C:/Users/Jenny/Dropbox/7Teaching/Spatial_Zoology2016/Marine_network_ClassR")
setwd("SETWDHERE") # Change to your working directory

##############################
# Data overview: Survey looking at social relationships between researchers

###################
###Load and name files
d <-read.csv("acquaintance.csv", stringsAsFactors = FALSE, 
						 strip.white = TRUE, na.strings = c("NA",""), header= TRUE )
n<-seq(from =1, to= 53)
row.names(d)<-paste0("X", n)
d<-as.matrix(d)

z<-graph.adjacency(d)
k<-data.frame(get.edgelist(z))

###############################
# Data exploration

degree(z, normalized =FALSE)
betweenness(z, normalized=FALSE)
eigenc<-evcent(z)$vector

#####################################
#####Make a basic network graph and save as a pdf

# code for setting graph options
# e.g. size of nodes (vertex. size), type of layout (plot.layout), label size
igraph.options(plot.layout=layout.random, vertex.size=8, vertex.label=NA, 
							 edge.arrow.size = 0, vertex.color =NA) 

# plotting graph from above
plot(z) 

# plot graph with new layout and labels
igraph.options(plot.layout=layout.auto, vertex.size=10, vertex.label=V(z)$names,
							 edge.arrow.size = 0, vertex.color =NA, vertex.label.color ="black", vertex.label.cex= 1)


# saving new graph to PDF
pdf("Aquaint_after.pdf") 

# view new graph - this is needed to export graph
plot(z)

dev.off() # tells it to stop exporting figures
 
# plot layout
coord <- layout.auto(z)

#####################################
# create graphs based on different node attributes - colored differently based on 3 types of centrality

#####1. DEGREE CENTRALITY graph
# Number of connections each node has, normalized
hc5 <- heat.colors(9) # color index for 9 colors - heat colors: red and yellow
g.bet <- degree(z) # degree centrality for graph
g.bet.max <- max(g.bet) # max degree centrality for graph
vcolors <- 9 - round(8 *(g.bet / g.bet.max)) # makes color ramp to span values of degree centrality 
vcolors <- hc5[vcolors] # sets color ramp as colors for graph

# set new graph colored by degree centrality
igraph.options(plot.layout=layout.auto, vertex.size=10, vertex.label=V(z)$names,
							 edge.arrow.size = 0, vertex.color = vcolors, vertex.label.color ="black", vertex.label.cex= 1)
plot(z)               
pdf("Aquaint_after_degree.pdf")
plot(z) # red nodes have high degree centrality, yellow nodes are low
dev.off()

########2. BETWEENNESS CENTRALITY graph
# number of shortest paths from all verticies which pass through a particular node. Used to determine how important a node is for connecting unconnected groups or individuals. A person with high BC may be an important intermediary between other network members.
hc5 <- heat.colors(9) 
g.bet <- betweenness(z) 
g.bet.max <- max(g.bet) 
vcolors <- 9 - round(8 *(g.bet / g.bet.max)) 
vcolors <- hc5[vcolors] 

igraph.options(plot.layout=layout.circle, vertex.size=10, vertex.label=V(z)$names, edge.arrow.size = 0, vertex.color = vcolors, vertex.label.color ="black", vertex.label.cex= 1)

plot(z)               
pdf("Aquaint_after_betweenness.pdf")
plot(z)
dev.off()

########3. EIGENVECTOR centrality
# high value has friends who are important and well connected. Sum of centrality values of nodes connected to.
hc5 <- heat.colors(9) 
g.bet <- data.frame(eigen_centrality(z) )
g.bet.max <- max(g.bet$vector) 
vcolors <- 9 - round(8 *(g.bet$vector / g.bet.max)) 
vcolors <- hc5[vcolors] 

igraph.options(plot.layout=layout.auto, vertex.size=10, vertex.label=V(z)$names,
							 edge.arrow.size = 0, vertex.color = vcolors, vertex.label.color ="black", vertex.label.cex= 1)

plot(z)               
pdf("Aquaint_after_eigen.pdf")
plot(z)
dev.off()



