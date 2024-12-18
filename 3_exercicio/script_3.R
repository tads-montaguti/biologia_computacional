if (!require(BiocManager)) install.packages("BiocManager")
if (!require(igraph)) install.packages("igraph")

BiocManager::install("RedeR")
BiocManager::install("TreeAndLeaf")

library("RedeR")
library("TreeAndLeaf")
library(igraph)

#### VIGNETTE ####
vignette("RedeR")


#### STARTTING UP REDER ####
resetRedeR()
startRedeR()

#-- Generate an 'hclust' object from the 'iris' dataset
hc_iris <- hclust(dist(iris[,-5]))

#-- Convert the 'hclust' object into a 'tree-and-leaf' object
tal <- treeAndLeaf(hc_iris)

#--- Map 'iris' variables to the tree-and-leaf graph
#Note: 'refcol = 0' indicates that 'iris' rownames will be used as mapping IDs
tal <- att.mapv(g = tal, dat = iris, refcol = 0)

#--- Set node attributes using the 'att.setv' wrapper function
cols <- c("#80b1d3","#fb8072","#8dd3c7")
tal <- att.setv(tal, from="Species", to="nodeColor", cols=cols)
tal <- att.setv(tal, from="Species", to="nodeLineColor", cols=cols) 
tal <- att.setv(tal, from="Petal.Width", to="nodeSize", nquant=6, xlim=c(5,50,1))

#--- Set other attributes using igraph shortcuts
V(tal)$nodeLabel <- ""
E(tal)$edgeLineColor <- "grey70"

#--- Send the tree-and-leaf graph to RedeR
addGraphToRedeR(tal, zoom=50)

#--- Suggestion: anchor inner nodes to adjust the final layout
selectNodes(V(tal)$name[!V(tal)$isLeaf], anchor=TRUE)

#--- Call 'relax' to fine-tune the leaf nodes
relaxRedeR(p1=10, p2=100, p3=5, p4=120, p5=1, p6=100)

#--- Add legends
addLegendToRedeR(tal, type="nodecolor", title="Species", stretch=0.2)
addLegendToRedeR(tal, type="nodesize", title="PetalWidth")

# resetRedeR()
