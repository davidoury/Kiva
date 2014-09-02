load('dataframes')
library(igraph)
library(parallel)
library(ggplot2)

# Create a dataframe of edges - the dataframe has two columns 
# an edge is created by function graph.data.frame 
# from the node named by the first column 
# to the node named by the second column
#
edge.df = data.frame(loan_id  = as.character(loans.lenders.df$id), 
                     lender_id = as.character(loans.lenders.df$lender_ids), 
                     stringsAsFactors=FALSE)

# Create vector of unique loans and a vector of unique lenders
# We need a unique list of node names for the graph.data.frame function
# There will be two types of nodes: loans and lenders
#
loan.vector   = unique(edge.df$loan_id)
lender.vector = unique(edge.df$lender_id)

# Create dataframe of loan nodes  
# where each node has an attribute named "type" with value "loan"
#
loan.node.df = data.frame(id=loan.vector, 
                          type='loan', 
                          stringsAsFactors=FALSE)
# Verify details
str(loan.node.df)
min(loan.node.df$id)
max(loan.node.df$id) # ???
table(loan.node.df$type)
# rm(loan.vector)

# Create dataframe of lender vertices with type 'lender'
# where each node has an attribute named "type" with value "lender"
#
lender.node.df = data.frame(id=lender.vector, 
                            type='lender', 
                            stringsAsFactors=FALSE)
# Verify
str(lender.node.df)
min(lender.node.df$id)
max(lender.node.df$id)
table(lender.node.df$type)
# rm(lender.vector)

# Create node.df by concatenating the loan and lender dataframes
#
node.df = rbind(loan.node.df, lender.node.df)
table(node.df$type) # Verify
# rm(loan.node.df)
# rm(lender.node.df)

# Remove duplicates <id> values from node.df
#
dup.ndx = duplicated(node.df$id)
table(dup.ndx) # Check, skip next 7 lines if no duplicates
keep.ndx = !dup.ndx
node.df$id       [node.df$id         %in% node.df$id[dup.ndx]]
loan.node.df$id  [loan.node.df$id    %in% node.df$id[dup.ndx]]
lender.node.df$id[lender.node.df$id  %in% node.df$id[dup.ndx]]
node.df = node.df[keep.ndx,] # Keep the non-duplicates
sum(duplicated(node.df$id))  # Verify
# rm(dup.ndx,keep.ndx)

# Verify edge.df and node.df
#
str(edge.df)
str(node.df)

# Create an undirected graph from edge.df and node.df (using graph.data.frame)
# Nodes are loans or lenders (node.df)
# Edges from lenders to the loans they fund (edges.df)
# the edges though are undirected (directed=FALSE)
#
g.lole = graph.data.frame(d=edge.df,
                          directed=FALSE, 
                          vertices=node.df)
# the vertices/nodes function V returns a "vertex sequence" 
# which can be accessed by number or node name
V(g.lole)[3]
V(g.lole)['716797']
# The attributes of each node are accessible (each node has a "type" attribute)
V(g.lole)[3]$type
# Verify
table(V(g.lole)$type) # Verify
# rm(edge.df, node.df)

# All edges link loans and lenders
# Set the "type" of every edge to "loan-lender"
#
E(g.lole)$type = 'loan-lender'
table(E(g.lole)$type) # Verify

# Create vectors of loan nodes and lender nodes (for bookkeeping purposes)
# The which function returns the indices with value TRUE
#
lole.loan.ndx   = which(V(g.lole)$type == 'loan')
lole.lender.ndx = which(V(g.lole)$type == 'lender')
# Verify
length(lole.loan.ndx) 
length(lole.lender.ndx)
table(node.df$type)

# Verify that these vectors work as expected
#
some.loan.ndx   = sample(length(lole.loan.ndx),5)
some.lender.ndx = sample(length(lole.lender.ndx),5)
V(g.lole)[lole.loan.ndx  [some.loan.ndx]]$name
V(g.lole)[lole.loan.ndx  [some.loan.ndx]]$type
V(g.lole)[lole.lender.ndx[some.lender.ndx]]$name
V(g.lole)[lole.lender.ndx[some.lender.ndx]]$type

# The plot.neighborhood function plots the induced subgraph consisting of 
# a node and its neighborhood
# The neighbors of a node is the set of nodes which share an edge with that node
# The degree of a node is the number of edges connected to that node
# The induced subgraph of a node and its neighbors consists of
# 1) that node and its neighbors
# 2) all edges between these nodes
#
plot.neighborhood = function(g,a.node) {
  a.neighborhood = neighbors(g,a.node)
  sg = induced.subgraph(g,c(a.node,a.neighborhood))
  plot(sg,vertex.size=1)
}
# Examples
plot.neighborhood(g.lole,V(g.lole)['656901'])
degree(g.lole,V(g.lole)['656901'])
plot.neighborhood(g.lole,V(g.lole)['zingo9373'])
degree(g.lole,V(g.lole)['zingo9373'])

# Get the degree for all lender nodes (store in lole.loan.deg)
#
lole.loan.deg = degree(g.lole, V(g.lole)[lole.loan.ndx])
length(lole.loan.deg)
qplot(log10(lole.loan.deg))
# qplot(lole.loan.deg)
table(lole.loan.deg<100)

# Get the degree for all loan nodes (store in lole.lender.deg)
#
lole.lender.deg = degree(g.lole, V(g.lole)[lole.lender.ndx])
length(lole.lender.deg)
qplot(log10(lole.lender.deg))
table(lole.lender.deg<100)

# Get lenders who funded fewer than 100 loans
#
lole.lender.deg.log = lole.lender.deg<100
lole.lender.deg.ndx = which(lole.lender.deg.log)
# Find max degree of all lender nodes of degree less than 100 (Verify)
max(degree(g.lole,
           V(g.lole)[lole.lender.ndx[lole.lender.deg.ndx]]))

# Get loans with fewer than 100 lenders
#
lole.loan.deg.log = lole.loan.deg<100
lole.loan.deg.ndx = which(lole.loan.deg.log)
# Find max degree of all loan nodes of degree less than 100 (Verify)
max(degree(g.lole, # Check
           V(g.lole)[lole.loan.ndx[lole.loan.deg.ndx]]))
degree(g.lole, # Use
       V(g.lole)[lole.lender.ndx[lole.lender.deg.ndx[1:10]]])

# Function get.full.neighborhood.edgelist 
# Input: graph (g) and a node of the graph (a.node)
# Returns: a list of edges connecting all neighbors of a.node
# Edges are from odd numbered nodes to the next node in the vector
#
get.full.neighborhood.edgelist = function(g,a.node) {
  a.neighborhood = neighbors(g,a.node)
  if( length(a.neighborhood) < 2 ) {
    return(c())
  } else {
    edgelist = as.vector(combn(a.neighborhood, 2))
    return(edgelist)
  }
}
# Examples
plot.neighborhood(g.lole,V(g.lole)['elisabeth1514'])
get.full.neighborhood.edgelist(g.lole,'elisabeth1514')
V(g.lole)[get.full.neighborhood.edgelist(g.lole,'elisabeth1514')]
plot.neighborhood(g.lole,V(g.lole)['656901'])
get.full.neighborhood.edgelist(g.lole,'656901')
V(g.lole)[get.full.neighborhood.edgelist(g.lole,'656901')]

# Create graph with only those lenders who fund fewer than 100 loans
# 1) Drop (lender) nodes which fund 100 or more loans
# 2) Drop edges connected to dropped (lender) nodes
#
g.reduced = 
  induced.subgraph(g.lole, 
                   c(V(g.lole)[lole.loan.ndx], # all loans
                     V(g.lole)[lole.lender.ndx[lole.lender.deg.ndx]]))
                      # only lendes tha fund fewer than 100 loans
# Dropped about 4000 lender nodes (check)
table(V(g.lole)$type)    # from original graph g.lole
table(V(g.reduced)$type) # from newly created graph g.reduced
# Dropped about 1.1M edges (check)
table(E(g.lole)$type)    # from original graph g.lole
table(E(g.reduced)$type) # from newly created graph g.reduced

# IGNORE THIS PARAGRAPH
# # Inspect g.reduced graph
# #
# plot.neighborhood(g.lole,V(g.lole)['656934'])
# degree(g.lole,V(g.lole)['656934'])
# plot.neighborhood(g.reduced,V(g.reduced)['656934'])
# degree(g.reduced,V(g.reduced)['656934'])

# Recompute loan/lender indices, as they change when nodes are removed
# and an induced subgraph is created
#
reduced.loan.log   = V(g.reduced)$type=='loan'   
reduced.lender.log = V(g.reduced)$type=='lender' 
reduced.loan.ndx   = which(reduced.loan.log)
reduced.lender.ndx = which(reduced.lender.log)
# Verify
length(reduced.loan.ndx)
length(reduced.lender.ndx)
# There are still loans funded by many lenders
max(degree(g.reduced, V(g.reduced)[reduced.loan.ndx]))   
# Only those lenders remain that funded fewer than 100 loan
max(degree(g.reduced, V(g.reduced)[reduced.lender.ndx])) 

# Function get.edgelist returns the edge list created by 
# linking all lenders who fund the same loan
# Do this using the g.reduced graph in which lenders fund fewer than 100 loans
#
get.edgelist = function(node.list,n) {
  m = length(node.list)
  if( n<1 || m==1 ) {
    return(unlist(lapply(
      node.list, 
      function (a.node) { 
        get.full.neighborhood.edgelist(g.reduced,
                                       a.node)})))
  } else {
    p = trunc(m/2)
    return(unlist(mclapply(
      list(node.list[1    :p], 
           node.list[(p+1):m]),
      function(sub.list) { get.edgelist(sub.list,n-1) })))}}
system.time({
  new.edges = get.edgelist(reduced.loan.ndx, 5)
}) # 60s

### STOP HERE

# Get degree of lender nodes to count loans per lender
# Will use this later and compare it to lender.lender.degree
#
lender.loan.degree = degree(g.reduced, 
                            V(g.reduced)[reduced.lender.ndx])

# Add new lender-lender edges and create graph g.lender
#
system.time({
  g.lender = add.edges(g.reduced,
                       new.edges,
                       attr=list(type='lender-lender'))
}) # 40s

# Get degree of lender nodes to count fellow loaners per lender
#
lender.lender.degree = 
  degree(delete.edges(g.lender,
                      E(g.lender)[E(g.lender)$type=='loan-lender']),
         V(g.reduced)[reduced.lender.ndx])

# Look at degree vectors
#
length(lender.loan.degree)   # Verify
length(lender.lender.degree) # Verify
qplot(lender.loan.degree)
qplot(lender.loan.degree,log='y')
qplot(lender.lender.degree)
qplot(lender.loan.degree, 
      lender.lender.degree)

# Inspect new g.lender graph
#
table(E(g.lender)$type) # Verify
plot.neighborhood(g.lole,V(g.lole)['656934'])
plot.neighborhood(g.reduced,V(g.reduced)['656934'])
plot.neighborhood(g.lender,V(g.lender)['656934'])
# Notice multiple edges between two nodes
degree(g.lole,V(g.lole)['easternhillscommunity'])
# Notice that <easternhillscommunity> funded 122 loans
g.helena = induced.subgraph(g.lender,
                            neighbors(g.lender,
                                      V(g.lender)['656934']))
g.helena
V(g.helena)
E(g.helena)
E(g.helena)[incident(g.helena,V(g.helena)['helena7483'])]
plot(g.helena)
plot.neighborhood(g.helena,V(g.helena)['helena7483'])
# Notice the muliple edges between a single pair of vertices
plot.neighborhood(g.lole,V(g.lole)['helena7483'])
V(g.lole)[neighbors(g.lole,V(g.lole)['helena7483'])]
plot.neighborhood(g.lole,V(g.lole)['mike3223'])
V(g.lole)[neighbors(g.lole,V(g.lole)['mike3223'])]
mutual.loan = neighbors(g.lole,V(g.lole)['helena7483']) %in%
              neighbors(g.lole,V(g.lole)['mike3223'])
V(g.lole)[neighbors(g.lole,V(g.lole)['helena7483'])[mutual.loan]]


# Change parallel edges into a single weighted edge
#
E(g.lender)$weight <- 1 
g.lender.weighted = simplify(g.lender, edge.attr.comb=list(weight="sum"))

# Inspect g.lender.weighted graph
#
g.lender
g.lender.weighted
weighted.edges.log = # Find edges with weights over 1
  E(g.lender.weighted)$weight >  1
weighted.edges.ndx = which(weighted.edges.log)
table(weighted.edges.log)
length(weighted.edges.ndx)
E(g.lender.weighted)[ # Get three edges with weights over 1
  sample(weighted.edges.ndx,3)]$weight

E(g.lender.weighted)[6569690]
neighbors(g.reduced,V(g.reduced)['kevin8449'])
V(g.reduced)[neighbors(g.reduced,V(g.reduced)['jonathan8962'])]
overlap = neighbors(g.reduced,V(g.reduced)['kevin8449']) %in%
          neighbors(g.reduced,V(g.reduced)['jonathan8962'])
sum(overlap)

system.time({
  g.lender.only = # g.lender graph without loan nodes
    delete.vertices(g.lender,
                    V(g.lender)[which(V(g.lender)$type=='loan')])
}) # 20s

# Metrics START HERE
#
qplot(log10(degree(g.lender.only,V(g.lender.only))))

sdf = degree(g.lole,V(g.lole)[lender.ndx])
lkj = degree(g.lender.only, V(g.lender.only)[])

### 25 Jul 2014
###

# The second will take forever
#
table(V(g.lender)$type)
table(E(g.lender)$type)
# user  system elapsed 
# 486.357   6.358 492.516 

E(g.lender)[1:10]
lkj = edge.df[edge.df$lender_id=='gooddogg1','loan_id']

sdf = edge.df[edge.df$lender_id=='cathy48173479','loan_id']
sum(sdf %in% lkj)
sdf = edge.df[edge.df$lender_id=='elise6052','loan_id']
sum(sdf %in% lkj)

library(ggplot2)
qplot(log10(degree(g4,V(g4))))




