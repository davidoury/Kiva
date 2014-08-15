load('dataframes')
library(igraph)
library(ggplot2)
library(parallel)

# Create a dataframe of edges
#
edge.df = data.frame(loan_id  = as.character(loans.lenders.df$id), 
                     lender_id = as.character(loans.lenders.df$lender_ids), 
                     stringsAsFactors=FALSE)

# Create vector of unique loans and vector of unique lenders
#
loan.vector   = unique(edge.df$loan_id)
lender.vector = unique(edge.df$lender_id)

#  Create dataframe of loan vertices with type 'loan'
#
loan.node.df = data.frame(id=loan.vector, 
                          type='loan', 
                          stringsAsFactors=FALSE)
str(loan.node.df)
min(loan.node.df$id)
max(loan.node.df$id) # ???
table(loan.node.df$type)
rm(loan.vector)

#  Create dataframe of lender vertices with type 'lender'
#
lender.node.df = data.frame(id=lender.vector, 
                            type='lender', 
                            stringsAsFactors=FALSE)
str(lender.node.df)
min(lender.node.df$id)
max(lender.node.df$id)
table(lender.node.df$type)
rm(lender.vector)

# Create node.df from the dataframes of loan and lender nodes
#
node.df = rbind(loan.node.df, lender.node.df)
table(node.df$type) # Verify
rm(loan.node.df)
rm(lender.node.df)

# Remove duplicates <id> values from node.df
#
dup.ndx = duplicated(node.df$id)
keep.ndx = !dup.ndx
table(keep.ndx) # Check
node.df$id       [node.df$id         %in% node.df$id[dup.ndx]]
loan.node.df$id  [loan.node.df$id    %in% node.df$id[dup.ndx]]
lender.node.df$id[lender.node.df$id  %in% node.df$id[dup.ndx]]
node.df = node.df[keep.ndx,] # Keep the non-duplicates
sum(duplicated(node.df$id))  # Verify
rm(dup.ndx,keep.ndx)

# Verify edge.df and node.df
#
str(edge.df)
str(node.df)

# Create undirected graph from edge.df and node.df
# Nodes are loans and lenders
# Edges from each lender to each loan they fund
#
g.lole = graph.data.frame(d=edge.df,
                                 directed=FALSE, 
                                 vertices=node.df)
table(V(g.lole)$type) # Verify
rm(edge.df, node.df)

# All edges link loans and lenders
#
E(g.lole)$type = 'loan-lender'
table(E(g.lole)$type)

# Create vectors of loan nodes and lender nodes
#
lole.loan.ndx   = which(V(g.lole)$type == 'loan')
lole.lender.ndx = which(V(g.lole)$type == 'lender')
length(lole.loan.ndx)   # Verify
length(lole.lender.ndx) # Verify

# Verify that these vectors work as expected
#
V(g.lole)[lole.loan.ndx[1:10]]$name
V(g.lole)[lole.lender.ndx[1:10]]$name

# Get degree for all lender nodes
#
lole.loan.deg = degree(g.lole, V(g.lole)[lole.loan.ndx])
length(lole.loan.deg)
qplot(log10(lole.loan.deg))
table(lole.loan.deg<100)

# Get degree for all loan nodes
#
lole.lender.deg = degree(g.lole, V(g.lole)[lole.lender.ndx])
length(lole.lender.deg)
qplot(log10(lole.lender.deg))
table(lole.lender.deg<100)

# Get lenders who funded fewer than 100 loans
#
lole.lender.degree.log = lole.lender.deg<100
lole.lender.degree.ndx = which(lole.lender.degree.log)
# Find max degree of all lender nodes of degree less than 100 (Verify)
max(degree(g.lole,
           V(g.lole)[lole.lender.ndx[lole.lender.degree.ndx]]))

# Get loans with fewer than 100 lenders
#
lole.loan.degree.log = lole.loan.deg<100
lole.loan.degree.ndx = which(lole.loan.degree.log)
# Find max degree of all loan nodes of degree less than 100 (Verify)
max(degree(g.lole, # Check
           V(g.lole)[lole.loan.ndx[lole.loan.degree.ndx]]))
degree(g.lole, # Use
       V(g.lole)[lole.lender.ndx[lole.lender.degree.ndx[1:10]]])

# Function to plot the induced subgraph from a node and its neighborhood
#
plot.neighborhood = function(g,a.node) {
  a.neighborhood = neighbors(g,a.node)
  sg = induced.subgraph(g,c(a.node,a.neighborhood))
  plot(sg,vertex.size=1)
}
# Examples
plot.neighborhood(g.lole,V(g.lole)['656901'])
plot.neighborhood(g.lole,V(g.lole)['zingo9373']) # MISSING???
plot.neighborhood(g.lole,V(g.lole)['stephen4774']) 

# Function to create edges between all nodes of a neighborhood
# The returned edgelist is a single vector of nodes
# Edges go from odd numbered nodes the next node in the vector
#
get.full.neighborhood.edgelist = function(g,a.node) { # a.node=a.loan
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
V(g.lole)[get.full.neighborhood.edgelist(g.lole,'elisabeth1514')]
plot.neighborhood(g.lole,V(g.lole)['656901'])
V(g.lole)[get.full.neighborhood.edgelist(g.lole,'656901')]

# Create graph with only those lenders who fund fewer than 100 loans
#
g.reduced = 
  induced.subgraph(g.lole, 
                   c(V(g.lole)[lole.loan.ndx],
                     V(g.lole)[lole.lender.ndx[lole.lender.degree.ndx]]))
table(V(g.reduced)$type)
table(E(g.reduced)$type)

# Inspect g.reduced graph
# The only difference is that some lenders have been removed
#
plot.neighborhood(g.lole,V(g.lole)['656934'])
plot.neighborhood(g.reduced,V(g.reduced)['656934'])

# Recompute loan/lender indices
# When the number of vertices changes they get renumbered
#
reduced.loan.log   = V(g.reduced)$type=='loan'   # Nodes were renumbered 
reduced.lender.log = V(g.reduced)$type=='lender' # 
reduced.loan.ndx   = which(reduced.loan.log)
reduced.lender.ndx = which(reduced.lender.log)
length(reduced.loan.ndx)
length(reduced.lender.ndx)
max(degree(g.reduced, V(g.reduced)[reduced.loan.ndx]))   # Verify
max(degree(g.reduced, V(g.reduced)[reduced.lender.ndx])) # Verify

# Get the edge list created by linking lenders who fund the same loan
# using the g.reduced graph in which lenders fund fewer than 100 loans
#
# First, define the function
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
# Second, use it
system.time({
  new.edges = get.edgelist(reduced.loan.ndx,5)
}) # 52s

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




