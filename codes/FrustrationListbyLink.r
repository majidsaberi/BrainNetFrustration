# Paper: Pattern of frustration formation in the functional brain network
# DOI: https://doi.org/10.1162/netn_a_00268 
# Journal: Network Neuroscience
# Usage: FrustrationListbyLink.r
# Date: 8/20/2022
# Author: Majid Saberi
# Description: This function returns a list of frustrated triangles 
#              that a selected link is involved in them.

FrustrationListbyLink <- function(conn,node1,node2){ #connectiviy(adjacency) matrix and interesting connected nodes
  dm <- dim(conn)[1] #number of available nodes
  othernodes <- seq(dm)[!seq(dm) %in% c(node1,node2)] #number of other nodes
  frustlist <- matrix(NA,nrow = 1,ncol = 3) #creating frustration list matrix
  colnames(frustlist) <- c("node1","node2","node3") #labeling columns
  #this loop explores every triangle that contains interesting link to find frustrations
  for(i in othernodes){ 
      if( conn[node1,node2] *  conn[node1,i] * conn[node2,i] < 0 ) #frustrating detection
        frustlist <- rbind( frustlist,c(node1,node2,i) ) #adding triangle to frustration list
  }
  frustlist <- frustlist[-1,] #clearing the list from NAs
  return(frustlist)
}

#execution for a sample signed matrix (it also works for weighted input matrix)
sample_mat <- matrix( sample(c(1,-1),size = 81,replace = T) , ncol=9,nrow=9)
sample_node1 <- 2
sample_node2 <- 3
FrustrationListbyLink(sample_mat,sample_node1,sample_node2)
