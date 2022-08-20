# Paper: Pattern of frustration formation in the functional brain network
# DOI: https://doi.org/10.1162/netn_a_00268 
# Journal: Network Neuroscience
# Usage: FrustrationListbyNode.r
# Date: 8/20/2022
# Author: Majid Saberi
# Description: This function returns a list of frustrated triangles 
#              that a selected node is involved in them.

FrustrationListbyNode <- function(conn,node){ #connectiviy(adjacency) matrix and interesting node number
  dm <- dim(conn)[1] #number of available nodes
  othernodes <- seq(dm)[seq(dm) != node] #number of other nodes
  frustlist <- matrix(NA,nrow = 1,ncol = 3) #creating frustration list matrix
  colnames(frustlist) <- c("node1","node2","node3") #labeling columns
  #this loop explores every triangle that contains interesting node to find frustrations
  for(i in othernodes[1:(dm-2)]){ 
    for(j in othernodes[i:(dm-1)]){
      if( conn[node,i] *  conn[node,j] * conn[i,j] < 0 ) #frustrating detection
        frustlist <- rbind( frustlist,c(node,i,j) ) #adding triangle to frustration list
    }
  }
  frustlist <- frustlist[-1,] #clearing the list from NAs
  return(frustlist)
}

#execution for a sample signed matrix (it also works for weighted input matrix)
sample_mat <- matrix( sample(c(1,-1),size = 81,replace = T) , ncol=9,nrow=9)
sample_node <- 2
FrustrationListbyNode(sample_mat,sample_node)
