# Paper: Pattern of frustration formation in the functional brain network
# Usage: RegionalContr.r
# Date: 5/6/2022
# Author: Majid Saberi
# Description: This function receives a signed connectivity matrix and returns
#              the contribution of each node in frustration formation.

RegionalContr <- function(mat) {  #mat is a square signed matrix
  dm <- dim(mat)[1]  #dimension of the matrix
  contr <- vector(length = dm , mode = "numeric")  #contribution vector
  #permuting for all triadic relations
  for( i in 1:(dm-2)){
    for( j in (i+1):(dm-1)){
      for( k in (j+1):dm){
        y <- mat[i,j]*mat[i,k]*mat[j,k]  #multiplication of triad links
        if( y < 0 ){  #considering negative values as frustration 
          #adding node contribution 
          contr[i] <- contr[i] + 1
          contr[j] <- contr[j] + 1
          contr[k] <- contr[k] + 1
        }
      }
    }
  }
  return(contr)
}

#execution for a sample signed matrix
sample_mat <- matrix( sample(c(1,-1),size = 81,replace = T) , ncol=9,nrow=9)
RegionalContr(sample_mat)
