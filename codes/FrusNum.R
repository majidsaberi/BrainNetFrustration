# Paper: Pattern of frustration formation in the functional brain network
# Usage: FrusNum.r
# Date: 5/6/2022
# Author: Majid Saberi
# Description: This function receives a signed connectivity matrix and returns
#              the number of presented frustrations.

FrusNum <- function(mat) {  #mat is a square signed matrix
  dm <- dim(mat)[1]  #dimension of the matrix
  frust <- 0  #number of frustrations
   #permuting for all triadic relations
   for( i in 1:(dm-2)){
     for( j in (i+1):(dm-1)){
       for( k in (j+1):dm){
         y <- mat[i,j]*mat[i,k]*mat[j,k]  #multiplication of triad links
         if( y < 0 )  #considering negative values as frustration 
           frust <- frust + 1  #counting frustrations
       }
     }
   }
  return(frust)
}

#execution for a sample signed matrix
sample_mat <- matrix( c(-1,1) , ncol=9,nrow=9)
FrusNum(sample_mat)

