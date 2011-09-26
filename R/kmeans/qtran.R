#
#
#

qtran<-function(LS) {
  for(I in 1:M) {
    L1 <- IC1[I]
    L2 <- IC2[I]

    R1 <- (sum(IC1==L1)*distance(A[I,], C[L1,])^2) / (sum(IC1==L1) - 1)
    R2 <- (sum(IC1==L2)*distance(A[I,], C[L2,])^2) / (sum(IC1==L2) + 1)

    if(R1<R2) {

    }
    else {
      IC1[I] <- L2
      IC2[I] <- L1
      
      for(n in 1:N) {
        C[L2,n] <<- mean(A[,n][IC1[I]==L2])
        C[L1,n] <<- mean(A[,n][IC1[I]==L1])
      }    
    }
  }
}