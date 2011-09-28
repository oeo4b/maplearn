#
# Quick-transfer stage
#

qtran<-function(LS) {
  RLS <- rep(FALSE, K)
  for(I in 1:M) {
    L1 <- IC1[I]
    L2 <- IC2[I]

    if((L1%in%which(LS))|(L2%in%which(LS))) {
      R1 <- ifelse(sum(IC1==L1)==1, 0, (sum(IC1==L1)*distance(A[I,], C[L1,])^2) / (sum(IC1==L1) - 1))
      R2 <- (sum(IC1==L2)*distance(A[I,], C[L2,])^2) / (sum(IC1==L2) + 1)

      if(R1<R2) {}
      else {
        IC1[I] <<- L2
        IC2[I] <<- L1

        RLS[L1] <- TRUE
        RLS[L2] <- TRUE   
   
        for(n in 1:N) {
          C[L2,n] <<- mean(A[,n][IC1==L2])
          C[L1,n] <<- mean(A[,n][IC1==L1])
        }    
      }
    }
  }
  return(RLS)
}