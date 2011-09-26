#
#
#

optra<-function(LS) {
  for(I in 1:M) {
    L1 <- IC1[I]
    if(L1%in%which(LS)) {
      # Step 4a
      R2 <- {}
      include <- (1:K)[(1:K)!=L1] 

      for(L in include) {
        R2 <- c(R2, (sum(IC1==L)*distance(A[I,], C[L,])^2) / (sum(IC1==L) + 1))
      }

      L2 <- order(R2)[1]
      a <- ifelse(sum(IC1==L1)==1, 0, ((sum(IC1==L1)*distance(A[I,], C[L1,])^2) / (sum(IC1==L1) - 1)))
      if(min(R2)>=a) {
        # No reallocation
        IC2[I] <<- L2
      }
      else {
        IC2[I] <<- L1
        IC1[I] <<- L2

        for(k in c(L1, L2)) {
          for(n in 1:N) {
            C[k,n] <- mean(A[,n][IC1==k])
          }
        }
      }
    }
    else {
      # Step 4b
      R2 <- {}
      include <- LS 

      for(L in include) {
        R2 <- c(R2, (sum(IC1==L)*distance(A[I,], C[L,])^2) / (sum(IC1==L) + 1))
      }

      L2 <- order(R2)[1]
      a <- ifelse(sum(IC1==L1)==1, 0, ((sum(IC1==L1)*distance(A[I,], C[L1,])^2) / (sum(IC1==L1) - 1)))
      if(min(R2)>=a) {
        # No reallocation
        IC2[I] <<- L2
      }
      else {
        IC2[I] <<- L1
        IC1[I] <<- L2

        for(k in c(L1, L2)) {
          for(n in 1:N) {
            C[k,n] <- mean(A[,n][IC1==k])
          }
        }
      }
    }
  }

}

