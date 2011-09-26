#
# K-means clustering algorithm
# Based on Hartigan, Wong (1979)
#

# Optimal-transfer and quick-transfer stage functions
source("optra.R")
source("qtran.R")

# Global variables
A <- matrix(rnorm(50*2), 50, 2) # Raw data
M <- length(A[,1]) # Number of points
N <- length(A[1,]) # Dimensions
K <- 5 # Number of clusters 
C <- matrix(0, K, N) # Cluster centres
IC1 <- rep(0, M) # 1st closest neighbor
IC2 <- rep(0, M) # 2nd closest neighbor
NC <- rep(0, K) # Number of points in each cluster
ITER <- 10 # Iterations

# Euclidean distance
distance <- function(a, b) {
  dist <- 0
  for(i in 1:length(a)) {
    dist <- dist + (a[i]-b[i])^2
  }
  return(sqrt(dist))
}

main<-function() {
  # Initialize the cluster centers
  for(k in 1:K) {
    for(n in 1:N) {
      C[k,n] <<- runif(1, min(A[,n]), max(A[,n]))
    }
  }

  # Step 1 - Closest neighbors  
  for(I in 1:M) {
    d <- rep(0, K)
    for(k in 1:K) {
      d[k] <- distance(A[I,], C[k,])
    }
    IC1[I] <<- order(d)[1]
    IC2[I] <<- order(d)[2]   
  }

  # Step 2 - Update cluster centres
  for(k in unique(IC1)) {
    for(n in 1:N) {
      C[k,n] <<- mean(A[,n][IC1==k])
    }
  }

  # Step 3 
  # Clusters already initiated into the live set

  # Step 4
  optra(rep(TRUE, K))
  print(IC1)
}

# Start the app
main()