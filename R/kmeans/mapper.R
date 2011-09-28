#
# Mapper
# K-means clustering algorithm
# Optional optimal and quick transfer stages - optra.R, qtran.R
#

# Stdin
all <- read.table(file("stdin"), header = F, sep="\n")
M <- length(all[,1])
N <- length(strsplit(as.character(all[1,]), " ")[[1]])
A <- matrix(0, M, N)

for(i in 1:M) {
  r <- strsplit(as.character(all[i,]), " ")[[1]]
  for(j in 1:N) {
    A[i,j] <- as.numeric(r[j])
  }
}
K <- 10 # Number of clusters 

# Global transfer variables
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
  # Manually set the starting centers
  TBL <<- read.table(file="centers")
  
  for(i in 1:K) {
    for(j in 1:N) {
      C[i,j] <<- as.numeric(TBL[i,j])
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

  # Stdout -- Averages and points per cluster
  for(k in 1:K) {
    cat(k, "\t", sum(IC1==k), "\t", sep="")
    cat(C[k,])
    cat("\n", sep="")
  }
}

# Start the app
main()

