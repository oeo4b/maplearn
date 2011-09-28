#
# Reducer -- Calculate weighted averages
#

#Stdin
all <- read.table(file("stdin"), header = F, sep="\n")
M <- length(all[,1])
N <- 0
L <- rep(0, M)
TL <- rep(0, M)
C <- {}

for(i in 1:M) {
  r <-strsplit(as.character(all[i,]), "\t")[[1]]
  L[i] <- as.numeric(r[1])
  TL[i] <- as.numeric(r[2])
  r <- strsplit(r[3], " ")[[1]]
  N <- length(r)
  for(j in 1:N) {
    C <- c(C, as.numeric(r[j]))
  }
}
C <- matrix(C, M, N, byrow=T)

main<-function() {
  # data.frame L, M, C
  K <- unique(L)

  # Partial sums
  S <- C*TL

  # Weighted averages
  for(k in K) {
    cat(k, "\t", sep="")
    for(n in 1:N) {
      cat(mean(S[which(L==k),n]) / sum(TL[L==k]), " ", sep="")
    }
    cat("\n")
  }
}

# Start the app
main()