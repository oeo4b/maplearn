##
#
# Mapper for a simple linear regression
# Parameters estimated using ordinary least squares
#
##

all <- read.table(file("stdin"), header = F, sep = "\n")

# row/column lengths; delimit by ' '
all <- strsplit(as.character(all[,1]), " ")
n <- length(all)
m <- length(all[[1]])

# Design matrix
X <- matrix(1, n, m)
y <- rep(0, n)
for(i in 1:n) {
  y[i] <- as.numeric(all[[i]][1])
  for(j in 2:m) {
    X[i,j] <- as.numeric(all[[i]][j])
  }
}
 
# Ordinary least squares
A <- 0
b <- 0

for(i in 1:n) {
  A <- A + X[i,]%*%t(X[i,])
  b <- b + X[i,]*t(y[i])
}

# Stdout 
cat("A\t")
cat(A)
cat("\n")
cat("b\t")
cat(b)
cat("\n")