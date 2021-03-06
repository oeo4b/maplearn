# Naive Bayes classifier for discrete variables
# Assume conditionally independent
# P(X1...Xn|Y) = P(X1|Y)...P(Xn|Y)
# Use Bayes rule and maximize over all possible classifiers
# Y <- argmax P(Y=yk) * P(Xi|Y=yk)...P(Xn|Y=yk)

# To stdout, in order (sum and length for each prob):
# Map a-priori probs -- P(Y=y1)...P(Y=yk)
# Map conditional probs -- P(X1|Y=y1)...P(Xn|Y=yk)

# Delimit by ' '
all <- read.table(file("stdin"), header=F, sep="\n")
all <- strsplit(as.character(all[,1]), " ")

# Row i, column j
# First column is response
n <- length(all)
m <- length(all[[1]])

# Set up a matrix
X <- matrix(0, n ,m-1)
y <- rep(0, n)
for(i in 1:n) {
  y[i] <- all[[i]][1]
  for(j in 2:m) {
    X[i,j-1] <- all[[i]][j]
  }
}

# Stdout response sum and length
for(i in unique(y)) {
  cat("P(y = ", i, ")\t", sum(y==i), " ", length(y), "\n", sep="")
}

# Stdout predictors sum and length
for(i in 1:(m-1)) {
  for(j in unique(X[,i])) {
    for(k in unique(y)) {
      cat("P(x", i, " = ", j, " | y = ", k, ")\t", sum(X[,i][y==k]==j), " ", length(X[,i][y==k]), "\n", sep="")
    }
  }
}