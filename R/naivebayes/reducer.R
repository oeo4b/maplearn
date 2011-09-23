# Naive Bayes classifier for discrete variables
# Assume conditionally independent
# P(X1...Xn|Y) = P(X1|Y)...P(Xn|Y)
# Use Bayes rule and maximize over all possible classifiers
# Y <- argmax P(Y=yk) * P(Xi|Y=yk)...P(Xn|Y=yk)

# To stdout, in order (sum and length for each prob):
# Map a-priori probs -- P(Y=y1)...P(Y=yk)
# Map conditional probs -- P(X1|Y=y1)...P(Xn|Y=yk)

# Delimit by "\t"
all <- read.table(file("stdin"), header=F, sep="\n")
all <- strsplit(as.character(all[,1]), "\t")

# Row i, column j
# First column is response
n <- length(all)
key <- rep(0, n)
value <- matrix(0, n, 2)

# Fit the key => value pair into seperate matrices
for(i in 1:n) {
  key[i] <- all[[i]][1]
  for(j in 1:2) {
    value[i,j] <- strsplit(all[[i]][2]," ")[[1]][j]
  }
}

# Calculate the probs
for(i in unique(key)) {
  cat(i, "\t", as.character(sum(as.numeric(value[,1][key==i])) / sum(as.numeric(value[,2][key==i]))), "\n", sep="")
}