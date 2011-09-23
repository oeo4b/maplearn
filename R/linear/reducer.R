##
#
# Mapper for a simple linear regression
# Parameters estimated using ordinary least squares
#
##

# Stdin
all <- read.table(file("stdin"), header = F, sep = "\n")
all <- strsplit(as.character(all[,1]), "\t")

# Count key/value pairs
n <- length(all)
A <- 0
b <- 0
for(i in 1:length(all)) {
  key <- as.character(all[[i]][1])
  values <- strsplit(as.character(all[[i]][2]), " ")[[1]]
  if(key=="A") {
    A <- A + matrix(as.numeric(values), sqrt(length(values)), sqrt(length(values)))
  }
  if(key=="b") {
    b <- b + as.numeric(values)
  }
}

# Stdout
cat("A\t")
cat(A)
cat("\n")
cat("b\t")
cat(b)
cat("\n")