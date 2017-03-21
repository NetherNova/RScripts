par(mfrow = c(2,2))

x <- y <- seq(-1, 1, 0.1)

alpha = 0.3

#RIDGE Regression
#r = outer(x^2, y^2, "+")
#contour(r, drawlabels=F)

#Lasso Regression
colnames(r) = seq(-1, 1, 0.1)
rownames(r) = seq(-1, 1, 0.1)
r = outer(abs(x), abs(y), "+")
contourplot(r, xlab="w1", ylab="w2")

#Grake Lasso 0.2 similar
L = matrix(c(0, 0.2, 0.2, 0), nrow=2) # Kernel
myFun = function(a, b) { z <- c(a, b); z %*% (diag(rowSums(L)) - L) %*% z}
r = alpha * outer(abs(x), abs(y), "+") + (1-alpha) * outer(x, y, FUN = Vectorize(myFun))
colnames(r) = seq(-1, 1, 0.1)
rownames(r) = seq(-1, 1, 0.1)
contourplot(r, xlab="w1", ylab="w2")

#Grake Lasso 0.7 similar
L = matrix(c(0, 0.5, 0.5, 0), nrow=2) # Kernel
myFun = function(a, b) { z <- c(a, b); z %*% (diag(rowSums(L)) - L) %*% z}
r = alpha * outer(abs(x), abs(y), "+") + (1-alpha) * outer(x, y, FUN = Vectorize(myFun))
colnames(r) = seq(-1, 1, 0.1)
rownames(r) = seq(-1, 1, 0.1)
contourplot(r, xlab="w1", ylab="w2")

#Grake Lasso 0.7 similar
L = matrix(c(0, 0.8, 0.8, 0), nrow=2) # Kernel
myFun = function(a, b) { z <- c(a, b); z %*% (diag(rowSums(L)) - L) %*% z}
r = alpha * outer(abs(x), abs(y), "+") + (1-alpha) * outer(x, y, FUN = Vectorize(myFun))
colnames(r) = seq(-1, 1, 0.1)
rownames(r) = seq(-1, 1, 0.1)
contourplot(r, xlab="w1", ylab="w2")

