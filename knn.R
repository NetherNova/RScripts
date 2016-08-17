#With reference to Statistical Computing with R - By Maria L. Rizzo
library(RANN)
library(boot)

knnStatistics = function(Z, ix, sizes) {
  k = 4
  n1 = sizes[1]
  n2 = sizes[2]
  Z = Z[ix,]
  nearest = nn2(Z,Z)
  test = (nearest$nn.idx <= n1)
  T_k = 0
  
  for(i in 2:k)
  {
    T_k = T_k + sum(test[,1] == test[,i])   
  }
  return (T_k / ((n1+n2) * k))
}

#z = rbind(x,y)
#o = rep(0, nrow(z))

#DATA = data.frame(cbind(z,o))

#k = knn(x, y, rep(0, nrow(x)), k = 1, algorithm ="cover_tree")
#indices = attr(k, "nn.index")
#print k nearest neighbors of row 3
#dist(z)
#print(indices[3, ])

#X = matrix(rnorm(1200, mean = 0, sd = 0.5), 300, 4)
#Y = matrix(rnorm(1200, mean = 0, sd = 0.5), 300, 4)
X = matrix(c(1, 2.3, 80.5, 51.0, 1, 
             0, 2.4, 80.0, 50.0, 0,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1,
             0, 2.3, 80.8, 50.9, 1), 
           12, 5)
Y = matrix(c(0, 2.5, 80.1, 50.0, 0, 
             1, 2.3, 80.5, 51.0, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0,
             1, 2.4, 80.3, 50.2, 0), 
           12, 5)
Z = rbind(X,Y)
n1 = nrow(X)
n2 = nrow(Y)
N = c(n1, n2)
#This is a pooled hypothesis test for the two sample problem (not optimal, more data would help) 
boot.obj = boot(data=Z, statistic=knnStatistics, sim="permutation", R=999, sizes = N)
tb = c(boot.obj$t, boot.obj$t0)
p = mean(tb >= boot.obj$t0)
print(p)
hist(tb, freq=F, main="", xlab="reps of T_n,k")
points(boot.obj$t0, 0, cex=1, pch=16)