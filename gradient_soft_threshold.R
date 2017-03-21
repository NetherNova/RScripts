# For complete weight vector soft-thresholding
soft_threshold <- function(vector, gamma) {
  n = length(vector)
  result = rep(0, n)
  for(i in 1:n) {
    if (vector[i] > gamma)
    {
      result[i] = vector[i] - gamma
    }
    else if (abs(vector[i]) <= gamma)
    {
      result[i] = 0
    }
    else
    {
      result[i] = vector[i] + gamma
    }
  }
  return(result)
}

#  For single variable soft-thresholding
soft_threshold_single <- function(value, lambda) {
  if (value > lambda)
  {
    return (value - lambda)
  }
  else if (abs(value) <= lambda)
  {
    return (0)
  }
  else
  {
    return (value + lambda)
  }
}

degree_matrix <- function(m)
{
  x = colSums(m)
  n = ncol(m)
  return (diag(x, n, n))
}

# Deprecated version: Uses gradient-descent approach with Graph Lasso 2L no L1 penalty
graph_lasso <- function(X, y, n_iter, similarity_matrix, soft)
{
  #set.seed(34)
  learning_rate = 0.0001
  ncol = dim(X)[2]
  n = dim(X)[1]
  w = c(rnorm(ncol)) # initialize each weight to random N(0,1)
  lambda = 0.01
  alpha = 0.1
  print(paste("init weight:", w))
  
  i_error = rep(0, n_iter)
  
  for (i in 1:n_iter)
  {
    i_error[i] = sqrt(1/n * sum((y - X%*%w)^2))
    w = w - learning_rate*((-t(X)%*%(y - X%*%w)) + (1-alpha)*2*((t(similarity_matrix)+similarity_matrix)%*%w))
    if(soft==T)
    {
      w = soft_threshold(w, alpha)
    }
  }
  print(paste("weights updated:", w))
  print(paste("Final error: ", sqrt((1/n * sum((y - X%*%w)^2)))))
  plot(1:n_iter, i_error, type = 'l')
  convergence = min(which(abs(diff(i_error, 1)) <= 0.01))
  abline(v= convergence, col=c('red'))
  text(x = convergence, y = 2*i_error[convergence], labels=paste("Converged at: ", convergence))
}

grake_lasso <- function(X, y, n_iter, similarity_matrix)
{
  #set.seed(34)
  learning_rate = 0.0001
  ncol = dim(X)[2]
  n = dim(X)[1]
  w = c(rnorm(ncol)) # initialize each weight to random N(0,1)
  lambda = 0.9
  alpha = 0.5
  print(paste("init weight:", w))
  
  SVD = svd(similarity_matrix)
  print(SVD)
  S = SVD$u %*% diag(sqrt(SVD$d))
  print(S)
  print(sqrt(SVD$d))
  
  X_new = rbind(X, sqrt(1 - alpha) * S)
  X_new = sqrt(1 + lambda * (1 - alpha)) * X_new
  y_new = c(y, rep(0, ncol))
     
  i_error = rep(0, n_iter)
  
  for (i in 1:n_iter)
  {
    i_error[i] = sqrt(1/n * sum((y - X%*%w)^2))
    for (j in 1:ncol)
    {
      norm_j = norm(X[,j], type="2")
      w[j] = (t(X[, j])%*%(y - X[, -j]%*%w[-j]) / (t(X[, j]) %*% X[, j]))
      w[j] = soft_threshold_single(w[j], (lambda*alpha / (sqrt(1 + lambda * (1 - alpha)))) / (norm_j))
    }
  }
  print(paste("weights updated:", w))
  print(paste("Final error: ", sqrt((1/n * sum((y - X%*%w)^2)))))
  plot(1:n_iter, i_error, type = 'l')
  convergence = min(which(abs(diff(i_error, 1)) <= 0.001))
  abline(v= convergence, col=c('red'))
  text(x = convergence, y = 2*i_error[convergence], labels=paste("Converged at: ", convergence))
}

# For logistic regression
log_graph_lasso <- function(X, y, n_iter, similarity_matrix, soft)
{
  #set.seed(34)
  learning_rate = 0.0005
  ncol = dim(X)[2]
  n = dim(X)[1]
  w = c(rnorm(ncol)) # initialize each weight to random N(0,1)
  lambda = 0.1
  alpha = 100
  print(paste("init weight:", w))
  
  i_error = rep(0, n_iter)
  
  for (i in 1:n_iter)
  {
    i_error[i] = sqrt(1/n * sum((y - X%*%w)^2))
    class = 1/(1+exp(-1*(w%*%t(X))))
    w_up = w - learning_rate*(t(X)%*%(y - t(class))) # + (1-alpha)*2*((t(similarity_matrix)+similarity_matrix)%*%w))
    if(soft==T)
    {
      w = soft_threshold(w_up, alpha)
    }
    else
    {
      w = w_up
    }
  }
  print(paste("weights updated:", w))
  print(paste("Final error: ", (1/n * sum((y - X%*%w)^2))))
  plot(1:n_iter, i_error, type = 'l')
  convergence = min(which(abs(diff(i_error, 1)) <= 0.01))
  abline(v= convergence, col=c('red'))
  text(x = convergence, y = 2*i_error[convergence], labels=paste("Converged at: ", convergence))
}

##TEST##
X = matrix(c(1,2,3,1.5,1.7,1.8,2,2,2,3.5,3.7,3.8,1,2,3,1.4,1.5,1.6,2,2,2,3.5,3.7,3.8,1,2,3,1.4,1.5,1.6), nrow=10)
y = c(1,1,1,2,3,4,1,2,3,4)
n_iter = 200
similarity_matrix = matrix(c(1,0.5,0.3,0.5,1,0.1,0.3,0.1,1), nrow=3)
grake_lasso(X, y, n_iter, similarity_matrix)
glm.fit(x, y)
