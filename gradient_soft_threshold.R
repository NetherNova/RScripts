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

degree_matrix <- function(m)
{
  x = colSums(m)
  n = ncol(m)
  return (diag(x, n, n))
}

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