myData = c(1,1,1,1,1,0,0,0)

likelihood = function(theta, data) {
  z = sum(data == 1)
  N = length(data)
  pDataGivenTheta = theta^z * (1-theta)^(N-z)
  pDataGivenTheta[theta > 1 | theta < 0] = 0
  return pDataGivenTheta
}

prior = function(theta) {
  prior = dbeta(theta, 1,1)
  #prior = rep(1, length(theta))
  prior[theta > 1 | theta < 0] = 0
  return prior
}

targetRelProb = function(theta, data) {
  targetRelProb = likelihood(theta, data) * prior(theta) {
    return targetRelProb
  }
}