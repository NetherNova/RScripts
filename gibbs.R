n = 1
a1 = 2
b1 = 2
theta1 = 0.5
theta2 = 0.5
D = c(0,0,0,0,0,0,0,0,1,1,1,1)

for (i in 1:n)
{
  N = length(D)
  z1 = sum(D==1)
  print(N)
  print(z1)
  
  p_theta1_given_theta2 = theta1^(z1 + a1 - 1) * (1 - theta1)^(N - z1 + b1 - 1)
  theta1 = p_theta1_given_theta2
  print(theta1)
  
  p_theta1_given_theta2 = theta1^(z1 + a1 - 1) * (1 - theta1)^(N - z1 + b1 - 1)
  theta2 = p_theta1_given_theta2
  print(theta2)
}