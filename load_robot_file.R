# Load robot data

robo = read.csv("D:\\Dissertation\\Data Sets\\Robot Execution Failure\\lp1.data", sep="\t", header=F)
n_seq = sum(as.numeric(robo$V1 != ""))
robo$sequence = 1:nrow(robo)

for(i in 1:n_seq)
{
  j = ((i-1) * 16)
  if(j >= nrow(robo))
  {
    break
  }
  label = robo[j, 1]
  while(robo[j+2, 1] == "")
  {
    robo[j, 1] = label
    robo$sequence[j] = i
    j = j + 1
  }
}