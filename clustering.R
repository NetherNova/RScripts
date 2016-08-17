library(corrplot)
library(ggplot2)
require(gridExtra)

d1=read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\automation.csv")
d2=read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\automation2.csv")
vars = c("ns.4.s.PC.Station.WinLC.RTX.Sentron_MW_Qt_L3", 
         "ns.4.s.PC.Station.WinLC.RTX.Sentron_S_ALL", "ns.4.s.PC.Station.WinLC.RTX.Sentron_Spannung_L1_N",
          "ns.4.s.PC.Station.WinLC.RTX.Sentron_STW", "ns.4.s.PC.Station.WinLC.RTX.SL500101ANALOG",
          "ns.4.s.PC.Station.WinLC.RTX.CU_Drehmoment", "ns.4.s.PC.Station.WinLC.RTX.Ist_Drehzahl", 
         "ns.4.s.PC.Station.WinLC.RTX.Sentron_Spannung_L2_N", "ns.4.s.PC.Station.WinLC.RTX.CU_WirkleistungAB",
         "ns.4.s.PC.Station.WinLC.RTX.Sentron_Strom_L2", "ns.4.s.PC.Station.WinLC.RTX.Sentron_Frequenz",
         "ns.4.s.PC.Station.WinLC.RTX.CU_Motortemp", "ns.4.s.PC.Station.WinLC.RTX.CU_ElektronikTemp")

# x = d2[sapply(d2, is.numeric)]
# C = cor(x)
# corrplot(C)
# cols = sapply(d2, is.character)
# d2[,cols] = lapply(d2[,cols], as.numeric)
x = as.matrix(scale(d2[, vars], center=T, scale=T))

svd = svd(x)
km = kmeans(x, centers = 3)
svd_plotting = data.frame(matrix(c(svd$u[, 1], svd$u[, 2], km$cluster), nrow = length(km$cluster)))
plot1 <- ggplot(svd_plotting, aes(x = X1, y = X2, color = X3)) + geom_point(size = 5)
svd_plotting = data.frame(matrix(c(svd$u[, 1], svd$u[, 2], 
                                   c(1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)), nrow = length(km$cluster)))
plot2 <- ggplot(svd_plotting, aes(x = X1, y = X2, color = X3)) + geom_point(size = 5)
grid.arrange(plot1, plot2, ncol=2)
### Gene expressions ###
viz = read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\labels.csv")
x = as.matrix(scale(viz[, 0:79], center=T, scale=T))
svd= svd(x)
svd_plotting = data.frame(matrix(c(svd$u[, 1], svd$u[, 2], viz$label), nrow=length(viz$label)))
ggplot(svd_plotting, aes(x = X1, y=X2, color = X3)) + geom_point(size=5)
ggplot(svd_plotting, aes(x = X1, y=X2, color = c(rep(1, 199), rep(2, 136)))) + geom_point(size=5)