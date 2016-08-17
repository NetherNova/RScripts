library(corrplot)
library(ggplot2)
require(gridExtra)

dall = read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\automation2.csv")
# labels = read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\labels.csv")
remove_cols = c(68)
dall = dall[, -remove_cols]

not_motor = c("ns.4.s.PC.Station.WinLC.RTX.SITASTER600101", 
              "ns.4.s.PC.Station.WinLC.RTX.TEMP",
              "ns.4.s.PC.Station.WinLC.RTX.STW_aus2",
              "ns.4.s.PC.Station.WinLC.RTX.STW_fuehrung_plc",
              "ns.4.s.PC.Station.WinLC.RTX.STW_Betrieb_freigabe",
              "ns.4.s.PC.Station.WinLC.RTX.ET200_2",
              "ns.4.s.PC.Station.WinLC.RTX.SILICHT300102",
              "ns.4.s.PC.Station.WinLC.RTX.SIGNAL_YELLOW",
              "ns.4.s.PC.Station.WinLC.RTX.ET200_1",
              "ns.4.s.PC.Station.WinLC.RTX.SL500101A1",
              "ns.4.s.PC.Station.WinLC.RTX.CU_Stoerung",
              "ns.4.s.PC.Station.WinLC.RTX.CU_ElektronikTemp",
              "ns.4.s.PC.Station.WinLC.RTX.SILICHT300103",
              "ns.4.s.PC.Station.WinLC.RTX.SL500101ANALOG",
              "ns.4.s.PC.Station.WinLC.RTX.SIGNAL_GREEN",
              "ns.4.s.PC.Station.WinLC.RTX.SIGNAL_ALARM",
              "ns.4.s.PC.Station.WinLC.RTX.STW_HLG_start",
              "ns.4.s.PC.Station.WinLC.RTX.STW_aus3",
              "ns.4.s.PC.Station.WinLC.RTX.STW_HLG_freigabe",
              "ns.4.s.PC.Station.WinLC.RTX.SISONAR700103",
              "ns.4.s.PC.Station.WinLC.RTX.STW_Quit",
              "ns.4.s.PC.Station.WinLC.RTX.SL500101A2",
              "ns.4.s.PC.Station.WinLC.RTX.SILICHT300104",
              "ns.4.s.PC.Station.WinLC.RTX.SIGNAL_RED",
              "ns.4.s.PC.Station.WinLC.RTX.ET200_1_OUT",
              "ns.4.s.PC.Station.WinLC.RTX.ET200_2_OUT",
              "ns.4.s.PC.Station.WinLC.RTX.SISONAR700104"
              )

indx = sapply(dall, is.factor)
dall[indx] <- lapply(dall[indx], function(x)
    { ifelse(x == 'false', 0, 1) }
  )

d1 = dall[, (colnames(dall) %in% not_motor)]
d2 = dall[, !(colnames(dall) %in% not_motor)]
x1 = data.frame(scale(d1, center=T, scale=T))
x2 = data.frame(scale(d2, center=T, scale=T))

removeAllNanCols <- function(df) {
  indx = lapply(df, function(x) all(is.nan(x)))
  bls = c()
  for(b in indx) {bls = c(bls, b)}
  df = df[, !bls]
  df = as.matrix(df)
}
x1 = removeAllNanCols(x1)
x2 = removeAllNanCols(x2)
#color = d1[, 13] < 1118200000
#ggplot(d1, aes(x = d1[, 13], y = d1[, 14], color = color, xlab="L1", ylab="P")) + geom_point(size = 5)
svd = svd(x1)
km = kmeans(x1, centers = 3)
svd_plotting = data.frame(matrix(c(svd$u[, 1], svd$u[, 2], km$cluster), nrow = length(km$cluster)))
plot1 <- ggplot(svd_plotting, aes(x = X1, y = X2, color = X3)) + geom_point(size = 5)

svd = svd(x2)
km = kmeans(x2, centers = 3)
svd_plotting = data.frame(matrix(c(svd$u[, 1], svd$u[, 2], km$cluster), nrow = length(km$cluster)))
#                                   c(1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,0,0,0,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2,2)), nrow = length(km$cluster)))
plot2 <- ggplot(svd_plotting, aes(x = X1, y = X2, color = X3)) + geom_point(size = 5)
grid.arrange(plot1, plot2, ncol=2)
### Gene expressions ###
# viz = read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\labels.csv")
# x = as.matrix(scale(viz[, 0:79], center=T, scale=T))
# svd = svd(x)
# svd_plotting = data.frame(matrix(c(svd$u[, 1], svd$u[, 2], viz$label), nrow=length(viz$label)))
# ggplot(svd_plotting, aes(x = X1, y=X2, color = X3)) + geom_point(size=5)