# library(corrplot)
# 
# M=cor(data)
# corrplot(M, type="lower", tl.pos="ld", tl.col = "black", tl.srt=25)
par(mfrow=c(2,1))
library(lars)
par(mfrow=c(1,2))
#Lines Overfitting
dot_col="black"
col1="deepskyblue3"
col2="brown2"
n_y = c(1:20)*0.5+2*rnorm(20) # replace with other time series
#n_y[10] = 8
variables=c(14,8,11,12,3)
names=colnames(X[,variables])
naive=lm(n_y~data.matrix(X[200:219, variables]))
naive2=lm(n_y~data.matrix(X[200:219, c(14)]))
plot(n_y, ylab="Cycle Time")
lines(naive$fitted.values, col=col1, lty=1)
lines(naive2$fitted.values, col=col2, lty=2)
legend("topleft", legend=c("Original", "Overfitted model", "Fit after feature selection"),
       lty=c(NA,1,2), pch=c(1, NA, NA), col=c(dot_col,col1,col2),bg="white",lwd=2, bty="n")
###LASSO###
object = lars(data.matrix(X[200:219, variables]), data.matrix(n_y), type="lasso", normalize=T, intercept=T, eps=0.00000000000001)
plot(object, breaks=F, col=c(1,32,"brown2","olivedrab4","gold3"), lty=c(1,2,3,4,5,6))
legend("bottomleft", legend=names,
       lty=c(1,2,3,4,5,6), pch=c(NA, NA, NA), col=c(1,32,"brown2","olivedrab4","gold3"),bg="white",lwd=2, bty="n")
summary(naive)
print(names)