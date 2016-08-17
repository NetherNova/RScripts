# plot process context tags (temperature and speed)
PCH = 19
X1 = 6
X2 = 15

layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE),
       widths=c(3,1), heights=c(2,2))

temp = c(80, 70, 70, 65, 75, 80, 80, 91, 94, 94, 90, 109, 118, 100, 96, 100, 100, 91)
speed = c(10, 20, 23, 23, 20, 50, 60, 50, 45, 55, 50, 60, 55, 55, 50, 30, 27, 30)

plot(temp, col = ifelse(x < X1 | x > X2, "deepskyblue3", "brown2"), pch = PCH, xlab="", xaxt="n", ylab = "Temperature")
lines(temp)

segments(x0 = X1, x1 = X1, y0 = 0, y1 = 80, lty=2)
segments(x0 = X2, x1 = X2, y0 = 0, y1 = 96, lty=2)

plot(speed, col = ifelse(speed < 40, "deepskyblue3", "brown2"), pch = PCH, xlab="", xaxt="n", ylab = "Speed")
lines(speed)

abline(v = X1, lty = 2)
abline(v = X2, lty = 2)

abline(h = 40, lty = 2)
rect(X1, 40, X2, 62, col = "gold2", border = "transparent", density = 6)
text(2, 45, paste("r = ", paste(round(cor(speed, temp), 3))))
text(10, 25, paste("r = ", paste(round(cor(speed[6:15], temp[6:15]), 3))))