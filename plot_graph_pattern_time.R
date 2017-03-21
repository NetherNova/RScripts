library(ggplot2)
require(grid)
require(scales)
results = read.csv("D:\\Dissertation\\Data Sets\\manufacturing\\timeing - faked.csv")
results$time = as.POSIXct(strptime(results$time, format="%H:%M:%OS"))
ggplot(results, aes(x=num_process,y=time, group=model, colour=model)) + 
  theme(legend.title = element_blank(), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom") + ylab("Time (Minutes)") + xlab("Number of Graphs") + 
  scale_color_manual(values=c("royalblue3", "firebrick3")) +
  geom_line(aes(linetype=model), size=1.5) + scale_y_datetime(breaks = date_breaks("120 sec"), labels=date_format("%M"))

dev.copy2pdf(file="Rplot_test.pdf", width=6, height=5)