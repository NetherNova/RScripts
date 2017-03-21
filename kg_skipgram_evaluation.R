require(ggplot2)
require(reshape2)

kg = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg128increasing_training_size_3pct_rate2.csv", header=T)
kg_skipgram = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_skipgram128increasing_training_size_3pct_rate2.csv", header=T)
kg = kg[,2:5]
kg_skipgram = kg_skipgram[, 2:5] 
kg$model = "KG"
kg_skipgram$model = "KG+Seq"
kg$training_step = kg$training_step * 50
kg_skipgram$training_step = kg_skipgram$training_step * 50

merged = rbind(kg, kg_skipgram)
#merged = merge(kg, kg_skipgram, by=c("V1"), all=FALSE)
#colors = c("firebrick3", "royalblue3", "black", "orange", "coral4", "green4",  "grey70", "grey80")

merged=aggregate(merged[, 3:4], list(merged$training_step, merged$model), mean)
names(merged) = c("Group.1", "model", "mean_rank", "hits_top_10")

ggplot(data=merged, aes(x=Group.1, y=hits_top_10, group=model, colour=model)) + geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + theme(legend.title = element_text(size=12), 
        legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
        axis.text=element_text(size=12), legend.position="bottom"
  ) + ggtitle("") + ylab("Hits Top-10") + xlab("Training Steps") + ylim(c(0, 40)) #+ scale_y_continuous(breaks=seq(0, 600, 50))
  #scale_color_manual(values=colors) 

ggplot(data=merged, aes(x=Group.1, y=mean_rank, group=model, colour=model)) + geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + theme(legend.title = element_text(size=12), 
                                                                                                         legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
                                                                                                         axis.text=element_text(size=12), legend.position="bottom"
) + ggtitle("") + ylab("Mean Rank") + ylim(c(280, 600)) + xlab("Training Steps") #+ scale_y_continuous(breaks=seq(0, 50, 5))



"""
min(kg$V4)
min(kg_skipgram$V4)

max(kg$V3)
max(kg_skipgram$V3)

kg_skipgram = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_skipgram_all_3pct.csv", header=T)
test_skip=aggregate(kg_skipgram[, 3:4], list(kg_skipgram$seq_data_size), mean)
#ggplot(data=test_skip, aes(x=Group.1, y=best_rank)) + geom_line()
test_skip$model="KG + Seq"

kg = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_all_3pct.csv", header=T)
test_kg=aggregate(kg[, 3:4], list(kg$seq_data_size), mean)
#ggplot(data=test_kg, aes(x=Group.1, y=best_rank)) + geom_line()
# plot comparison of kg, kg_skipgram for increasing size|KG|
test_kg$model="KG"
merged = rbind(test_kg, test_skip)

ggplot(data=merged, aes(x=Group.1, y=best_hits, group=model, colour=model)) + geom_line(aes(linetype=model)) + theme(legend.title = element_text(size=12), 
                                                                                                         legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
                                                                                                         axis.text=element_text(size=12), legend.position="bottom"
) + ggtitle("") + ylab("Hits Top-10") +  xlab("Embedding Size d") #+ scale_y_continuous(breaks=seq(0, 35, 10))
#scale_color_manual(values=colors) 

ggplot(data=merged, aes(x=Group.1, y=best_rank, group=model, colour=model)) + geom_line(aes(linetype=model)) + theme(legend.title = element_text(size=12), 
                                                                                                         legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
                                                                                                         axis.text=element_text(size=12), legend.position="bottom"
) + ggtitle("") + ylab("Mean Rank") +  xlab("Embedding Size d") #+ scale_y_continuous(breaks=seq(0, 300, 50))
"""