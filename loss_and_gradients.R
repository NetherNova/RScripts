require(ggplot2)
require(reshape2)

kg = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_parameters_3pct.csv", header=T) 
kg$model = "KG"
kg$training_step = kg$training_step * 10

#colors = c("firebrick3", "royalblue3", "black", "orange", "coral4", "green4",  "grey70", "grey80")

aggregated_loss = aggregate(kg[, 10], list(kg$embedding_size, kg$batch_size, kg$learning_rate, kg$model, kg$training_step), mean)
names(aggregated_loss) = c("embedding_size", "batch_size", "learning_rate", "model", "training_step", "loss")

learning_rate_loss = aggregated_loss[aggregated_loss$embedding_size == 180, ]
#aggregated_loss = aggregated_loss[aggregated_loss$learning_rate == 0.5, ]
learning_rate_loss = learning_rate_loss[learning_rate_loss$batch_size == 128, ]

ggplot(data=learning_rate_loss, aes(x=training_step, y=loss, group=learning_rate, colour=learning_rate)) + geom_line(aes(linetype=model)) + 
  geom_point(aes(shape=model)) + 
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
  axis.text=element_text(size=12), legend.position="bottom") + 
  ggtitle("") + ylab("Loss") + 
  xlab("Training Steps") #+ ylim(c(0, 2))

embedding_loss = aggregated_loss[aggregated_loss$batch_size == 128, ]
embedding_loss = embedding_loss[embedding_loss$learning_rate == 1.0, ]

ggplot(data=embedding_loss, aes(x=training_step, y=loss, group=embedding_size, colour=embedding_size)) + geom_line(aes(linetype=model)) + 
  geom_point(aes(shape=model)) + 
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
        axis.text=element_text(size=12), legend.position="bottom") + 
  ggtitle("") + ylab("Loss") + 
  xlab("Training Steps") #+ ylim(c(0, 2))