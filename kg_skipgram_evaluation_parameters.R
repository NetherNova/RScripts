require(ggplot2)
require(reshape2)

kg = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_parameters_3pct.csv", header=T)
kg$model = "KG"
kg$training_step = kg$training_step * 10

aggregated_mean_rank = aggregate(kg[, 6], list(kg$embedding_size, kg$batch_size, kg$learning_rate, kg$model, kg$fold), min)
names(aggregated_mean_rank) = c("embedding_size", "batch_size", "learning_rate", "model", "fold", "mean_rank")

aggregated_mean_rank = aggregate(aggregated_mean_rank[, 6], list(aggregated_mean_rank$embedding_size, 
                                                                 aggregated_mean_rank$batch_size, 
                                                                 aggregated_mean_rank$learning_rate,
                                                                 aggregated_mean_rank$model), mean)
names(aggregated_mean_rank) = c("embedding_size", "batch_size", "learning_rate", "model", "mean_rank")

print("Best Mean Rank Model: ")
print(aggregated_mean_rank[which.min(aggregated_mean_rank$mean_rank), ])

learning_rate_eval = aggregated_mean_rank[aggregated_mean_rank$batch_size == 128, ]

ggplot(data=learning_rate_eval, aes(x=learning_rate, y=mean_rank, group=embedding_size, colour=embedding_size)) + geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + theme(legend.title = element_text(size=12), 
legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom"
) + ggtitle("") + ylab("Mean Rank") + xlab("Learning_rate")

#
# ################# KG + Skip ####################
#
kg_skipgram = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_skipgram_parameters_3pct.csv", header=T)
kg_skipgram$model = "KG+Skip"
kg_skipgram$training_step = kg$training_step * 10
aggregated_mean_rank = aggregate(kg_skipgram[, 9], list(kg_skipgram$embedding_size, 
                                                        kg_skipgram$batch_size, 
                                                        kg_skipgram$learning_rate, 
                                                        kg_skipgram$num_skips, 
                                                        kg_skipgram$num_sampled, 
                                                        kg_skipgram$batch_size_sg,
                                                        kg_skipgram$fold,
                                                        kg_skipgram$model), min)
names(aggregated_mean_rank) = c("embedding_size", 
                                "batch_size", 
                                "learning_rate", 
                                "num_skips", 
                                "num_sampled", 
                                "batch_size_sg",
                                "fold", 
                                "model",
                                "mean_rank")
aggregated_mean_rank = aggregate(aggregated_mean_rank[, 9], list(aggregated_mean_rank$embedding_size, 
                                                                 aggregated_mean_rank$batch_size, 
                                                                 aggregated_mean_rank$learning_rate, 
                                                                 aggregated_mean_rank$num_skips, 
                                                                 aggregated_mean_rank$num_sampled, 
                                                                 aggregated_mean_rank$batch_size_sg, 
                                                                 aggregated_mean_rank$model), mean)
names(aggregated_mean_rank) = c("embedding_size", 
                                "batch_size", 
                                "learning_rate", 
                                "num_skips", 
                                "num_sampled", 
                                "batch_size_sg", 
                                "model", 
                                "mean_rank")

print("Best Mean Rank Model: ")
print(aggregated_mean_rank[which.min(aggregated_mean_rank$mean_rank), ])

param_eval = aggregated_mean_rank[aggregated_mean_rank$batch_size == 128, ]
param_eval = param_eval[param_eval$batch_size_sg == 128, ]
param_eval = param_eval[param_eval$num_skips == 2, ]
param_eval = param_eval[param_eval$learning_rate == 1.0, ]

ggplot(data=param_eval, aes(x=num_sampled, y=mean_rank, group=embedding_size, colour=embedding_size)) + 
  geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + 
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom") + 
  ggtitle("") + ylab("Mean Rank") + xlab("Num Sampled")

param_eval = aggregated_mean_rank[aggregated_mean_rank$batch_size == 128, ]
param_eval = param_eval[param_eval$batch_size_sg == 128, ]
param_eval = param_eval[param_eval$num_skips == 2, ]
param_eval = param_eval[param_eval$embedding_size == 140, ]

ggplot(data=param_eval, aes(x=num_sampled, y=mean_rank, group=learning_rate, colour=learning_rate)) + 
  geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + 
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom") + 
  ggtitle("") + ylab("Mean Rank") + xlab("Num Sampled")