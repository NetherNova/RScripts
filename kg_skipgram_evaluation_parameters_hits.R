require(ggplot2)
require(reshape2)

kg = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_parameters_3pct.csv", header=T)
kg$model = "KG"
kg$training_step = kg$training_step * 10

aggregated_hits = aggregate(kg[, 7], list(kg$embedding_size, kg$batch_size, kg$learning_rate, kg$model, kg$fold), max)
names(aggregated_hits) = c("embedding_size", "batch_size", "learning_rate", "model", "fold", "hits_top_10")

aggregated_hits = aggregate(aggregated_hits[, 6], list(aggregated_hits$embedding_size, 
                                                       aggregated_hits$batch_size, 
                                                       aggregated_hits$learning_rate,
                                                       aggregated_hits$model), mean)
names(aggregated_hits) = c("embedding_size", "batch_size", "learning_rate", "model", "hits_top_10")

print("Best Hits Model: ")
print(aggregated_hits[which.max(aggregated_hits$hits_top_10), ])

learning_rate_eval = aggregated_hits[aggregated_hits$batch_size == 32, ]
ggplot(data=learning_rate_eval, aes(x=learning_rate, y=hits_top_10, group=embedding_size, colour=embedding_size)) + geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + theme(legend.title = element_text(size=12), 
legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom") + ggtitle("") + ylab("Hits") + xlab("Learning_rate")

#
# ################# KG + Skip ####################
#
kg_skipgram = read.csv("D:\\Dissertation\\Data Sets\\Event Variant Amberg\\Eval\\evaluation_kg_skipgram_parameters_3pct.csv", header=T)
kg_skipgram$model = "KG+Skip"
kg_skipgram$training_step = kg$training_step * 10
aggregated_hits = aggregate(kg_skipgram[, 10], list(kg_skipgram$embedding_size, 
                                                        kg_skipgram$batch_size, 
                                                        kg_skipgram$learning_rate, 
                                                        kg_skipgram$num_skips, 
                                                        kg_skipgram$num_sampled, 
                                                        kg_skipgram$batch_size_sg,
                                                        kg_skipgram$fold,
                                                        kg_skipgram$model), max)
names(aggregated_hits) = c("embedding_size", 
                                "batch_size", 
                                "learning_rate", 
                                "num_skips", 
                                "num_sampled", 
                                "batch_size_sg",
                                "fold", 
                                "model",
                                "hits_top_10")
aggregated_hits = aggregate(aggregated_hits[, 9], list(aggregated_hits$embedding_size, 
                                                       aggregated_hits$batch_size, 
                                                       aggregated_hits$learning_rate, 
                                                       aggregated_hits$num_skips, 
                                                       aggregated_hits$num_sampled, 
                                                       aggregated_hits$batch_size_sg, 
                                                       aggregated_hits$model), mean)
names(aggregated_hits) = c("embedding_size", 
                                "batch_size", 
                                "learning_rate", 
                                "num_skips", 
                                "num_sampled", 
                                "batch_size_sg", 
                                "model", 
                                "hits_top_10")

print("Best Hits Rank Model: ")
print(aggregated_hits[which.max(aggregated_hits$hits_top_10), ])

param_eval = aggregated_hits[aggregated_hits$batch_size == 64, ]
param_eval = param_eval[param_eval$batch_size_sg == 128, ]
param_eval = param_eval[param_eval$num_skips == 4, ]
param_eval = param_eval[param_eval$learning_rate == 0.8, ]

ggplot(data=param_eval, aes(x=num_sampled, y=hits_top_10, group=embedding_size, colour=embedding_size)) + 
  geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + 
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom") + 
  ggtitle("") + ylab("Hits Top 10") + xlab("Num Sampled")

param_eval = aggregated_hits[aggregated_hits$batch_size == 64, ]
param_eval = param_eval[param_eval$batch_size_sg == 128, ]
param_eval = param_eval[param_eval$num_skips == 4, ]
param_eval = param_eval[param_eval$embedding_size == 140, ]

ggplot(data=param_eval, aes(x=num_sampled, y=hits_top_10, group=learning_rate, colour=learning_rate)) + 
  geom_line(aes(linetype=model)) + geom_point(aes(shape=model)) + 
  theme(legend.title = element_text(size=12), legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), legend.position="bottom") + 
  ggtitle("") + ylab("Hits Top 10") + xlab("Num Sampled")