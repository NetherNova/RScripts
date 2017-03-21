library(ggplot2)
require(gridExtra)
require(grid)
require(scales)

#results = read.csv("D:\\Dissertation\\Data Sets\\Movies\\MovieSummaries\\classifiers_df_8.csv")
#results = read.csv("D:\\Dissertation\\Data Sets\\Movies\\MovieSummaries\\classifiers_df_8_all_constraints.csv")
#results = read.csv("D:\\Dissertation\\Data Sets\\Manufacturing\\classifiers_new_cons.csv")
#results = read.csv("D:\\Dissertation\\Data Sets\\Movies\\MovieSummaries\\classifiers_fair_movies.csv")
results = read.csv("D:\\Dissertation\\Data Sets\\Movies\\MovieSummaries\\classifiers_movies2017-01-12.csv")

upperbound_f1 = min(max(results$f1.score) * 1.2, 1)
lowerbound_f1 = max(min(results$f1.score) * 0.8, 0)
upperbound_runtime = max(results$runtime) * 1.2
lowerbound_runtime = min(results$runtime) * 0.8

scale_runtime = c(lowerbound_runtime, upperbound_runtime)
scale_f1 = c(lowerbound_f1, upperbound_f1)
excluded_models = c("gMGFL", "gMGFL+cons")

x_lab = "Number of Features"
#x_lab = "Number of Constraints"
y_lab1 = "F1"
y_lab2 = "Runtime (sec)"

#results_KN = results[results$classifier == "Nearest Neighbor" & results$model != "top-k+constraints" & results$model != "top-k", ]
#results_NB = results[results$classifier == "Naive Bayes"& results$model != "top-k+constraints" & results$model != "top-k" , ]
#results_SVM = results[results$classifier == "Linear SVM" & results$model != "top-k+constraints" & results$model != "top-k" , ]

results_KN = results[results$classifier == "Nearest Neighbor", ]
results_NB = results[results$classifier == "Naive Bayes" , ]
results_SVM = results[results$classifier == "Linear SVM", ]

#colors = c("grey0", "grey20", "grey40", "grey60", "grey70", "grey80") 
colors = c("firebrick3", "royalblue3", "black", "orange", "coral4", "green4",  "grey70", "grey80")

g1 = ggplot(results_KN, aes(x=num_features, y=f1.score, group=model, colour=model)) + theme_bw() +
  theme(legend.position="none",legend.title = element_text(size=12), 
        legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), 
        axis.text=element_text(size=12) 
        #legend.position="bottom"
        ) + 
  ggtitle("KNN") + ylab(y_lab1) + xlab(x_lab) + 
  scale_color_manual(values=colors) + scale_y_continuous(limits=scale_f1) +
  geom_line(aes(linetype=model), size=1.0) + geom_point(aes(shape=model), size = 4)
# Test for significance

g_runtime = ggplot(results, aes(x=num_features, y=runtime, group=model, colour=model)) + theme_bw()+
  theme(legend.title = element_text(size=12), 
        legend.text = element_text(size=12), axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12), 
        legend.position="bottom") +   ylab(y_lab2) + xlab(x_lab) + 
  scale_color_manual(values=colors) + scale_y_continuous(limits=scale_runtime) +
  geom_line(aes(linetype=model), size=1.0) +geom_point(aes(shape=model), size = 4)

g3 = ggplot(results_NB, aes(x=num_features, y=f1.score, group=model, colour=model)) + theme_bw()+
  theme(legend.position="none", axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12)) + ggtitle("NB") +  ylab(y_lab1) + xlab(x_lab) + 
  scale_color_manual(values=colors) + scale_y_continuous(limits=scale_f1) +
  geom_line(aes(linetype=model), size=1.0) +geom_point(aes(shape=model), size = 4)
# Test for significance

g5 = ggplot(results_SVM, aes(x=num_features, y=f1.score, group=model, colour=model)) + theme_bw()+
  theme(legend.position="none", axis.title.y =element_text(size=14), axis.title.x =element_text(size=14), axis.text=element_text(size=12)) + ggtitle("SVM") + ylab(y_lab1) + xlab(x_lab) + 
  scale_color_manual(values=colors) + scale_y_continuous(limits=scale_f1) +
  geom_line(aes(linetype=model), size=1.0) +geom_point(aes(shape=model), size = 4)
# Test for significance


grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}
grid.arrange(g1, g3, g5, g_runtime, layout_matrix=rbind(c(1,2,3), c(4,4,4)))
#grid_arrange_shared_legend(g1, g3, g5, g_runtime, layout_matrix=rbind(c(1,2,3), c(4,4,4)))
#grid_arrange_shared_legend(g1, g3, g5)
