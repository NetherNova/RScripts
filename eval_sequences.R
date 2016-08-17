library(ggplot2)
require(grid)
type = "roc"
if (type == "f1")
{
  results = read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\clustering_results _constrained.csv")

  pd <- position_dodge(0.15)
  
  ggplot(results, aes(x=k, y=f1.avg, colour=clustering, group=clustering)) + 
    geom_errorbar(aes(ymin=f1.avg-f1.sd, ymax=f1.avg+f1.sd), colour="black", width=.1, position=pd) +
    geom_line(position=pd) +
    geom_point(position=pd, size=3, shape=21, fill="white") + # 21 is filled circle
    xlab("k") +
    ylab("F1") +
    scale_colour_hue(name="Clustering Type",    # Legend label, use darker colors
                     breaks=c("With Sequence Constraints", "Without Sequence Constraints"),
                     labels=c("With Sequence Constraints", "Without Sequence Constraints"),
                     l=40) +                    # Use darker colors, lightness=40
    ggtitle("Effect of Clustering with Sequence Constraints on Classification") +
    expand_limits(y=0) +                        # Expand y range
    scale_y_continuous(limits = c(0.0, 1.0)) +
    scale_x_discrete() + 
    theme_bw() +
    theme(legend.justification=c(1,0),
          legend.position=c(1,0))
} else
{
  results = read.csv("D:\\Dissertation\\Data Sets\\Demonstrator\\clustering_roc_results.csv")
  constraint_best_k = 7
  without_best_k = 4
  d_new = rbind(d3[d3$k == without_best_k & d3$clustering == "Without Sequence Constraints", c("fpr.avg", "tpr.avg", "clustering")], d3[d3$k == constraint_best_k & d3$clustering == "With Sequence Constraints", c("fpr.avg", "tpr.avg", "clustering")])
  ggplot(d_new, aes(x=fpr.avg,y=tpr.avg, group=clustering, colour=clustering))+
    geom_line(aes(linetype=clustering), size=1.5) +
    geom_abline(slope=1, intercept=0) +
      scale_linetype_manual(values=c("twodash", "dotted")) +
      #scale_color_manual(values=c('#999999','#E69F00')) +
    labs(title= "ROC curve", 
         x = "False Positive Rate (1-Specificity)", 
         y = "True Positive Rate (Sensitivity)") + 
    scale_y_continuous(limits = c(0.0, 1.0)) +
    scale_x_continuous(limits = c(0.0, 1.0)) +
    theme_bw() +
    theme(legend.justification=c(1,0),
          legend.position=c(1,0), legend.key.size=unit(1.5, "cm"))
}