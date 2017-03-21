library(ggplot2)
library(grid)
library(reshape2)

event_embeddings = "D:\\Dissertation\\Data Sets\\Event Variant Amberg\\final_embeddings.txt"
data = read.csv(event_embeddings, header=FALSE)
data = as.matrix(data)
data = data[1:6, ]
#data = matrix(data, dimnames = list(c("P1", "P2", "P3", "P4", "P5", "P6", "P7"), c(1:64)))
row.names(data) = paste("Variant", 1:nrow(data), sep="")
data.m = melt(data)
ggplot(data.m, aes(x=Var1, y=Var2)) + geom_tile(aes(fill=value), colour="white") + scale_fill_gradient(low="white", high="steelblue")


event_embeddings = "D:\\Dissertation\\Data Sets\\Event Variant Amberg\\final_embeddings_events.txt"
data = read.csv(event_embeddings, header=FALSE)
data = as.matrix(data)
data = data[c(44, 274, 127), ] # index um eins versetzt
#data = matrix(data, dimnames = list(c("P1", "P2", "P3", "P4", "P5", "P6", "P7"), c(1:64)))
#row.names(data) = paste("Variant", 1:nrow(data), sep="")
row.names(data) = c("BM 94 FE2: Stau nach FE2", 
                    "FE4 Vertikalhub Pick&Place AST / unten",
                    "FE13 Sort. Schieber: Teil fehlt in Endlage")
data.m = melt(data)
ggplot(data.m, aes(x=Var1, y=Var2)) + geom_tile(aes(fill=value), colour="white") + scale_fill_gradient(low="white", high="steelblue")