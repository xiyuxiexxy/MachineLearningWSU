cost<-c(-5:5)
y<-rowMeans(accuracy)
jpeg("cost-accuracy-plot.jpg")
plot(cost, y, xlab="log10cost", ylab="accuracy",main="cost-accuracy-plot")
dev.off()