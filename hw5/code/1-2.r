#1
Data<-read.table("hw5exptdata",sep=",")
Data<-unique(Data)

#2
class1<-Data[which(Data[,3]==1),]
class2<-Data[which(Data[,3]!=1),]



jpeg("plot.jpg")
plot(class1[,1],class1[,2],xlim=c(-4,3),ylim=c(-4,3))
points(class2[,1],class2[,2],col="red")
dev.off()

