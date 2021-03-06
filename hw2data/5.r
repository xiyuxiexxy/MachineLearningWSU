#install.packages("FNN")
#library(FNN)




n<-nrow(test)
accuracy<-rep(0,21)
knn_label<-array(dim=c(n,1))
for(i in 1:n)
{
	tst_train<-rbind(test[i,], train)
	d<-as.matrix(dist(tst_train,meth="euclidean",p=2))
	neigbor<-order(d[1,])[1:5]					#5=k+1
	neigbor<-neigbor[-1]
	neigbor<-neigbor-1	
	lb<-train_label[neigbor,2]
	if(mean(lb)>1)
		knn_label[i]=2
	else
		knn_label[i]=1
}	

e_ac=0;

for(i in 1:n)
{
	if (knn_label[i]==test_label[i,2])
		e_ac<-e_ac+1;
}
e_ac=e_ac/n

knn_label<-array(dim=c(n,1))
for(i in 1:n)
{
	tst_train<-rbind(test[i,], train)
	d<-as.matrix(dist(tst_train,meth="manhattan"))
	neigbor<-order(d[1,])[1:5]					#5=k+1
	neigbor<-neigbor[-1]
	neigbor<-neigbor-1	
	lb<-train_label[neigbor,2]
	if(mean(lb)>1)
		knn_label[i]=2
	else
		knn_label[i]=1
}	

h_ac=0;

for(i in 1:n)
{
	if (knn_label[i]==test_label[i,2])
		h_ac<-h_ac+1;
}
h_ac=h_ac/n



accuracy<-rep(0,21)
for(j in 1:21)
{
	for(i in 1:n)
	{
		tst_train<-rbind(test[i,], train)
		d<-as.matrix(dist(tst_train,meth="euclidean",p=2))
		r<-j+1
		neigbor<-order(d[1,])[1:r]					#5=k+1
		neigbor<-neigbor[-1]
		neigbor<-neigbor-1	
		lb<-train_label[neigbor,2]
		if(mean(lb)>1)
			knn_label[i]=2
		else
			knn_label[i]=1
	}	

	e_ac=0;

	for(i in 1:n)
	{
		if (knn_label[i]==test_label[i,2])
			e_ac<-e_ac+1;
	}
	e_ac=e_ac/n


	accuracy[j]<-e_ac
}
x<-c(1:21)
plot(x,accuracy)



n<-nrow(test)
accuracy<-rep(0,21)
for(i in 1:21)
{
	preds<-knn(train, test, cl, k=j)	
	tab<-table(preds,test_label[,2])
	accuracy[i]<-(tab[1,1]+tab[2,2])/n
}
	
x<-c(1:21)
plot(x,accuracy)


