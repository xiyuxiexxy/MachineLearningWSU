setwd("/home/xiyu/Downloads/570/hw2data")
train_data<-read.table("train.data",sep=",",header=TRUE)
train_label<-read.table("train.label",sep=",",header=TRUE)
test_label<-read.table("test.label",sep=",",header=TRUE)
test_data<-read.table("test.data",sep=",",header=TRUE)
train<-train_data[,2:31]
test<-test_data[,2:31]
cl<-train_label[,2]
n<-nrow(train_label)
sum<-sum(train_label[2])
ncancer<-2*n-sum
pc<-ncancer/n
pnc<-1-pc

cancer<-train[1:ncancer,]
cancer_means<-colMeans(cancer)
cancer_sd<-sd(cancer)
ncid<-ncancer+1
non_cancer<-train[ncid:n,]
non_cancer_means<-colMeans(non_cancer)
non_cancer_sd<-sd(non_cancer)
r<-nrow(test)
c<-ncol(test)

px_c<-array(dim=c(r,c))
px_nc<-array(dim=c(r,c))
for (i in 1:c)
{

	px_c[,i]<-exp( -(test[,i]-cancer_means[i])^2 / (2* (cancer_sd[i]^2))) /(sqrt(2*pi)*cancer_sd[i])
	px_nc[,i]<-exp( -(test[,i]-non_cancer_means[i])^2 / (2* (non_cancer_sd[i]^2))) /(sqrt(2*pi)*non_cancer_sd[i])
}

class_mle<-array(dim=c(r,1))
for (i in 1:r)
{
	x<-1
	y<-1
	for(j in 1:c)
	{
		x<-x*px_c[i,j]
		y<-y*px_nc[i,j]
	}
	if(x>y)
		class_mle[i]<-1
	else
		class_mle[i]<-2

}
mle_accuracy<-0
for(i in 1:r)
{
	if (class_mle[i]==test_label[i,2])
		mle_accuracy<-mle_accuracy+1;
}
mle_accuracy<-mle_accuracy/r

tsum<-sum(test_label[,2])
tc<-2*r-tsum
ptc<-tc/r
ptnc<-1-ptc

class_map<-array(dim=c(r,1))
for (i in 1:r)
{
	x<-1
	y<-1
	for(j in 1:c)
	{
		x<-x*px_c[i,j]
		y<-y*px_nc[i,j]
	}
	x<-x*ptc
	y<-y*ptnc
	if(x>y)
		class_map[i]<-1
	else
		class_map[i]<-2

}
map_accuracy<-0
for(i in 1:r)
{
	if (class_map[i]==test_label[i,2])
		map_accuracy<-map_accuracy+1;
}
map_accuracy<-map_accuracy/r


