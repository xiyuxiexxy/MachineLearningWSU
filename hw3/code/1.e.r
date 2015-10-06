sum<-sum(numData[,1])
r<-nrow(numData)

n2<-sum-r
p2<-n2/r	
n1<-r-n2
p1<-n1/r

edible<-numData[1:n1,]
x<-n1+1;
poisonous<-numData[x:r,]


edible<-numData[1:n1,]
x<-n1+1;
poisonous<-numData[x:r,]

ntest<-r*0.25
ntest1<-ntest*p1
ntest2<-ntest*p2

accuracy<-rep(0,10)

sum_acc=0
for (j in 1:10)
{
pivot1=sample(c(1:n1),size=ntest1)
test1=edible[pivot1,]
train1=edible[-pivot1,]

pivot2=sample(c(1:n2),size=ntest2)
test2=poisonous[pivot2,]
train2=poisonous[-pivot2,]


test<-rbind(test1,test2)
train<-rbind(train1,train2)


acc=0;


tree<-id3(train,attr)

for(i in 1:nrow(test))
{
	#print(i)
	result=Parse(tree,test[i,])
	if(result==test[i,1])
		acc=acc+1;

}

accuracy[j]=acc/nrow(test)
sum_acc=sum_acc+accuracy[j]
print(accuracy)
}

avg_acc=sum_acc/10


