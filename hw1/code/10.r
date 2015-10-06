
accuracy<-array(dim=c(11,10))
for (j in 1:11)
{
	c<-0.000001*(10^j)
	for (i in 1:10)
	{
		Model<-svm(stdtrdata[i,,],trlabel[i,,],type="C-classification",kernel="linear",cost=c)

		tab<- table( predict(Model, stdtstdata[i,,]), tstlabel[i,,])
		accuracy[j,i]<-(tab[1,1]+tab[2,2]+tab[3,3])/15;
	}
	
}
print(accuracy)
print(rowMeans(accuracy))
