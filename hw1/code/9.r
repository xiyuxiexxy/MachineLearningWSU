
accuracy<-array(dim=c(1,10))
for (i in 1:10)
{
	Model<-svm(stdtrdata[i,,],trlabel[i,,],type="C-classification",kernel="linear",cost=1)
	tab<- table( predict(Model, stdtstdata[i,,]), tstlabel[i,,])
	print(tab)
	accuracy[i]<-(tab[1,1]+tab[2,2]+tab[3,3])/15;
	
}
print(accuracy)	
