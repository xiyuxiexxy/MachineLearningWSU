


widths<-c(-6:6)
widths<-10^widths
costs<-c(-6:3)
costs<-10^costs



W<-length(widths)
C<-length(costs)

#i<-1
#j<-1

cl<-Data[,17]
Cs<-list()




for(i in 1:11)
{
	Cs[[i]]<-which(cl==i)
	#print(i)
	#print(length(Cs[[i]]))
	sample(Cs[[i]])
}



# radial 

accuracys<-array(dim=c(C,W,10))
for(c in 1:C)
{
	for(w in 1:W)
	{
	
		ACC<-c()
		for(fold in 1:10)
		{
			
			test_pivot<-c()
			for(i in 1:11 )
			{
				start<-(fold-1)*floor(length(Cs[[i]])*0.1)+1
				end<-start+floor(length(Cs[[i]])*0.1)-1
	
				test_pivot<-c(test_pivot,Cs[[i]][start:end])
			}

	

			test<-Data[test_pivot,]
			train<-Data[-test_pivot,]



			
			data<-train[,1:16]
			label<-train[,17]




			testdata<-test[,1:16]
			testlabel<-test[,17]



			model<-svm(data,label,type="C",kernel= 'radial', gamma=widths[w],cost=costs[c])

			pred<-predict(model,testdata)

			tab<-table(pred, testlabel)


			class<-nrow(tab)
			acc<-0
			for(m in 1:class)
				acc=acc+tab[m,m]
			print(acc)
			ACC<-c(ACC,acc)
		}
	

		accuracys[c,w,]=ACC

	}
}








