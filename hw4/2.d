
#	Hs<-seq(from=1,to=51,by=10)		
#	LH<-length(Hs)	

#	for(index_h in LH:1)
#	{
#		H<-Hs[index_h]
#		print("hidden node")
#		print(H)

H<-10
cl<-Data[,17]

Cs<-list()

for(i in 1:11)
{
	Cs[[i]]<-which(cl==i)
	#print(i)
	#print(length(Cs[[i]]))
	sample(Cs[[i]])
}


for(hold in 1:1)
{
	
	test_pivot<-c()
	for(i in 1:11 )
	{
		start<-(hold-1)*floor(length(Cs[[i]])*0.1)+1
		end<-start+floor(length(Cs[[i]])*0.1)-1
	
		test_pivot<-c(test_pivot,Cs[[i]][start:end])
	}

	
	test<-Data[test_pivot,]
	train<-Data[-test_pivot,]



	
	#Backpropagation(train,test,40,0.3)	

#}
