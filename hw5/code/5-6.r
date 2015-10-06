T<-250
install.packages("e1071")
library('e1071')


#3
N1<-nrow(class1)
N2<-nrow(class2)

pivot1<-sample(1:N1)
pivot2<-sample(1:N2)

test<-array()
train<-array()
testacc<-c()

confusion<-list()

precisions1<-c()
recalls1<-c()
precisions2<-c()
recalls2<-c()
s<-c(1:10)
s<-as.character(s)
for (f in 1:10)		#fold
{
	
	time<-floor(nrow(class1)/10)

	start=time*(f-1)+1
	end=time*f
	test<-class1[pivot1[start:end],]


	train<-class1[-pivot1[start:end],]


	time<-floor(nrow(class2)/10)
	start=time*(f-1)+1
	end=time*f

	test<-rbind(test, class2[pivot2[start:end],])
	train<-rbind(train, class2[-pivot2[start:end],])
	
	
	D<-rep(1/nrow(train),nrow(train))


	a<-array()
	h<-list()	
	y<-list()	

	N<-nrow(train)

	errors<-array()
	trainacc<-c()

  for(t in 1:T)
  {
	
	sam<-sample(1:N, N,replace=TRUE ,prob=D)
	
	trainlab<-train[sam,3]
	traindata<-train[sam,1:2]		#draw sample
	
	h[[t]]<-svm(traindata,trainlab,type="C-classification",kernel= 'linear', cost=1)

						#train h

	pred<-predict(h[[t]],train[,1:2])
	y[[t]]<-as.numeric(as.vector(pred))	#get y
	
	
	I<-rep(0, nrow(train))
	I[which(y[[t]]!=train[,3])]=1	#indicator
	
	error=D%*%I
	#error=0
	#for (i in 1:N)
	#	error=error+D[i]*I[i]		#error

	errors[t]=error
	print(error)

	if(error>=0.5)
	{	
		t=t-1
		break;
	}
	else
	{
	
		
		a[t]<-0.5*log((1-error)/error)
	
	#	Z <- sum(D * exp(-a*train[,3]*h[[t]]) )
	#	D <- (D * exp(-a*train[,3]*h[[t]]))/Z
	
		Z<-0	
		for(i in 1:N)
		{
			Z=Z+D[i]*exp(-a*train[i,3]*y[[t]][i])
		}
	
		for(i in 1:N)
		{
			D[i]=D[i]*exp(-a*train[i,3]*y[[t]][i])/Z	#D
		}
	}


	

	#trainacc so far
#	Y<-array()
#	for(tr in 1:N)
#	{
#		Y[tr]=0
#		for(t1 in 1:t)
#		{
#			Y[tr]=Y[tr]+a[t1]*y[[t1]][tr]
#		}
#	}
	
#	trainacc[t]<-length(which(sign(Y)==train[,3]))
#
#	print(trainacc[t])

	
  }

#	break	
		
	H<-c()
	n<-nrow(test)
	testy<-list()
	for(t2 in 1:t)
	{
		pred<-predict(h[[t2]],test[,1:2])
		testy[[t2]]<-as.numeric(as.vector(pred))
	}	
	



	for(te in 1:n)
	{
		H[te]=0
		for(t2 in 1:t)
		{
			H[te]=H[te]+a[t2]*testy[[t2]][te]
		}
	}
	
#	testacc[f]<-length(which(sign(H)==test[,3]))


	
	
		
	# +	tp	fn	p
	# -	fp	tn	n
	#	p'	n'
	
	Y<-sign(H)
	table<-array(dim=c(2,2))

	table[1,]=0
		table[2,]=0	
		for(i in 1:n)
		{
			if( test[i,3]==1)
			{
				if(Y[i]==1)
					table[1,1]=table[1,1]+1
				else
					table[1,2]=table[1,2]+1
			}
			else
			{
				if(Y[i]==1) 
					table[2,1]=table[2,1]+1
				else
					table[2,2]=table[2,2]+1
			}

		}

	confusion[[f]]<-table



	#precision =tp /p'
	#recall    =tp/p
	
	precision1= table[1,1]/ (table[1,1]+table[2,1])
	recall1 =table[1,1] /(table[1,1]+table[1,2])
	
	precision2= table[2,2]/ (table[2,2]+table[1,2])
	recall2 =table[2,2] /(table[2,1]+table[2,2])
		
	precisions1[f]=precision1
	recalls1[f]=recall1

	precisions2[f]=precision2
	recalls2[f]=recall2



#	tps<-c()
#	fps<-c()

#	thresholds<-H[order(H)]
#	for(r in 1:n)
#	{
#		thr=thresholds[r]
#		
#		Y<-rep(-1,n)
#		Y[which(H>thr)]=1

#		table[1,]=0
#		table[2,]=0	
#		for(i in 1:n)
#		{
#			if( test[i,3]==1)
#			{
#				if(Y[i]==1)
#					table[1,1]=table[1,1]+1
#				else
#					table[1,2]=table[1,2]+1
#			}
#			else
#			{
#				if(Y[i]==1) 
#					table[2,1]=table[2,1]+1
#				else
#					table[2,2]=table[2,2]+1
#			}

#		}
#		tps[r]=table[1,1]
#		fps[r]=table[2,1]



#	}
	#		jpeg(s[f])
#		plot(fps,tps )
#		dev.off()

}





