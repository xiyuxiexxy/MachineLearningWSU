
Hs<-seq(from=20,to=50,by=10)

#Ns<-c(0.01,0.1,0.3)


tr_acc=list()
te_acc=list()



N<-nrow(train)

random<-sample(N)
newtrain<-train
for(i in 1:N)
newtrain[i,]=train[random[i],]

train<-newtrain

input<-train[,1:16]
x0<-rep(1,N)
input<-cbind(x0,input)	
input<-t(input)


ninput<-nrow(input)


noutput<-length(unique(train[,17]))
label<-array(dim=c(N,noutput))

for(i in 1:N)
{
	l<-train[i,17]
	label[i,]<-rep(0,noutput)
	label[i,l]=1
}



TN<-nrow(test)		
test_input<-test[,1:16]
x0<-rep(1,TN)
test_input<-cbind(x0,test_input)	
test_input<-t(test_input)

testlabel<-array(dim=c(TN,noutput))
for(i in 1:TN)
{
	l<-test[i,17]
	testlabel[i,]<-rep(0,noutput)
	testlabel[i,l]=1
}

n<-0.1
for (epN in 1:length(Ns))
{
	
	H<-Hs[epH]
	print(epH)
	print(H)
	
#	n<-Ns[epN]
#	print(epN)
#	print(n)


	for(fold in 1:10)
	{
			
		test_pivot<-c()
		for(i in 1:11 )
		{
			start<-(fold-1)*floor(length(Cs[[i]])*0.1)+1
			end<-start+floor(length(Cs[[i]])*0.1)-1

			test_pivot<-c(test_pivot,Cs[[i]][start:end])
		}

	


		w<-array(dim=c(H, ninput))

		for(i in 1:H)
		{
			w[i,]=runif(ninput,-0.05,0.05)
		}
		w<-t(w)

								#w[17,H]

		v<-array(dim=c(noutput, H+1))

		for(i in 1:noutput)
		{
			v[i,]=runif(H+1,-0.05,0.05)
		}

		v<-t(v)						#v[H+1,11]

	
	
		y<-array(dim=c(N,noutput))
		train_acc<-c()
		test_acc<-c()
				
		for(ep in 1:20)
		{

			for(t in 1:N)
			{
				z<-rep(0,H)
				for(h in 1:H)
				{
					z[h]=sigmoid(t(w[,h])%*%input[,t])
				}	

				z<-c(1,z)				#(a)

				O<-rep(0,noutput)
				for(i in 1:noutput)
					O[i]<-t(v[,i])%*%z


				Y<-rep(0,noutput)
				for(i in 1:noutput)
					Y[i]<-exp(O[i])/sum(exp(O))		#(b)




				deltaV<-array(dim=c(noutput,H+1))
				for(i in 1:noutput)
				{
					deltaV[i,]=n*(label[t,i]-Y[i])*z
				}
				deltaV<-t(deltaV)			#(c)


				deltaW=array(dim=c(H,ninput))


				for(h in 1:H)
				{
				
					sum=0
					for(i in 1:noutput)
						sum=sum+(label[t,i]-Y[i])*v[h,i]
					deltaW[h,]=n*sum*z[h+1]*(1-z[h+1])*input[,t]
	
				}	

				deltaW<-t(deltaW)

				v<-v+deltaV
				w<-w+deltaW
			}	



			thisy<-array(dim=c(N,noutput))
			for(t in 1:N)
			{
				z<-rep(0,H)
				for(h in 1:H)
				{
					z[h]=sigmoid(t(w[,h])%*%input[,t])
				}	

				z<-c(1,z)				#(a)



				O<-rep(0,noutput)
				for(i in 1:noutput)
					O[i]<-t(v[,i])%*%z


				Y<-rep(0,noutput)
				for(i in 1:noutput)
					Y[i]<-exp(O[i])/sum(exp(O))		#(b)


				thisy[t,]<-Y
			}


		
			accuracy<-0
			for(i in 1:N)
			{
				fit<-order(thisy[i,])[11]
				if(train[i,17]==fit)
					accuracy=accuracy+1
			}
			

			
			print("train acc")
			print(accuracy)

			train_acc<-c(train_acc,accuracy)


			testacc<-0
			testy<-array(dim=c(TN,noutput))
			for(t in 1:TN)
			{
				z<-rep(0,H)
				for(h in 1:H)
				{
					z[h]=sigmoid(t(w[,h])%*%test_input[,t])
				}	

				z<-c(1,z)				#(a)



				O<-rep(0,noutput)
				for(i in 1:noutput)
					O[i]<-t(v[,i])%*%z


				Y<-rep(0,noutput)
				for(i in 1:noutput)
					Y[i]<-exp(O[i])/sum(exp(O))		#(b)


				testy[t,]<-Y
			}


			for(i in 1:TN)
			{
				fit<-order(testy[i,])[11]
				if(test[i,17]==fit)
					testacc=testacc+1
			}
			

			test_acc<-c(test_acc,testacc)
			
		}

		print(train_acc)
		print(test_acc)
	
	}		
	tr_acc[[epN]]<-train_acc
	te_acc[[epN]]<-test_acc
			
}

