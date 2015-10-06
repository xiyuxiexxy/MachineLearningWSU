
#Backpropagation<-function(train, test, H, n)
#{



H<-10

Hs<-c()
tr_acc=list()
te_acc=list()

H<-100
prehacc<-0
epH<-0

#while(1)
#{
	
	epH<-epH+1
	print(epH)
	print(H)
	
	ep<-1
	train_acc<-rep(0,500)
	test_acc<-rep(0,500)


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

	

		Ns<-seq(from=0,to=0.3,by=0.01)

		index_n=length(Ns)
		n<-Ns[index_n]
		
		y<-array(dim=c(N,noutput))
		acc<-0

		
		while(1)
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
					deltaW[h,]=n*sum*z[h]*(1-z[h])*input[,t]
	
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

			train_acc[ep]<-accuracy


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
			

			print("test acc")
			print(testacc)
			test_acc[ep]<-testacc

			if(accuracy<=acc)
			{
				index_n=index_n-1
				n<-Ns[index_n]

				if(index_n==1)
					break	

				print(n)

			}

			else
			{
				acc<-accuracy
				y<-thisy
			}
			
			#print(acc)

			ep<-ep+1
			
		}

		print(train_acc)
		print(test_acc)
		if(acc>prehacc)
			prehacc<-acc
		
		Hacc<-c(Hacc,acc)
		Hs<-c(Hs,H)

		
			
		H<-H+50
		if(H>=1000)
			break
		
	#}
#}
