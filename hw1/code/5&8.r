

trmean<-array(dim=c(10,4))
trsd<-array(dim=c(10,4))


stdtstdata<-array(dim=c(10, 15,4))

stdtrdata<-array(dim=c(10, 135,4))

for (i in 1:10)
{

	 trmean[i,]<-colMeans(trdata[i,,])
	 trsd[i,]<-sd(trdata[i,,])

 	 for (j in 1:4)
	 {
		
		stdtrdata[i,,j]=(trdata[i,,j]-trmean[i,j])/trsd[i,j]
		stdtstdata[i,,j]=(tstdata[i,,j]-trmean[i,j])/trsd[i,j]	
	 }

}



