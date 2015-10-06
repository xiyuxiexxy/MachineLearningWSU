A<-c(1:50)
pivot<-matrix(data=NA,nrow=10,ncol=5)
pivot[1,]=sample(A,size=5)
for( i in 2:10)
{
	pivot[i,]=sample(A[- pivot[1:i-1,]],size=5)
}


tstdata<-array(dim=c(10, 15,4))
tstlabel<-array(dim=c(10,15,1))

trdata<-array(dim=c(10, 135,4))
trlabel<-array(dim=c(10, 135,1))
for (i in 1:10)
{
tstdata[i,,]<-data[c(pivot[i,], pivot[i,]+50,pivot[i,]+100),]
tstlabel[i,,]<-label[c(pivot[i,], pivot[i,]+50,pivot[i,]+100),]
trdata[i,,]<-data[-c(pivot[i,], pivot[i,]+50,pivot[i,]+100),]
trlabel[i,,]<-label[-c(pivot[i,], pivot[i,]+50,pivot[i,]+100),]
}
