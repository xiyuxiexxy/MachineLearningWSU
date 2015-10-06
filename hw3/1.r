Data<-read.table("agaricus-lepiota.data", sep=",")
Data<-unique(Data)
Data<-Data[order(Data[,1]),]
c<-ncol(Data)
r<-nrow(Data)
numData<-array(dim=c(r,c))
for( i in 1:c)
{
	numData[,i]= as.numeric(Data[,i])
}



cal_entropy <- function(set)
{
	sum<-sum(set[,1])
	r<-nrow(set)
	n2<-sum-r
	p2<-n2/r	
	n1<-r-n2
	p1<-n1/r

	return(-p1*log(p1,base=2)-p2*log(p2,base=2))
}
root_entropy<-cal_entropy(numData)

cal_gain<-function(set,index)
{

	value<-unique(set[,index])
	nvalue<- length(value)

	result=cal_entropy(set)
	
	for(i in 1:nvalue)
	{
		newset<-set[set[,index]==value[i],]
		nnewset<-nrow(newset)
	
		gain<-cal_entropy(newset)*nnewset/nrow(set)

		
		if(!is.na(gain))
		{
			result=result-gain
		}
			
	}
		
	return(result)

}


find_max<-function(set,attr)
{
	nattr<-length(attr)
	
	index=1
	gain=cal_gain(set,attr[1])
	
	for(i in 2:nattr)
	{
		if( cal_gain(set,attr[i])>gain)
			index=i
	}

	return(index)
}



id3<-function(set,attr)
{

	sum<-sum(set[,1])
	r<-nrow(set)

	n2<-sum-r
	p2<-n2/r	
	n1<-r-n2
	p1<-n1/r

	if(p1==1)
		return(1)
	else if(p2==1)
		return(0)
	else
	{
		
		index<-find_max(set, attr)
		
		
		value<-unique(set[,attr[index]])
	
		nvalue<- length(value)

		
		for(i in 2:nvalue)
		{
			newset<-set[set[,attr[index]]==value[i],]	
			nnewset<-nrow(newset)
			
			if(nnewset==0)
			{
				mu<-mean(set[,1])
				if(mu>1)
					return(2)
				else
					return(1)
				

			}

			else
			{
				id3(newset, attr[-index])
			}
			
		}
		


	}
	

}




ParseID3<-function(set,attr,data)
{

	sum<-sum(set[,1])
	r<-nrow(set)

	n2<-sum-r
	p2<-n2/r	
	n1<-r-n2
	p1<-n1/r

	if(p1==1)
		return(1)
	else if(p2==1)
		return(2)
	else
	{
		
		index<-find_max(set, attr)
		
		
		value<-unique(set[,attr[index]])
	
		nvalue<- length(value)

		
		
			newset<-set[set[,attr[index]]==data[index],]	
			nnewset<-nrow(newset)
			
			if(nnewset==0)
			{
				mu<-mean(set[,1])
				if(mu>1)
					return(2)
				else
					return(1)
				

			}

			else
			{
				ParseID3(newset, attr[-index],data)
			}
			
		
	

	}
	



}

id3<-function(set,attr,data)
{

	sum<-sum(set[,1])
	r<-nrow(set)

	n2<-sum-r
	p2<-n2/r	
	n1<-r-n2
	p1<-n1/r

	if(p1==1)
		return(1)
	else if(p2==1)
		return(0)
	else
	{
		
		index<-find_max(set, attr)
		
		
		value<-unique(set[,attr[index]])
	
		nvalue<- length(value)

		
		for(i in 2:nvalue)
		{
			newset<-set[set[,attr[index]]==value[i],]	
			nnewset<-nrow(newset)
			
			if(nnewset==0)
			{
				mu<-mean(set[,1])
				if(mu>1)
					return(2)
				else
					return(1)
				

			}

			else
			{
				id3(newset, attr[-index])
			}
			
		}
		


	}
	
}





sum<-sum(numData[,1])
r<-nrow(numData)

n2<-sum-r
p2<-n2/r	
n1<-r-n2
p1<-n1/r

edible<-numData[1:n1,]
x<-n1+1;
poisonous<-numData[x:r,]





























