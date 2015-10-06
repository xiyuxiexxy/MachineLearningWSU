nnode=0
id3<-function(set,attr)
{
	
	
	root<-list(label=0,attr=0,nbranch=0,branch=list())
	
	nnode<<-nnode+1

	if(is.vector(set)==TRUE)
	{
		root$label=set[1]
			root$attr=0
			root$branch=NULL
			return(root)

	}


	sum<-sum(set[,1])
	r<-nrow(set)

	n2<-sum-r
	p2<-n2/r	
	n1<-r-n2
	p1<-n1/r


	if(is.na(p1)||is.na(p2))
	{
			root$label=0
			root$attr=0
			root$branch=NULL
			return(root)

	}


	##print(p1)
	##print(p1)
	##print(length(attr))

	if(length(attr)==0)
	{
			##print("0");		
			mu<-mean(set[,1])
			if(mu>1.5)
			{
				root$branch[[i]]=list(label=2, attr=0,nbranch=0,branch=list())
			}
			else
			{
				root$branch[[i]]=list(label=1, attr=0,nbranch=0,branch=list())
			}

	}










	entropy=cal_entropy(set)

	if (entropy==0)
	{
		##print(set)
		##print(p1)
		##print(p2)		
		if(p1>p2)
		{
			root$label=1
			root$attr=0
			root$branch=NULL
			return(root)
		}
	
	
		root$label=2
		root$attr=0
		root$branch=NULL
		return(root)

	}
	else
	{
		##print("branch");	
		
		index<-find_max(set, attr)
		##print("findmax")
		value<-unique(numData[,attr[index]])
	
		nvalue<- length(value)

		root$attr<-attr[index]
		root$values<-value

		root$nbranch=nvalue
		 
		root$branch<-list()

		for(i in 1:nvalue)
		{
			##print("before")
			newset<-set[set[,attr[index]]==value[i],]	

			##print("after")
			nnewset<-nrow(newset)
			
			if(length(nnewset)==0)
			{
				nnewset=0
			}
			if(nnewset==0)
			{
				nnode<<-nnode+1
				mu<-mean(set[,1])
				if(mu>1.5)
				{
					root$branch[[i]]=list(label=2, attr=0,nbranch=0,branch=list())
				}
				else
				{
					root$branch[[i]]=list(label=1, attr=0,nbranch=0,branch=list())
				}

			}

			else
			{
				root$branch[[i]]=id3(newset, attr[-index])
			}
			
		}

		
		return(root)

	}
	
}






