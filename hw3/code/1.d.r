
npath<-0
Parse<-function(root,data)
{
	
	if(root$label!=0)
	{
		npath<<-npath+1
		return(root$label)

	}
	else
	{
			
		index<-root$attr
		#print(index)
		branchid<-data[index]
		#print(branchid)
		#print(root$values)
		
		if(match(branchid,root$values)==FALSE)
		{
			npath<<-npath+1
			return(0)
		}

		newset<-root$branch[[branchid]]

		npath<<-npath+1
		return(Parse(newset,data))
			

	}
	
}

