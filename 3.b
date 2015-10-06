R=-0.04 

U<-array(dim=c(3,3)) 
 
 for (i in 1:3) 
 for(j in 1:3) 
 U[i,j]=0 

 U[3,3]=1 
 U[3,2]=-1 

Q=array(dim=c(3,3,3)) 
for (i in 1:3) 
for (j in 1:3) 
for (m in 1:3) 
	Q[i,j,m]=0 
i=0 
while(1) 
{ 
	i<-i+1 
	Q[1,1,1]=R+U[1,2] 
	Q[1,1,2]=R+U[1,1] 
	Q[1,1,3]=R+U[2,1] 
	U[1,1]=max(Q[1,1,]) 

	 
	Q[1,2,1]=R+U[1,3] 
	Q[1,2,2]=R+U[1,2] 
	Q[1,2,3]=R+U[1,2] 
	U[1,2]=max(Q[1,2,]) 


	Q[1,3,1]=R+U[1,3] 
	Q[1,3,2]=R+U[1,3] 
	Q[1,3,3]=R+U[2,3] 
	U[1,3]=max(Q[1,3,]) 

	Q[2,3,1]=R+U[2,3] 
	Q[2,3,2]=R+U[1,3] 
	Q[2,3,3]=R+U[3,3] 
	U[2,3]=max(Q[2,3,]) 
	
		print(Q[1,1,1]) 	
		print(Q[1,2,1]) 	
		print(Q[1,3,3]) 	
		print(Q[2,3,3])
		print("===============")
	if(i>7)
		break; 	 
}
