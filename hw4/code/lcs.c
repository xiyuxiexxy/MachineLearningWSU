char *seq1[]
char *seq2[]



int LCS(int x, int y)
{
	if(x==0 ||y==0)
		return 0;
	else if(strcmp( strcmp(seq1[x],seq2[y])==0)
	{
		int result= LCS(x-1,y-1);
		result++;
		printf("%s\t",seq1[x] );
		return result;
	}
	else
	{
		printf("x-1,y\n");
		int result1=LCS(x-1,y);

		printf("x,y-1\n");
		int result2=LCS(x,y-1);
		if(result1<result2)
			return result1;
		else 
			return result2;
	}



}

char line[1024];
int i loadseq(char *filename, char* seq[])
{

	FILE *fin=fopen(filename,"r");
	int i=0;
	while(fgets(line, 1024,fin ))
	{
		char*gene=strtok(line,"\n");
		seq[i]=strdup(gene);
		i++;
	}
	fclose(fin);
	return i;

}
int main()
{
	
	FILE *strain=fopen(argv[2],"r");

	int length=loadseq(argv[1],seq1);
	int i=0;
	for(i=0;i<length;i++)
		printf("seq1[i]\n");

	

	//loadseq(argv[2],seq2);

}
