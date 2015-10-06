`knn.predict` <-
function(train, test, y, dist.matrix, k=1,
    agg.meth=if (is.factor(y)) "majority" else "mean",
    ties.meth="min") {

#number of predictions to make
n<-length(test)

#sort the indexes for the training and test sets
if (is.unsorted(train)) train<-sort(train)
if (is.unsorted(test)) test<-sort(test)

#only need the rows for the test data and columns
#for the training data
d<-dist.matrix[test,train]

#only need the responses for the training data
if (length(y)>length(train)) y<-y[train]

#calculate closest neighbors
d<-t(apply(d,1,function(x) rank(x,ties.method=ties.meth)))

#return aggregate response for the k closest neighbors
apply(d,1,function(x) apply(data.frame(y[x<=k]),2,agg.meth))
}

