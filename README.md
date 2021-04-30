library(MASS) ##used to calculate inverse for non squared as well as square matrices
> makeCacheMatrix<-function(x=matrix()){
+ inv<-NULL ##initializing inverse as NULL
+ set<-function(y){
+ x<<-y
+ inv<<-NULL
+ }
+ get<-function()x  ##function to get matrix x
+ setInverse<-function(inverse)inv<<-inverse
+ getInverse<-function(){
+ inver<-ginv(x)
+ inver%*%x  ##function to obtain inverse of the matrix
+ }
+ list(set=set,
+ get=get,
+ setInverse=setInverse,
+ getInverse=getInverse)
+ }
>  cacheSolve<-function(x,...) ##gets cache data
>  {
+ inv<-x$getInverse()
+ if(!is.null(inv))  ##checking whether inverse in Null
+ {
+  message("getting cached data")
+  return(inv) ##returns inverse value
+ }
+ mat<-x$get()
+ inv<-solve(mat,...) ##calculates inverse value
+ x$setInverse(inv)
+ inv  ##Return a matrix that is the inverse of 'x'
+ }
