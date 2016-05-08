## This fucntions are constructed to be used together
## only for square invertible matrices. Given such a matrix M
## makeCacheMatrix(M) will create a cache list with the information of M 
## then 
## cacheSolve(M) will find the inverse of M. First it will check if such inverse was
## already calculated to use it. Otherwise it computes the new inverse and sets its value oin ## the list 
## This code can be tested using the following lines
## mat2<-matrix(c(1, 0 ,0, 1),2,2) #here we introduce an invertible matrix
## cacheSolve(mat2) #save info in the list
## cacheSolve(m2) # solves to find the inverse
## cacheSolve(m2) # the second time it recovers the inverse from cache
## m3<-makeCacheMatrix(mat3) # this is another test for 3x3 matrices
## cacheSolve(m3)


## This function saves the information of a matrix and its inverse for future computations
mat3<-matrix(c(7,2,1,0,3,-1,-3,4,-2),3,3)
## Write a short comment describing this function
     makeCacheMatrix <- function(x = matrix()) {

	m<-NULL
	set<-function(y){
		x<<-y
		m<<-NULL
	}
	get<-function()x
	setinv<-function(solve)m<<-solve
	getinv<-function() m
	list(set=set,get=get,
	setinv=setinv,
	getinv=getinv)
	
}

## This function computes the inverse of a matrix (saved with makeCacheMatrix)
## It first check its existence in the list, otherwise it computes a new value
	cacheSolve <- function(x, ...) {
	  m<-x$getinv()
	  if(!is.null(m)){
	  	message("getting cached inverse")
	  	return(m) 
	  }
	  data <- x$get()
	  m<-solve(data, ...)
	  x$setinv(m)
	  m
}