# makeCacheMatrix-This function returns a list of 4 functions which are used to
#
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
# params: input-x=matrix(),an optional parameter. 
#          if not provided can be provided using set function of the list returned
#         output: list of functions mentioned above

makeCacheMatrix<-function(x=matrix()){
  inverse<- NULL
  set<-function(y){
    x<<-y
    inverse<<-NULL
  }
  get <- function()x
  setinverse<-function(i)inverse<<-i
  getinverse<-function()inverse
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


# cacheSolve-This function returns the inverse of the matrix. It first checks if
# the inverse has already been computed .it gets the inverse if not null 
# and does not compute it again.otherwise it computes sets the inverse in 
# the cache so that it can be retreived next time without computing.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  print(inverse)
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  message("setting inverse")
  x$setinverse(inverse)
  x$getinverse()
  inverse
}

#you can check the above two functions by running
#mat<-matrix(c(4,2,7,6),nrow=2,ncol=2)
#l<-makeCacheMatrix(mat)
#cacheSolve(l)
