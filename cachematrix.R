## Put comments here that give an overall description of what your
## functions do
## The pair of functions cache the inverse of a matrix

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inverse<-NULL
  set<-function(y){
    x<<-y
    Inverse<<-NULL
  }
  get<-function()x
  setInverse<-function(I){
    Inverse<<-I
  }
  getInverse<-function()Inverse
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## Write a short comment describing this function
## Compute the inverse of the matrix returned by the makeCacheMatrix function 
## above if it hasn't been inversed, otherwise just return the inverse 
## of the matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inverse<-x$getInverse()
  if(!is.null(Inverse)){
    message("getting cached data")
    return(Inverse)
  }
  Matrix<-x$get()
  Inverse<-solve(Matrix)
  x$setInverse(Inverse)
  Inverse
}
