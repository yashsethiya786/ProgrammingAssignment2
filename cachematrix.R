## Put comments here that give an overall description of what your
## functions do

## This function creates a  "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <-NULL
  set <-function(y){                    ##set function
    x<<-y
    inv<<-NULL
  }
  get <-function() {x}                  ##get function
  setInverse <- function(inverse){inv <<-inverse}            ##setInverse function
  getInverse <- function() {inv}                             ##getInverse function
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. If the inverse has already been calculated (and the matrix has not changed), then
##`cacheSolve` should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inv <-x$getInverse()
  if(!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  mat <-x$get()
  inv <-solve(mat,...)
  x$setInverse(inv)
  inv
}

        

