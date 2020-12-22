## Put comments here that give an overall description of what your
## functions do

## create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function (y)
    x<<- y
    m <<- NULL
  
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <-function() m
  list(set = set, 
       get = get, 
       setInv = setInverse, 
       getInv = getInverse)

}


## computes the invers of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
  m <- x$getInv( )
  if(!is.null(m)){
    message("getting cache data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
