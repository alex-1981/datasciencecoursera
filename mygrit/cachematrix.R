## Matrix inversion is usually a costly computation and there may be 
## some benefit to caching the inverse of a matrix rather than compute 
## it repeatedly. The next pair of functions can cache the inverse of 
## a matrix.

## makeCacheMatrix creates a special "vector", which is really a list 
## containing a function to:
##    set the value of the vector
##    get the value of the vector
##    set the value of the inverse of the matrix
##    get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setinversematrix <- function(inversematrix) m <<- inversematrix
   getinversematrix <- function() m
   list(set = set, get = get,
        setinversematrix = setinversematrix,
        getinversematrix = getinversematrix)
   
}


## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache. This function , assume that 
## the matrix supplied is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   
   m <- x$getinversematrix()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setinversematrix(m)
   m
   
}
