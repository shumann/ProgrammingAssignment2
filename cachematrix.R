## Program by Shumann

## Matrix inversion is usually a costly computation and their may be some benefit to caching the inverse of a 
## matrix rather than compute it repeatedly.


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## makeCacheMatr requires a valid invertible matrix as its input.

makeCacheMatrix <- function(x = matrix()) {
  
  # Check for a valid matrix  
  if (!is.matrix(x)) stop("Not a valid matrix"); 
  
  # Basic Check for an invertible matrix
  if (ncol(x) != nrow(x)) stop("The matrix is not invertible"); 
  
  inv <- NULL
  setmatrix <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  getmatrix <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(getmatrix = getmatrix,
       setinverse = setinverse,
       getinverse = getinverse)

}


##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$getmatrix()
  inv <- solve(data, ...)
  x$setinverse(inv)
 inv
    
}
