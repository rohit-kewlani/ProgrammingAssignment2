## Matrix inversion is usually a costly computation and their may be some benefit to caching the 
## inverse of a matrix rather than compute it repeatedly

## Below are a pair of functions that cache the inverse of a matrix.
###########################################################################################################
## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
###########################################################################################################
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then 
## the cachesolve  retrieves the inverse from the cache.
###########################################################################################################

## Initiatilizing the matrix

makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Function to return the inverse Matrix post checking for a cached copy 
## In case it's already been computed, it returns the cache copy or else does a fresh computation

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



##Example for inversion of a 3x3 matrix
x=makeCacheMatrix(matrix(rnorm(9),3,3))
cacheSolve(x)
