## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it repeatedly.  The following two functions
## allow you to create a matrix, which also cache the inverse of the matrix if it is invertible.
## This example assumes that the matrix is square and invertible, can can be solved by using
## the solve() function.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    set <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    ## Computing the inverse of a square matrix can be done with the solve function in R.
    ## For example, if X is a square invertible matrix, then solve(X) returns its inverse.
    ## For this assignment, assume that the matrix supplied is always invertible.
    myMatrix <- x$get()
    inverse <- solve(myMatrix, ...)
    x$setInverse(inverse)
    inverse
}

## Test function to test the execution
testSolution <- function() {
    m <- matrix(1:4,2,2)
    m1 <- makeCacheMatrix(m)
    print( m1 )
    print( m1$get() )
    ## debug(cacheSolve)
    print( cacheSolve(m1) )
    print( cacheSolve(m1) )
    print( m1$get()[1,] )
    print( m1$get()[,1] )
    m1$set(matrix (c(1,3,2,4),2,2))
    print( m1$get() )
    ## undebug(cacheSolve)
    print( cacheSolve(m1) )
    print( cacheSolve(m1) )
}


