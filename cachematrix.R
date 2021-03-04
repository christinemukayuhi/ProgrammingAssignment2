## Put comments here that give an overall description of what your
## functions do
##The inverse of a matrix plays the same roles in matrix algebra as 
##the reciprocal of a number and division does in ordinary arithmetic.

## Write a short comment describing this function
## Note that to inverse matri it should be a square matrix.
##determinant of the matrix must not be zero

## makeCacheMatrix:
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## The point is that x and inv are stored in the enclosing environment of the set, get, 
## setInverse, getInverse functions. That means the environment within which 
## they were defined, i.e., the environment created by the makeCacheMatrix().

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}
