## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#  This function produces a list object which allows
#  the inverse of a matrix to be cached so it only needs 
#  to be computed a single time.
#  subsequent calls to cachesolve will 
#  result in retrieving the inverse stored in this 
#  cached object.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function () x
    setinv <- function(inverse) inv <<-inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
#  Performs the function solve for a matrix only
#  if that matrix hasn't already been solved for
#  by checking to see if the inverse of the matrix 
#  has already been cached in the above object construction
#  we assume in this process that the matrix is
#  invertible (square, non 0 determinant, etc.)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)){
        message("getting cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    inv
}

# Tested this with the following simple cases:
# m<-matrix(c(1,2,-1,1),nrow=2,ncol=2)
# m <- matrix(c(-1,6,11,16,21,2,0,12,
#  17,22,3,8,1,18,23,4,9,14,1,24,5,10,15,20,-1),nrow=5,ncol=5)
# m <- matrix(rnorm(36),nrow=6,ncol=6)
