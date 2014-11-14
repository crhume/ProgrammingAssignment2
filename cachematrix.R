## Directions: Put comments here that give an overall description of what your
## functions do
#   - These two functions will allow the caching of the matrix inversion
#   process to potentially eliminate having to repeat a costly subroutine.
#   The first function is called by 
#   > cm <- makeCacheMatrix(x) 
#   where x is any invertible (square) matrix.  The results can be stored for
#   easy recall, we are using cm here as a referencable name.
#   The second function is called by 
#   > cacheSolve(cm)
#   Where cm is the referencable name used earlier.  The result is the inverse
#   of matrix x, but repetetive calls to this same function will eliminate
#   additional steps and return the cached inverse instead.

# Tested this with the following simple cases:
# m<-matrix(c(1,2,-1,1),nrow=2,ncol=2)
# m <- matrix(c(-1,6,11,16,21,2,0,12,
#  17,22,3,8,1,18,23,4,9,14,1,24,5,10,15,20,-1),nrow=5,ncol=5)
# m <- matrix(rnorm(36),nrow=6,ncol=6)
# m <- matrix(rnorm(n*n),nrow=n,ncol=n)    # picked a few n
# m <- matrix(sample(400),nrow=20,ncol=20)


## Directions: Write a short comment describing this function
#  This function produces a list object which allows
#  the inverse of a matrix to be cached so it only needs 
#  to be computed a single time.
#  subsequent calls to cachesolve will 
#  result in retrieving the inverse stored in this 
#  cached object.

makeCacheMatrix <- function(x = matrix()) {
    # inv will store the inverse of the matrix if present
    inv <- NULL
    # set function will replace the matrix present with 
    # the requested matrix and simultaneously erase the
    # matrix inverse which was cached
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    # getter function returns the matrix
    get <- function () x
    # setinv function caches the inverse of x
    setinv <- function(inverse) inv <<-inverse
    # getinv function returns the inverse of x once cached
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Directions: Write a short comment describing this function
#  Performs the function solve for a matrix only
#  if that matrix hasn't already been solved for
#  by checking to see if the inverse of the matrix 
#  has already been cached in the above object construction
#  we assume in this process that the matrix is
#  invertible (square, non 0 determinant, etc.)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    # loads the value of the inv element of the cached matrix object
    inv <- x$getinv()
    # checks to see if that inv object is present.  If it is, the
    # value is read rather than calculated.
    if(!is.null(inv)){
        message("reusing cached inverse")
        # return and quit
        return(inv)
    }
    # if inv is null, we calculate the inverse using solve (note the call to
    # the get to return the matrix contents of the cacheMatrix object)
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
    inv
}

