# Introduction.
#
# Matrix inversion is a costly operation, so caching (rather than
# doing the inversion repeatedly) the inverse of a 
# matrix is a good idea if you have to use the inverse more than a few times or so  (there are
# also alternatives to matrix inversion that are not implemented here).

#This is a pair of functions that caches a matrix inverse. The input must be a square matrix, otherwise you'll get an error message.

# About the following function:
#
# makeCacheMatrix creates a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverted matrix
# 4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inverted <<- inverse
    getinverse <- function() inverted
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# NOTE: the <<- syntax used within the function above ensures that the x and inv
# contents remain available across the two functions via the shared parent
# environment (I think...looking at: ?assignOps).

# About the following function:
#
# cacheSolve returns the inverse of the matrix. This function
# inverses the matrix returned by makeCacheMatrix above. If the
# inverse is already there, then cacheSolve will retrieve this inverse, from the
# cache.
# It is assumed that the matrix is always invertible.

cacheSolve <- function(x) {
    inverted <- x$getinverse()
    if(!is.null(inverted)) {
        message("I have something in cache for you, fetching it...")
        return(inverted)
    }
    message("Alas, no cache available, so I have to compute an inverse for you...")
    data <- x$get()
    inverted <- solve(data)
    x$setinverse(inverted)
    return(inverted)
}

#This code was inspired by the example on caching the mean of vector provided in the course material and many examples on Github. 
