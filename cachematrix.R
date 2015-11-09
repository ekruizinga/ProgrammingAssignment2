# Introduction.
#
# Matrix inversion is a costly operation, so caching (rather than
# doing the inversion repeatedly) the inverse of a 
# matrix is a good idea if you have to use the inverse more than a few times or so  (there are
# also alternatives to matrix inversion that are not implemented here).

# This is a pair of functions that caches a matrix inverse. The input must be a square matrix, otherwise you'll get an
# error message.

# About the following function:
#
# makeCacheMatrix creates a list containing a function to
# 1. set the matrix
# 2. get the matrix
# 3. set the inverted matrix
# 4. get the inverted matrix

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL # set an empty data structure.
    set <- function(y) {
        x <<- y           #make data structure available across functions.
        inverted <<- NULL #make data structure available across functions.
    }
    get <- function() x
    setinverse <- function(inverse) inverted <<- inverse
    getinverse <- function() inverted
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
    
    #create special "vector" (really a list for the inverse handling function) 
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
    if(!is.null(inverted)) { #if I don't have a NULL data structure for inverted, I can proceed.
        message("I have something in cache for you, fetching it...") 
        return(inverted) #present the result
    }
    message("Alas, no cache available, so I have to compute an inverse for you...")
    data <- x$get() #so I need to work on data that is provided to me, so set them to a structure called 'data'
    inverted <- solve(data) #do the inversion and assign to 'inverted'
    x$setinverse(inverted) #then set the inverse in the cache
    return(inverted) #present the result
}

# This code was inspired by the example on caching the mean of vector (provided in the course material) and many examples on Github. 
# including Sefak Ilic: https://github.com/sefakilic/coursera-rprog-assignment2/blob/master/cachematrix.R and also 
