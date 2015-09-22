## A function to create a vector of 4 functions:
## (1) set the value of the vector
## (2) get the value of the vector
## (3) set the inv matrix
## (4) get the inv mean

makeCacheMatrix <- function(x = matrix()) {
	    invx <- NULL
        set <- function(y=matrix()) {
                x <<- y
                invx <<- NULL
        }
        get <- function() x
        
        setinv <- function(ix) invx <<- ix
        getinv <- function() invx
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## A function to invert the matrix created with makeCacheMatrix. However, it first checks to see
## if the inversion has already been calculated. If so, it gets the inv table from the cache
## and skips the computation. Otherwise, it calculates the inversion, and sets the value of
## via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        datamatrix <- x$get()
        m <- solve(datamatrix, ...)
        x$setinv(m)
        m
}

