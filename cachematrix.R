#This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix(numeric)) {
m <- NULL
        set <- function(y) { #set the value of the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x #get the value of the matrix
        setinverse <- function(solve) m <<- solve  #set the value of the inverse
        getinverse <- function() m  #get the value of the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


# This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        m <- x$getinverse()  #get matrix
        if(!is.null(m)) {	# check cache and get inverse of matrix if previously computed
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)	#if not cached, compute inverse of matrix
        m

}
