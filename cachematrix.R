##These two functions create a matrix in list form and then caches the
##inverse of the matrix.  Running cacheSolve again will recall the inverse
##from memory.  NOTE: for cacheSolve to work you must use an input of the
##type makeCacheMatrix()

##This first function creates a "matrix" which is really a list of 4 functions:
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


##This function calculates the inverse of the "matrix" created in the function
##makeCacheMatrix above.  It first checks to see if the inverse has already
##been calculated and if so, gets the inverse from the cache and skips the
##computation. Otherwise it calculates the inverse of the matrix and sets
##the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
