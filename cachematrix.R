## The first function, makeCacheMatrix creates a special "vector", which is a list containing a function to set and get inverse
## by using the built-in function "solve".

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(solve) m <<- solve
        getmean <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The second function cacheSolve utilized the result from the first function and uses a conditional structure to extract the 
## inverse if it is already there or calculate it otherwise.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}
