## These two functions can create a special object that stores a 
## matrix and caches its inverse. In this case, we needn't compute
## the inverse repeatedly once it has been computed and cached.

## This function creates a cache, which contains 4 functions:
##      1.set: set a matrix, and set inverse"s" to NULL at first.
##      2.get: return to the matrix.
##      3.setsolve: set inverse of the matrix to "s".
##      4.getsolve: return to the inverse"s".

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, 
             get = get, 
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function reads the "s", and checks whether the "s"
## has been computed. If so, return inverse"s" without 
## computing again; if not, read the matirx in cache and 
## compute its inverse.

cacheSolve <- function(x) {
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data)
        x$setsolve(s)
        s
        ## Return a matrix that is the inverse of 'x'
}
