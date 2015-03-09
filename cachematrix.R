## functions used to do matrix inversion using cached inverse of an already
## calculated matrice, avoiding waste of coputing time

## function creates a special object that stores a matrix and caches its inverse
## in a different environment. Also creates setters and getters to
## access stored data
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    ## set Matrix values
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    
    ## get Matrix values
    get <- function() x
    
    ## set inverse cached value
    setinvert <- function(solve) m <<- solve
    
    ## get inverse cached value
    getinvert <- function() m
    
    list(set = set, get = get,
         setinvert = setinvert,
         getinvert = getinvert)
}


## This function returns the inverse of the special "matrix" created with
## `makeCacheMatrix`.
cacheSolve <- function(x, ...) {
    m <- x$getinvert()
    
    ## If the matrix was aready calculated once and nothing
    ## has changed, retrieve the inverse from cache
    if(!is.null(m)) {
        message("getting cached data")
        ## ends execution here returning cached value
        return(m)
    }
    
    ## data not cached, do inversion
    data <- x$get()
    m <- solve(data, ...)
    x$setinvert(m)
    m
}
