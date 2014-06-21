## The two functions combine into a set of functions that create and cache the inverse of a matrix.
## Therefore, the inverted matrix, if unchanged, does not have to be recreated every time it is used. 

## makeCacheMatrix creates a series of functions that allows for a matrix to be set into the function,
## placed into cacheSolve function to be inverted, stored, and recalled using x$getinv.

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL 
    Set <- function(y) {  ## creates a function that places a matrix into the makeCacheMatrix function
        x <<- y 
        inv_m <<- NULL ## inv_m is set to NULL in case it was assigned perviously
    }
    SetInv <- function(inv) ## pulls in the inverted matrix created using cacheSolve
        inv_m <<- inv 
    Get <- function() ## function that allows x to be pulled into cacheSolve 
        x
    GetInv <- function() ## function that holds the inverted matrix in cache
        inv_m
    list (set = Set, ## attaches the functions to the variable associated with makeCacheMatrix
          get = Get,
          setinv = SetInv,
          getinv = GetInv)
}


## cacheSolve checks to see if the matrix has been inverted and cached.
## If it has, then the inverted matrix is recalled from the cache. 
## Otherwise, the matrix is inverted and set so that it can be accessed by makeCacheMatrix.

cacheSolve <- function(x, ...){
    inv <- x$getinv() ## retrieves the cached inverted matrix (if present)
    if(!is.null(inv)) { 
        message("Getting cached data") 
        return(inv) 
    }
    table <- x$get()   
    inv <- solve(table, ...) ## creates the inverted matrix (if stored in cache)
    x$setinv(inv) 
    inv 
}
