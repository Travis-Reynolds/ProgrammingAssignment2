## The two functions combine into a set of functions that create and cache the inverse of a matrix.
## Therefore, the inverted matrix, if unchanged, does not have to be recreated every time it is used. 

## NOTE: The additional comments in the function itself are to demonstrate comprehension.
## They would be removed if used outside of class. 

## makeCacheMatrix creates a series of functions that allows for a matrix to be set into the function,
## placed into cacheSolve function to be inverted, stored, and recalled using x$getinv.

makeCacheMatrix <- function(x = matrix()) {
    inv_m <- NULL ## creates the empty varaible inv_m
    Set <- function(y) {  ## creates a function that places a matrix into the makeCacheMatrix function
        x <<- y ## the selected matrix is moved to the parent environment
        inv_m <<- NULL ## inv_m is set to NULL in case it was assigned perviously
    }
    SetInv <- function(inv) ## pulls the inverted matrix created using cacheSolve
        inv_m <<- inv ## assigns the contents of the cached inverted matrix to inv_m in the parent environment
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
    inv <- x$getinv() ## retrieves the data in GetInv run in makeCacheMatrix
    if(!is.null(inv)) { ## determines if inv is NULL (empty), if an inverted matrix is in cache...
        message("Getting cached data") ## the message is printed...
        return(inv) ## the cached matrix is returned, and the function is closed
    }
    table <- x$get() ## pulls x (from Get in makeCacheMatrix) into the function  
    inv <- solve(table, ...) ## creates the inverted matrix
    x$setinv(inv) ## puts the inverted matrix into the makeCacheMatric function
    inv ## displays the inverted matrix
}
