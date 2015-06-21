## The following functions create create a special object that stores a matrix and caches its inverse  

## This function creates a special matrix which a list containg a function to set the value of 
## matrix, get value of the matrix, set the value of its inverse and get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list( set = set,get = get,
              setinv =setinv, getinv=getinv)
        
}


## The following function calcultes the inv of the special matrix from the above functon.
## The function checks to see if the invere has been calculated, if it is then the inverse
## is sourced from th cache.  if not it is calculated the function calculates the inverse 
## from the matrix and caches it using the setinv function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cache inverse")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        
}

