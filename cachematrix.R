## Make the inverse of a matrix cacheable/retrievable,
## to avoid repeated computation

## Creates a special matrix object that enables us to 
## set/get values of the matrix and its inverse, so that the inverse can be cached.
makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(new_matrix){
                x <<- new_matrix
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(new_inv) inv <<- new_inv 
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Calculate the inverse of a matrix.
## If the result is in cache, retrieve this result instead of calculating again.
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        m <- x$get()
        inv <- solve(m)
        x$setinv(inv)
        inv
}