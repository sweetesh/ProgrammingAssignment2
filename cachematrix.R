
## This function creates a special matrix object and is capable of
## storing its inverse in the cache. Invverse of the matrix is stored
## in the variable inv. "set" function is used to assign values of matrix y
## to matrix x and setting its inverse to NULL by default. "get" is used to retrieve the value of matrix x. 
## "setinverse" function takes the value of inverse of matrix x and stores it in variable inv, which is the variable
## that would provide inverse of matrix x in future if matrix x has not changed. "setinverse" 
## would be called from cacheSolve function if matrix has changed in between and its inverse is not
## found in cache. "getinverse" is the function to get the inverse of a matrix, if inverse is not already stored
## in cache, it means that "inv" variable would be NULL for it and hence NULL would be returned.

makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function takes the argument as matrix x and first checks the cache to find if its 
## inverse is already computed and stored. Function getinverse does the task of cache lookup and
## if it finds x's inverse then it returns it otherwise it returns NULL. In case NULL is returned
## by getinverse then actual computation of inverse takes place using builtin function solve. 
## Afterward, this computed value of inverse is stored in cache using function setinverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
