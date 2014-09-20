## Written by Roger Kaufmann, based on example code in the assignment

## These functions are used to chache the inverse of a matrix
## As given in the assignment, they assume that the matrix supplied is invertible
## (i.e. matrix is not singular and matrix is square)

## makeCacheMatrix returns a list containing functions to 
## set and get the (current) value of the matrix and, similarly, 
## setinverse and getinverse for setting/getting the inverse of the matrix
## Note: setinverse should always be called with the solve function
## e.g. x$setinverse(solve(x$get()))

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        # Check if the new matrix object is not equal to the stored one
        if (!identical(x, y)) {
            x <<- y
            # Setting a new matrix object requires the inverse to be set to NULL
            inverse <<- NULL
        }

    }
    get <- function() x
    setinverse <- function(new_inverse) inverse <<- new_inverse
    getinverse <- function() inverse
    
    list (set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)

}

## chacheSolve returns a matrix that is the inverse of x and checks whether
## the inverse has already been calculated.
## Precondition: x has to be a list created with makeCacheMatrix()

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    
    # If inverse is not NULL return chached inverse
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    # Else calculate the inverse of the matrix in x with solve()
    inverse <- solve(x$get(), ...)
    # store it 
    x$setinverse(inverse)
    # and return the inverse
    inverse
}
