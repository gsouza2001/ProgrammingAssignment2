## Below are two functions that are used to create a special object that 
## stores a square matrix, and caches its inverse

## The first function, makeCacheMatrix, creates a list containing four 
## functions.  Each function does as follows:
##  Function 1. Set the value of the matrix (set())
##  Function 2. Get the value of the matrix (get())
##  Function 3. Set the value of the inverse of the matrix (setinv())
##  Function 4. Get the value of the inverse of the matrix (getinv())


makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The following function takes as an input the list generated in the previous 
## function.  It then calculates the inverse of the matrix created
## in the list resulting in the above function. 
## It first, however, checks to see if the inverse
## has already been calculated.  If so, it gets the inverse from the cache
## and skips the calculation.  Otherwise, it calculates the inverse using the
## "solve()" function, and sets the value of the inverse in the cache via the
## setinv() function. 

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    else {
        data <- x$get()
        inv <- solve(data)
        x$setinv(inv)
        return(inv)
    }
}
