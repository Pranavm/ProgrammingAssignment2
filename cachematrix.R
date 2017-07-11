## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special matrix object with getters and setters for the 
## matrix value and its inverse

## cache solve takes the special matrix object as argument and returns the inverse
## Computes the inverse the first time the first time the function is called and stores it
## Returns the stored value as long as the matrix value doesn't change

## Write a short comment describing this function
## Creates the special matrix object with getters and setters for inverse and the matrix value 
## Takes and matrix as argument and returns the special object which is a list
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## Takes the special matrix object as argument and returns the inverse of the matrix
## Caches the computed result and returns the cached result on subsequent calls

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    print(data)
    print(class(data))
    i <- solve(data)
    x$setInverse(i)
    i
}
