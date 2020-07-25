##FirstExercise
##Sorry for my english, I´m working on improving :)
##In this function we created a function that create the necessaries 
##conditions for the cachesolved function can work. It read the original matrix and 
##calculate its inverse

MakeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse){
        inv <<- inverse
    } 
    getinverse <- function(){
        inv
    } 
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The following function calculates the Inverse of a Matrix created with the above function. 
##However, it first checks to see if the Inverse of a Matrix has already been calculated. 
##If so, it gets the Inverse of a Matrix from the cache and skips the computation. 
##Otherwise, it calculates the Inverse of a Matrix of the data and 
##sets the value of the Inverse of a Matrix in the cache via the setinverse function

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}