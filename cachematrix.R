## Two functions are used to compute the inverse of a matrix, checking first whether
## it was already computed and if so taking its cache saved value

#makeCacheMatrix sets and gets the value of a matrix x and its inverse
makeCacheMatrix <- function(x = numeric()) {
    #i, the inverse of x, is initially set to NULL 
    i <- NULL
    
    #sets is a kind of method setting the value of the matrix to be inverted, x,
    # and the inverse matrix to NULL
    #Note that the matrix cannot be initialized with set, but with makeCacheMatrix. 
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    # returns the current value of the matrix
    get <- function() x
    
    # assigns inverse to i
    setinverse <- function(inverse) i <<- inverse
    
    #returns the current value of the inverse of x, i 
    getinverse <- function() i
    
    #makeCacheMatrix returns a list containing set, get, setinverse, and getinverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

#cacheSolve is a function to compute the inverse of a matrix x, 
#testing first whether the inverse was already computed and saved in cache

cacheSolve <- function(x, ...) {
    #i is a local variable, initialized calling the getinverse function of makeCacheMatrix
    #to recover the inverse saved in cache.
    i <- x$getinverse()
    #if i was already computed (not null) the saved value is returned
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    #else, if i has not been computed yet, the inverse is computed using the solve function
    data <- x$get()
    i <- solve(data, ...)
    
    #the new computed inverse is saved in makeCacheMatrix using setinverse
    x$setinverse(i)
    
    # returns the newly computed inverse
    i
}
