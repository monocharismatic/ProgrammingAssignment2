## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: This function creates a special "matrix" object
# that can cache its inverse.

# > cacheH3 <- makeCacheMatrix(h3)
# > cacheH3$get()
#           [,1]      [,2]      [,3]
# [1,] 1.0000000 0.5000000 0.3333333
# [2,] 0.5000000 0.3333333 0.2500000
# [3,] 0.3333333 0.2500000 0.2000000
# > cacheH3$set(h3)

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inverse <<- i
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# cacheSolve: This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix function.
# If the inverse has already been calculated (and the matrix has not changed),
# then the cachesolve should retrieve the inverse from the cache.

# > cacheH3$getinverse()
# NULL
# > cacheSolve(cacheH3)
#      [,1] [,2] [,3]
# [1,]    9  -36   30
# [2,]  -36  192 -180
# [3,]   30 -180  180
# > cacheH3$getinverse()
#      [,1] [,2] [,3]
# [1,]    9  -36   30
# [2,]  -36  192 -180
# [3,]   30 -180  180
# > cacheSolve(cacheH3)
# getting cached data
#      [,1] [,2] [,3]
# [1,]    9  -36   30
# [2,]  -36  192 -180
# [3,]   30 -180  180

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data)
    x$setinverse(i)
    i
}
