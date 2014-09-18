## ntdatascience
## purpose: to improve performance on calculating the inverse of the matrix
##   we will cache matrixes and the corresponding inverse
## ntdatascience programming assigment 2

## makeCacheMatrix: creates a custom vector object sot that we can manage caching
# 1. set/get the matrix
# 2. set/get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
          x <<- y
          m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse )
}


## cacheSolve: return the inverse of the matrix
## if matrix is in cache then return the already calculted inverse
## otherwise, store the matrix with the calcuted inverse so that it will be cached
## and then return this calculated inverse

cacheSolve <- function(x, ...) {

      message("cacheSolve")
      m <- x$getinverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinverse(m)
      m

}
