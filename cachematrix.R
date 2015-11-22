## The functions in this file are intended to create a matrix object that can store in cache the value of 
## its inverse and to retrieve such value using the stored result if existing, thus speeding up the computation
## time

## makeCacheMatrix creates a matrix object that can store in cache the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invers) inv <<- invers
  getinv <- function() inv
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve computes the inverse of a matrix, looking first if such result has already been calculated and
## stored in the cache and using that value if that is the case

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinv(inv)
  inv
  
}



