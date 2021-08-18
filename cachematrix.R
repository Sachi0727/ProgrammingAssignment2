## Cache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setSolve <- function(solve) inver <<- solve
  getSolve <- function() inver
  
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## computes the= inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inver <- x$getSolve()
  if(!is.null(inver)) {
    message("Getting the cached matrix...")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setSolve(inver)
  inver
}
