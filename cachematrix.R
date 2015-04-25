## Those functions  are able to cache results of inverting a matrix.
## If the matrix has not changed the second function returns cached value, if not calculates the inverse

## This function creates a special "matrix", which is a list containing functions to 
## set-get the value of the matrix and set-get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This function works with the special "matrix"(list) created by the first function
## if the value of the matrix has not changed it creates a cached value, otherwise calculates inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
  
  
}

