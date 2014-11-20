##makeCacheMatrix and cacheSolve work for caching inverse of a given matrix 
##and returning the cached inverse value if already cached.

## makeCacheMatrix outputs a list of functions 
##capable of storing and accessing a given matrix and its inverse.
##set and get are for setting and getting the matrix.
##setinverse and getinverse are for setting and getting the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


##Given the special object produced by makeCacheMatrix, 
##cacheSolve returns the inverse of the matrix.
##If the inverse of the matrix has not been cached yet, 
##it is calculated and then stored using the setinverse function. 
##If the inverse is already set, the cached value is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
