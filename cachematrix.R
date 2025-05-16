## Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # stores the cached inverse
  
  set <- function(y) {
    x <<- y
    inv <<- NULL  # reset inverse if matrix changes
  }
  
  get <- function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Function to compute (or retrieve cached) inverse of the matrix
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  mat <- x$get()
  inv <- solve(mat, ...)  # compute inverse
  x$setinverse(inv)
  inv
}
# Create a matrix
m <- matrix(c(1, 2, 3, 4), 2, 2)

# Make the special matrix
cm <- makeCacheMatrix(m)

# Compute the inverse (and cache it)
inv1 <- cacheSolve(cm)

# Retrieve cached inverse
inv2 <- cacheSolve(cm)  # this should show "getting cached inverse"

