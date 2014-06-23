## function returns an inverse of a matrix

# The following function does the following:
# 1. set the value of a matrix
# 2. gets the value of a matrix
# 3. sets the inverse value of a matrix
# 4. gets the inverse value of a matrix

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


# the following fuction solves and creates the inverse
# of a previously inputted matrix. If it has been solved
# previously the cachced data is retrieved instead

cacheSolve <- function(x, ...) {
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
