## This two functions computes the inverse of a square matrix, caches the result
## for reuse. 
## Sample usage:
## 1. instantiate the matrix using the makeCacheMatrix function
##    testMatrix <- makeCacheMatrix(matrix(1:4,2,2))
## 2. compute the inverse using the cacheSolve function
##    cacheSolve(testMatrix)
## 3. Reset the matrix value 
##    testMatrix$set(matrix(matrix(1:9),3,3))


## This function creates a 'matrix' object that can cache its inverse.
## It provides a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## m holds the inverse matrix and is set to NULL when matrix is first created
  m <- NULL
  
  ## this function sets the matrix being held to a new one and resets the inverse matrix back to null
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## this function returns the current matrix held
  get <- function() x
  
  ## this function caches the calculated inverse 
  setinverse <- function(inverse) m <<- inverse
  
  ## this function returns the cached inverse matrix
  getinverse <- function() m
  
  ## this provides a list of functions get/set the matrix and invsere matrix
  list(set = set, 
       get = get, 
       setinverse = setinverse, 
       getinverse = getinverse)
  
}


## This function returns a matrix that is the inverse matrix of 'x'.  It first checks to see
## if the inverse matrix is already solved. If so, it gets the inverse from the cache 
## and skips the computation.  Otherwise, it solves the matrix and sets the inverse matrix
## in the cache via the setinverse function

cacheSolve <- function(x, ...) {
        ## get the inverse matrix from the cache
        m <- x$getinverse()
        ## check if it available for reuse
        if (!is.null(m)) {
          ## found a valid inverse matrix, return the value
          message("getting cached data")
          return (m)
        }
        ## this part onwards will only fire if the matrix has not been solved and cached yet
        ## get the matrix
        data <- x$get()
        ## compute the inverse
        m <- solve(data)
        ## cache the inverse matrix for future reuse
        x$setinverse(m)
        m
}
