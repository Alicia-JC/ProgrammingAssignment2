## Using these two functions, the inverse of a matrix is calculated and the 
## result is cached. This way, if the result is needed again, it is not necessary
##to calculate it again, 

## This function creates a special matrix, which is in reality a list with 4 functions, which set the value of the matrix,
## get them, set the inverse of the matrix and gets it. It is not calculated here but
##stored. Then, which is necessary in order to know whether the inverse has already
##been calculated for the matrix or not.

makeCacheMatrix <- function(x = matrix()) {
  sm <- NULL
  set <- function(y) {
    x <<- y
    sm <<- NULL
  }
  get <- function() x
  setinv <- function(solve) sm <<- solve
  getinv <- function() sm
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function calculates the inverse of the special matrix created in the above
##function. Firstly, it checks whether it has already been calculated and if so 
##it directly returns the value. Otherwise it calculates the inverse and keeps it
##in the setinv function from the above function

cacheSolve <- function(x, ...) {
  sm <- x$getinv()
  if(!is.null(sm)) {
    message("getting cached data")
    return(sm)
  }
  data <- x$get()
  sm <-solve(data,...)
  x$setinv(sm)
  sm
}
