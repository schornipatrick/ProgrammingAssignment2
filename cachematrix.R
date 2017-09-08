## The functions makeCacheMatrix and solveCache are explained and written.

## Make Cache Matrix takes a m*n matrix that shall get inversed.
## It returns a m*2n matrix, containing the original and the (empty) inverse matrix of the same dimension

makeCacheMatrix <- function(x = matrix()) {
  # creates a special "matrix" object that can cache its inverse
  
  # inv is a matrix, full of NAs, with same dimensions as x
  inv <- matrix(nrow = nrow(x), ncol = ncol(x))
  
  # set the value of the matrix
  set <- function(mat) {
    x <<- mat
    inv <<- matrix(nrow = nrow(x), ncol = ncol(x))
  }
  # 
  # # get and set functions
  # get <- function() x
  # setinv <- function(solve) inv <<- solve
  # getinv <- function() inv
  
  # Build and return Cmat by collumn binding x and inv
  Cmat <- cbind(x, inv)
  Cmat
}


## CacheSolve takes a modified matrix, containing the cache with the inverse
## returns the inverse of the original matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  # get inv and see if there are NAs
  # If no NAs --> return cached data
  col <- ncol(x)
  invcol <- (col/2 + 1):col
  inv <- x[, invcol]
  
  if (!is.na(inv[1,1])) {
    message('getting cached data..')
    return(inv)
  }
  
  # if no cached data: Get matrix and calculate inverse
  matcol <- 1:(col/2)
  mat <- x[, matcol]
  inv <- solve(mat, ...)
  # delete NAs and bind inverse to matrix (--> save inverse in cache)
  x <- cbind(mat, inv)
  # newmat is a global variable (matrix with inverse in cache) that gets assigned to the matrix in the function call stack
  newmat <<- x
  # return inverse
  inv
}





