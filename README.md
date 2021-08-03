# R-Programming-Assignment-2
Assignment: Caching the Inverse of a Matrix

# THE FUNCTIONS
## Function 1
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x<<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, 
       get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Function 2
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message('obtaining cached data')
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

# TEST THE FUNCTION
## Create z matrix using the first function
zmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))

zmatrix$get()
 
## returned answer
[,1] [,2]
[1,]    1    3
[2,]    2    4

## Compute Inverse of a square Matrix

zmatrix$getInverse()
 
## returned answer

NULL

## Try solving using the second function

cacheSolve(zmatrix)

## returned answer
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5


## Try computing inverse again

 zmatrix$getInverse()
 
## returned answer

[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
