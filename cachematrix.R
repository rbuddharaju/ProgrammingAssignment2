## This function takes creates setters and gutters for matrix inversion 
## Function will initialize a matrix and then also initialize another matrix variable imv to 
## cache the matrix inverse for consecutive usage

## To test this one can follow below steps
## m1 <- matrix(1/2, -1/4, -1, 3/4)
## myMatrix_object <- makeCacheMatrix(m1)
## and then call by passing the matrix object to cacheSover function as below
## cacheSove(myMatrix_object)
## you should see below matrix
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4

makeCacheMatrix <- function(xm = matrix()) {
  imv <- NULL
  set <- function(y) {
    xm <<- y
    imv <<- NULL
  }
  
  get <- function(){ return(xm)} 
  setInverse <- function(inv) {imv <<- inv}
  getInverse <- function(){imv} 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## When user calls this function this will take the object of type
## makeCacheMatrix and returns the inverse object from cache if it is 
## already created or else it will create one

cacheSolve <- function(x, ...) {
  imv <- x$getInverse()
  if(!is.null(imv)) {
    message("getting cached data")
    return(imv)
  }
  data <- x$get()
  xm <- solve(data, ...)
  x$setInverse(xm)
  xm
}


