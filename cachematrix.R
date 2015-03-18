## Jennifer Nicki Programming Assignment #2
## The functions makeCacheMatrix and cacheSolve are used to cache the inverse of a matrix

## makeCacheMatrix requires a matrix as an argument and includes 4 functions: set, get, setinverse, and getinverse
## set and setinverse use <<- to assign a value in the parent environment, not the current environment of the function 
##itself  

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(b) {  ## this function assigns a value (matrix) for x in the parent environment
    x <<- b
    m <<- NULL
  }
  get <- function() x  ##gets the value of matrix x
  setinverse <- function(inverse) m <<- inverse  ##assigns inverse in the parent environment
  getinverse <- function() m  ##gets the value of the inverse of matrix x
  list(set = set, get = get,  
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve checks if x$getinverse has a value or not.  If not, the inverse is found with the solve() function
## and the setinverse function() is used to assign the inverse of matrix x.  If x$getinverse already
## has a value, it is returned

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)   ##finds the inverse of the matrix x
  x$setinverse(m)
  m
        
}


