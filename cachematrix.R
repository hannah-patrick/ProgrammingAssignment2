## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


# makeCacheMatrix is a function which will produce a list of functions. 
# Executing this function on a matrix will set variable i to NULL.
# i will hold the cached value of the inverse.
# The set function takes a matrix y and assigns x from makeCacheMatrix this value, since <<- modifys values in parent levels. 
# The get function returns the matrix x.
# The setinverse fuction sets i in the makeCacheMatrix function to the inverted value supplied.
# Get inverse returns the value of i in the makeCacheMatrix function.
# List returns a list of the 4 functions produced.


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


## Write a short comment describing this function

# cacheSolve takes a list of fuctions produced by the makeCacheMatrix function.
# It first returns the current value of i as determined by the makeCacheMatrix function.
# The default value of i is null. 
# cacheSolve then checks if i is not null.
# If i is not null the cacheSolve function is ended returning the value of the cached inverse, i.
# Otherwise the matrix x in the makeCacheMatrix function is returned using the get function.
# The inverse of the matrix is calculated and assigned to i.
# i is cached in the makeCacheMatrix function using the setinverse function.
# And the function ends returning i. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}



#test
# a <- matrix(c(1,2,3,4), nrow=2, ncol=2)
# cachematrix <- makeCacheMatrix(a)
# cachematrix$get()
# cacheSolve(cachematrix)

# b <- matrix(c(2,3,4,5), nrow=2, ncol=2)
# cachematrix <- makeCacheMatrix(b)
# cachematrix$get()
# cacheSolve(cachematrix)



