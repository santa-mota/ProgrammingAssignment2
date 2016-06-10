## Put comments here that give an overall description of what your
## functions do
##So,basically there are 2 functions.makeCacheMatrix takes in a matrix
##and creates a list containing values for the matrix and its inverse.
##The second function,cacheSolve,returns the inverse of the matrix,if it is
##cached,orcalculates the inverse,stores it,and then displays accordingly.

## Write a short comment describing this function

##This function is used to create a data structure in the form of a list
##to store the value of the matrix and its cached inverse.Also,it allows the
##user accessing to store the inverse data using calls like setinv.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv1) inv <<- inv1
  getinv <- function() inv
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}


## Write a short comment describing this function

##This is the function to check whether the matrix's inverse has been 
##cached or not.If yes,it returns the cached value,else it computes it anew.

cacheSolve <- function(x, ...) {
  invtemp <- x$getinv()
  if(!is.null(invtemp)){
    print("Returning cached data:")
    return(invtemp)
    }
  data <- x$get()
  invtemp <- solve(data)
  x$setinv(invtemp)
  invtemp
        ## Return a matrix that is the inverse of 'x'
}
