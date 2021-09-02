## this function creates a list that set value of matrix ;
## get the value of matrix;
## compute the inverse matrix and get the computed value of original matrix.
## value of the vector (reference to list)  

makeCacheMatrix <- function(x = matrix()) {
  MyInvMatrix<-NULL
  set<-function(y){
    x<<-y
    MyInvMatrix<<-NULL
  }
  get <- function() x
  setInv <- function(solve) MyInvMatrix <<- solve(x)
  getInv <- function() MyInvMatrix
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## Write a short comment describing this function

## this function check if the inverse matrix already exists otherwise computes the inverse matrix and store it in the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' (MyInvMatrix)
  
  MyInvMatrix <- x$getInv()
  if(!is.null(MyInvMatrix)) {
    message("getting cached data")
    return(MyInvMatrix)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInv(MyInvMatrix)
  MyInvMatrix
}

