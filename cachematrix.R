## this function creates a list of subfunctions  that cache the inverse of a matrix  ;

## creates a special matrix functions that cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  ## initialization of inverse matrix
  MyInvMatrix<-NULL
  
  ## sub functions taht set the matrix
  set<-function(y){
    x<<-y
    MyInvMatrix<<-NULL
  }
  
  ## getting the initial matrix  via a getter
  get <- function() x
  
  # setting the matrix : computation of the inverse matrix if needed
  
  setInv <- function(solve) MyInvMatrix <<- solve
  
  # getting the output of the computation via a getter 
  
  getInv <- function() MyInvMatrix
  
  # return the list of functions 
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## this function check if the inverse matrix has already been calculated (and remains unchanged), retrieve the stored value and return it.
## otherwise computes the inverse matrix and store it in the cache. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x' (MyInvMatrix)
  
  ## return a matrix inverse of the original matrix
  MyInvMatrix <- x$getInv()
  
  ## check the existence of any inverse calculated and return it if it's already set
  
  if(!is.null(MyInvMatrix)) {
    message("getting cached data")
    return(MyInvMatrix)
  }
  ## get the initial matrix 
  data <- x$get()
  ## compute the inverse of initial matrix (object) via function solve()
  MyInvMatrix<- solve(data, ...)
  
  ## set the inverse to the object
  x$setInv(MyInvMatrix)
  
  ## return the inverse matrix object 
  MyInvMatrix
}

