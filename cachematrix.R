## There are two functions below which computes the inverse of a matrix
## Inverse is only calculated if the result of the matrix is not available in cache

## The first function MakeCacheMatrix returns a list of following
  ## 1. Set the value of matrix
  ## 2. Get the value of matrix
  ## 3. Set the value of Inverse of matrix
  ## 4. Get the value of Inverse of matrix
makeCacheMatrix <- function(x = matrix()) {
  
 ## initialize matrix
  m<-NULL
  
  ## setter function to set values in different environment  
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  
  ## getter function to get value of matrix
  get<-function() x
  
  ## setter function to set inverse of matrix
  setmatrix<-function(solve) m<<- solve
  
  ## getter function to get inverse of matrix
  getmatrix<-function() m
  
  ## return list
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
  
}


## The second function cacheSolve does following:
## Reads through the list returned by makeCacheMatrix
## Check if the Inverse has been computed on the matrix
  ## 1. If not it computes the inverse and saves it in cache
  ## 2. If already computed get data from cache and return it

cacheSolve <- function(x, ...) {
        
  ## get inverse of x
  m<-x$getmatrix()
  
  ## If avaialble
  
  ## Not null means available in cache
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  
  ## if not available
  
  ## get matrix from the list
  matrix<-x$get()
  
  ## perform Inverse on the matrix
  m<-solve(matrix, ...)
  
  ## set the value of inverse to cache
  x$setmatrix(m)
  
  ## return inversed matrix
  m
  
}
