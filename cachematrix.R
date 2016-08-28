## This function creates a special "matrix" object that can cache its inverse
  

## makeCacheMatrix creates custom matrix type capable of running four functions
## set stores the matrix in cache, get recalls the matrix
## setInverse and getInverse do the same but for the inverse of the original matrix
  
makeCacheMatrix <- function(x = matrix()){    
  mat <- NULL
  set <- function(y){
    x <<- y  
    mat <<- NULL #to assign a value to "mat" in an environment 
  }
  get <- function() x #get matrix
  setInverse <- function(solve) mat<<- solve #set inverse matrix
  getInverse <- function() mat #get inverse matrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)  ## create list of functions
  }
  


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
## above. If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.cacheSolve take a custom
## matrix type created by the makeCacheMatrix function

  cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## Return a matrix that is the inverse of 'x'
  mat <- x$getInverse()               #query the x matrix's cache
  if(!is.null(mat)){                  #if there is a cache the inverse has been previously calculated
       message("getting cached data")    # sent message indicating this is just cache 
       return(mat)                       # return the cache  
  }
  data <- x$get()                     # get the matrix used by makeCacheMatrix function 
  mat <- solve(data, ...)             # calculate the inverse of the matrix
  x$setInverse(mat)                     # store the inverse matrix in cache using the makeCacheMatrix set function
  }
