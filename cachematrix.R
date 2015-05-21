## Functions makeCacheMatrix() and cacheSolve() are used to compute and cache a matrixInverse.  
## The original matrix is also cached.  
##
## An extra function testM() provides a quick sanity check of the two functions.  
## It is called without parameters and returns a couple of identity matrices if everything 
## is working


## MakeCacheMatrix() is used to cache a matrix and its inverse.
## The data can be passed in when the function is first called,
## and it can be set or changed later using set().

makeCacheMatrix <- function(x = matrix()) {
  ##initialize
  myMatrixInverseCache <- NULL
  
  ##fcn 
  set <- function(newDataMatrix) {
     if (!identical(x,newDataMatrix))
     {
         ##new data, old inverse if invalide
         x <<- newDataMatrix  
         myMatrixInverseCache <<- NULL
     }
  }
  
  #data getter
  get <- function() x
  
  #inverse getter and putter
  setInverse <- function(theInverseMatrix) myMatrixInverseCache <<- theInverseMatrix
  getInverse <- function() myMatrixInverseCache
  
  #create a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSovle returns the inverse of the matrix .  It is a companion function to makeCacheMatrix()
##
## Example usage:   myMatrix <- matrix(c(1,1,1,-1),2,2)
## Typical usage:   mCache <- makeCacheMatrix( myMatrix )
##                  myMatrixInv <- cacheSolve(mCache)
##  
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ## start by checking to see if there's a cached inverse
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix)) {
    message("getting cached matrix")
    return(inverseMatrix)
  }
  
  ## if we got here then  no inverse cached so get the data, invert and cache
  message("computing matrix inverse")
  data <- x$get()
  inverseMatrix <- qr.solve(data, ...)
  x$setInverse(inverseMatrix)
  
}

testM <- function ()
{
   myMatrix <- matrix(c(1,1,1,-1),2,2)
   mCache <- makeCacheMatrix(myMatrix)
   
   ## this call shows the message "computing matrix inverse"
   myMatrixInv <- cacheSolve(mCache)
   
   ##if everything works, should get the identity matrix
   test1 = myMatrixInv%*%myMatrix
   
   ## this call should show the message "getting cached matrix"
   myMatrixInv <- cacheSolve(mCache)
   test2 = myMatrixInv%*%myMatrix
   
   ## use a different matrix to test the set function
   myMatrix <- matrix(rnorm(16),4,4)
   mCache$set(myMatrix)
   mCache <- makeCacheMatrix(myMatrix)
   myMatrixInv <- cacheSolve(mCache)
   test3 = myMatrixInv%*%myMatrix
   
   ## if everything works, should return two 2x2 and one 4x4 identity matrices
   
   list(test1,test2,test3)
   
}
