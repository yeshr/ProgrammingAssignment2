
## makeCacheMatrix and cacheSolve function are used to compute and cache
## expensive matrix inverse operation for later retrival if the matrix
## being operated on as not changed.

## makeCacheMatrix creates a closure to cahce expensive computations.
##
## x{matrix} A matrix for which an inverse needs to be computed and
##           cached. Defaults to an emtpy matrix.
## return{list} Returns a vector with methods to manipulate matrix for
##                which an inverse needs to be computed and cached
##                and computed inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    ## Intialize inverse varible to NULL
    inv <- NULL
    
    ## Setter to intialize/update the matrix
    ## to be operated on.
    ##
    ## y{Matrix} A matrix for which an inverse need to be
    ##          computed.
    set <- function(y) {
      ## Assign the passed in matrix to env variable
      ## that can be cached.
      ## TODO: Add check to test if passed in matrix is invertable to improve on it.
      x <<- y
      ## Invalidate any previously computed inverse.
      inv <<- NULL
    }
    
    ## Getter to read the matrix for which inverse is being
    ## computed.
    ##
    ## return{matrix}
    get <- function() x
    
    ## Setter to cache the computed inverse of a matrix.
    ##
    ## invMatrix{Matrix} Computed inverse of a matrix.
    setInverse <- function(invMatrix) inv <<- invMatrix
    
    ## Getter to retrieve cached inverse matrix.
    ##
    ## return{matrix}
    getInverse <- function() inv
    
    ## Create list with references to getters and setters
    ## and return.
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)  
}


## Solve for inverse of a cahced matrix.
## x{list}  List of methods to getter and setter of makeCacheMatrix.
## return{matrix} Inverse matrix of x
cacheSolve <- function(x, ...) {
    ## Get cached inverse matrix
    invMatrix <- x$getInverse()
    
    ## Check if invMatrix is not null return cached inverse matrix
    if(!is.null(invMatrix)) {
      message("getting cached data")
      return(invMatrix)
    }
    
    ## Since we don't already have computed inverse
    ## retrive the matrix to solve for.
    matrix <- x$get()
    
    ## Compute inverse of retrieved matrix.
    ## NOTE: Assuming passed in matrix is invertable.
    invMatrix <- solve(matrix)
    
    ## Cache the inverse of matrix for later.
    x$setInverse(invMatrix)
    
    ## Return a matrix that is the inverse of 'x'
    invMatrix     
}
