## cachematrix.R
## cachematrix.R caches the result of the R function "solve(x)"; where x is
##      a square invertible matrix      
##
## Description - cachematrix.R consists of two function instances; 
##      makeCachedMatrix() instantiates a special "vector", which is a list 
##      of functions (set, get, setInverse, getInverse) used within the 
##      to create (set) and retrieve (get) a cached matrix from the 
##      inputted square invertible matrix; cacheSolve() calculates the inverse
##      of the stored matrix and stores (caches) the result, if cacheSolve() is
##      called again with the same input it checks if the inverse result 
##      already stored and returns the value, if not it calclates the inverse
##
## Limitations - makeCacheMatrix() does not currently check the input matrix
##      is square or invertible, therefore if it is neither cacheSolve() may
##      return a warning or fatal ERROR.
##
## Author - Gregg O'Marr
## Date - 26 July 2014
## Version - 1.0
###############################################################################
##
##      Note: This script was adapted from a script provided through the 
##      Coursera.org "R Programming" course as part of the Coursera/JHU
##      Data Science Specialization
##
###############################################################################


## makeCaheMatrix()
## Description - function makeCaheMatrix() takes a square invertible matix and
##      instantiates a special "vector", a list consisting of the functions 
##      (set, get, setInverse, getInverse) to "set": store the input matrix; 
##      "get": return the input matrix; "setInverse": store/cache the inverted 
##      matrix calculated by cacheSolve(); "getInverse": return the 
##      stored/cached inverted matrix.  "set" and "setInverse" store thier
##      respective values in an environment different from the environment
##      makeCaheMatrix() is initiated from.
##      

makeCacheMatrix <- function(x = matrix()) {
        
        m <- NULL  ## initialize the inverse matrix
        
        ## set the input matrix and initialize the inverse matrix
        set <- function(y) {
                x <<- y     ## cache the input matrix
                m <<- NULL  ## initialize the cache inverse matrix
        } ## end function set()
        
        ## get the input matrix
        get <- function() x
        
        ## set/cache the inverse matrix
        setInverse <- function(inverse) m <<- inverse
        
        ## get the cached inverse matrix
        getInverse <- function() m
        
        ## makeCacheMatrix() is a list of functions
        list(set = set, 
             get = get, 
             setInverse = setInverse, 
             getInverse = getInverse)

} ## end function makeCacheMatrix()


## cacheSolve()
## Description - function cacheSolve() checks whether the input matrice's 
##      inverse have been previously calculated and stored/cached; if it has
##      been calculated the cached inverse matrix is returned; if it has not
##      been calculated (cached value == NULL) then cacheSolve() calculates 
##      the inverse, caches the result using the input list's setInverse() 
##      function and returns the result of the matrix inverse
## 

cacheSolve <- function(x, ...) {
        
        ## get the cached matrix
        m <- x$getInverse()
        
        ## check if the inverse matrix has been calculated and cached; if TRUE 
        ##      print retrieval message and result to screen
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        } ## end if
        
        ## if FALSE; get the input matrix, calculate inverse matrix, 
        ##      cache and return result
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        
} ## end function cacheSolve()
