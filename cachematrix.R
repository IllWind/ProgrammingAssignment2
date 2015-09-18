## R uses lexical scoping. The following functions show
##   a use of lexical scoping in R.
##
## This function creates and returns functions to manage a 
##	 cache containing a matrix in the enclosing environment.
##   The created functions are:
##        setCache
##        getCache
##        setInverse
##        getInverse
##   The matrix must be invertible. (No code is included
##    	  to test or assure the matrix is invertible.)
##   
##   Usage example:  smallMat<-makeCacheMatrix()
##                   smallMat$setCache(matrix(1:4,2,2))
##
##   Then:           smallMat$getCache()          [,1] [,2]
##                                          [1,]     1    3
##                                          [2,]     2    4
##
makeCacheMatrix <- function(x=matrix()) {
	cache <- NULL
	# this function sets the value of the matrix in the cache
	setCache <- function(y) {
		x <<- y
		cache <<- NULL
	}
	# this function gets the value of the cached matrix 
	getCache <- function() {
		x
	}	
	# this function sets the inverse of the cached matrix
	setInverse <- function(solve) {
		cache <<- solve
	}	
	# this function gets the value of the inverse
	getInverse <- function() {
		cache
	}	
	# this stores and prints a list of the functions created above
	list(
		setCache = setCache,
  		getCache = getCache,
		setInverse = setInverse,
		getInverse = getInverse
	)
 }

##  This function checks to determine if the inverse
## 	  has been calculated.  If not, this function
## 	  calculates the inverse of the cached matrix
## 	  then stores the inverse.  Otherwise it returns
## 	  the value of the stored inverse.
##
##    Using the above example matrix, the first time
##    cacheSolve is called it returns:
##                   cacheSolve(smallMat)     
##                                           [,1] [,2]
##                                     [1,]    -2  1.5
##                                     [2,]     1 -0.5
##    subsequent calls return:
##                   cacheSolve(smallMat)
##                                     getting cached inverse     
##                                           [,1] [,2]
##                                     [1,]    -2  1.5
##                                     [2,]     1 -0.5
##
 cacheSolve <- function(x, ...) {
	cache <- x$getInverse()
	if(!is.null(cache)) {
		message("getting cached inverse")
		return(cache)
	}
	data <- x$getCache()
	cache <- solve(data, ...)
	x$setInverse(cache)
	cache
}
##
##  The original matrix and its inverse can, by inspection,
##    be seen to produce the identity matrix.  One can
##    verify that (the identity matrix) by the following:
##
##      smallMat$getCache() %*% smallMat$getInverse()
##
##    The return is as follows:
##  
##            [,1] [,2]
##       [1,]    1    0
##       [2,]    0    1
