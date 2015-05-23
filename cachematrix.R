################################################################################
##                                                                            ##
##  # FUNCTION: makeCacheMatrix                                               ##
##  # AUTHOR: Carlos Rabazo                                                   ##
##  # PURPOUSE: creates a special "matrix", which is really a list containing ##
##    a function to:                                                          ##
##      1.- set the value of the matrix                                       ##
##      2.- get the value of the matrix                                       ##
##      3.- set the value of the inverse of the matrix                        ## 
##      4.- get the value of the inverse of the matrix                        ##
##  # INPUT PARAMETERS: a matrix                                              ##
##  # OUTPUT PARAMETERS:  a list o 4 functions (set the matrix,get the matrix ##
##                                              set the inverse of the matrix,##
##                                              get the inverse of the matrix)##
##                                                                            ##
################################################################################
makeCacheMatrix <- function(x = matrix()) {
  ## When we create the special matrix, the inverse value is NULL
  inverse <- NULL
  
  ######################################
  ## The set function sets the matrix ##
  ######################################
  set <- function(y) {
    ## We assign the matrix to the x parameter and NULL to the inverse parameter
    ## in an environment that is different from the current environment
    x <<- y
    inverse <<- NULL
  }
  
  #########################################
  ## The get function returns the matrix ##
  #########################################
  get <- function() x
  
  ##################################################################
  ## The setinverse function sets the inverse value of the matrix ##
  ##################################################################
  ## We assign the inverse of the matrix matrix to the inverse parameter 
  ## in an environment that is different from the current environment
  setinverse <- function(inverse_param) inverse <<- inverse_param
  
  ###########################################################
  ## The getinverse function returns inverse of the matrix ##
  ###########################################################
  getinverse <- function() inverse
  
  ## We returns a list with the four functions (set matrix, get matrix,
  ## set the inverse of the matrix, get the inverse of the matrix)
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


################################################################################
##                                                                            ##
##  # FUNCTION: cacheSolve                                                    ##
##  # AUTHOR: Carlos Rabazo                                                   ##
##  # PURPOUSE: This function calculates the inverse of the special matrix    ##
##    created with the makeCacheMatrix function. It first checks to see if    ##
##    the inverse has already been calculated. If so, it gets the inverse     ##
##    from the cache and skips the computation. Otherwise, it calculates the  ##
##    inverse of the matrix and sets the value of the inverse in the cache    ##
##    via the setinverse function.                                            ##
##  # INPUT PARAMETERS: a special matrix (the list of four functions returned ##
##    by the makeCacheMatrix function), and others parameters for the solve   ##
##    function if you need to transfer to it another parameters               ##                                                                
##  # OUTPUT PARAMETERS: The cached value of the inverse of the matrix if it  ##
##    exists, otherwise it calculates the inverse of the matrix, update it    ##
##    and returns it                                                          ##
##                                                                            ##
################################################################################
cacheSolve <- function(x, ...) {
  ## Get the inverse of the matrix  
  inverse <- x$getinverse()
  
  ## If the inverse exists, we return this value and skip the computacion
  if(!is.null(inverse)) {
    message("getting cached data-inverse")
    return(inverse)
  }
  ## If the inverse does not exist, we calculate, we update and we return it
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

################################################################################
##  SECTION OF TESTS  ##########################################################
################################################################################
my_matrix<- matrix(c(1,5,3,3,2,2,2,1,4),nrow=3,ncol=3) # we creates a matrix
my_specialMatrix <- makeCacheMatrix(my_matrix)  # we creates a special matrix

## The first time, it is calculated, stored and returns the inverse
cacheSolve(my_specialMatrix) 
## The second time says it was obtained from the cache and it return it
cacheSolve(my_specialMatrix) 
## The third and following, as the second
cacheSolve(my_specialMatrix) 
## I obtein the matrix and its inverse that is cached
my_specialMatrix$get()
my_specialMatrix$getinverse()
## I set another matrix and I obtain it again
my_specialMatrix$set(matrix(c(2,3,5,0,0,1,1,0,1),nrow=3,ncol=3))
my_specialMatrix$get()
## Others tests
my_specialMatrix$setinverse(cacheSolve(my_specialMatrix))
my_specialMatrix$getinverse()
cacheSolve(my_specialMatrix)