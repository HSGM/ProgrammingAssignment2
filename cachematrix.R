
###################################################################################################################
#OBJECTIVE:
#When supplied with an invertible matrix the following program will calculate                                
#the inverse of the matrix and store the inverse in the cache so that for subsequent calculations that require      
#the inverse of the matrix, the program will first check the cache to see if the inverse is already calculated  
# and stored in the cache and if so obtain it from cache rather than calculate it. If not present, then it will
#calculate it.             
#(This programme is an adaptation of Prof Roger Peng's programme to calculate and cache the mean of a  vector.)     
###################################################################################################################


##The function makeCacheMatrix creates a special "matrix" object
##and the function cacheSolve gets the inverse already present in the cacher or calculates it.
#if not present in the cache

## The result of running makeCacheMatrix is a list containing 4 functions
##These functions are

# 1:set()  sets the value of the matrix
# 2:get()  gets the value of the matrix
# 3:setinverse() sets the the value of the inverse matrix
# 4:getinverse() gets the value of the inverse matrix



makeCacheMatrix <- function(x = matrix()) {
  
  inverse_matrix<-NULL                 # Initialize the "inverse_matrix" variable to NULL; 
   
      set <- function(y) {             # y takes the matrixthat we want ot invert as the argument 
                                       # passed into makeCacheMatrix
                                       # because the function set is contained in 
                                       # makeCacheMatrix. 
    
      x <<- y                          #set x for the function environment to the matrix that we want to invert (y)
    
      inverse_matrix <<- NULL          #set inverse_matrix for the makeCacheMatrix to NULL
      
      }
  
  get <- function() x                 #the get function returns x
  
  
  
  setinverse <- function(inverse) inverse_matrix <<- inverse  #the inverse of the matrix( passed in as inverse) 
                                                              #is stored in the cache in
                                                              #the variable inverse_matrix
  
  getinverse <- function() inverse_matrix  #the getinverse function returns the inverse of the matrix
  
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
   
  
}


##The  function cacheSolve calculates the inverse of the  "matrix"
##created with the makeCacheMatrix. However, it first checks to see if the
##inverse has already been calculated. If so, it gets the inverse from the
##cache and skips the computation. Otherwise, it calculates the inverse of
##the matrix and sets the value of the inverse in the cache via the `setinverse`
##function.


cacheSolve <- function(x, ...) {
  
  
  inverse_matrix <- x$getinverse()    # from the list x gets the inverse of the matrix and assigns the 
                                      # inverse  from that environment to this one.
  
  if(!is.null(inverse_matrix)) {
    
         message("getting cached inverse")  # if inverse_matrix is not null it returns the value of the inverse.
    
         return(inverse_matrix)
         }
  
  matrix <- x$get()                     #if inverse_matrix is null then the matrix that was passed in
                                        #as an argument to makeCacheMatrix is assigned to 'matrix'. 
                                        
  
  inverse_matrix <- solve(matrix)      # inverse of the "matrix" is obtained
  
  x$setinverse(inverse_matrix)         
  
  inverse_matrix
}     
