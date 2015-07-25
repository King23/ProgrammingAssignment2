## The first function below, makeCacheMatrix creates a special matrix by following the
## sequence below:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the inverse of the matrix
## 4) get the inversed matrix

makeCacheMatrix <- function(x = matrix()) {

  #initialize a variable for inversed matrix
  inv_mtrx <- NULL
  
  #set the value of the matrix and inverse matrix cached as NULL
  set <- function(y){
    x <<- y
    inv_mtrx <<- NULL    
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value for the inverse matrix
  set_inversematrix <- function(invrsM) inv_mtrx <<- invrsM
  
  #get the value of the inverse matrix
  get_inversematrix <- function() inv_mtrx
  
  #return all functions above in a list
  list( set = set, get = get,
        set_inversematrix = set_inversematrix,
        get_inversematrix = get_inversematrix
        )
    
}



## The function below calculates the inverse of the special matrix created with the above 
## functions. It will first check if the same inverse matrix been calculated (stored in 
## cache) before.If yes, it will not recalculates but will just get it from the cache. 
## Else, it will calculates the inverse matrix using "solve" function and store in cache.


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
   
  #get the calculated inversed matrix in cache
  inv_mtrx <- x$get_inversematrix()
  
  #check if cache has the inversed matrix, it yes then return it
  if (!is.null(inv_mtrx)){
      
    message("Inversed matrix calculated, getting cached data")
    return (inv_mtrx)
  }

  #Else get the matrix input for inverse calculation
  matrix_input <- x$get()
  
  #use the standard "solve" function to calculate matrix inversion
  inv_mtrx <- solve(matrix_input, ...)
  
  #set the calculated inverse matrix
  x$set_inversematrix(inv_mtrx)
  
  #return the inversed matrix
  inv_mtrx
}
