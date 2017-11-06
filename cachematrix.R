

##The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to

#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the matrix
#4.get the value of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
##set the value of the matrix
  set <- function(y) {
    x <<- y
    inverse_matrix <<- NULL
  }
## Get the value of the matrix
  get <- function() x
  
## Set the inverse of the matrix
  set_inverse <- function(inversematrix) inverse_matrix <<- inversematrix

## get the inverse of the matrix
  get_inverse <- function() inverse_matrix

# Put all the functions above into a list
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}



##The following function computes the inverse of the  "matrix" created with the above function. 

cacheSolve <- function(x, ...) {
#check to see if the mean has already been calculated 
  inverse_matrix <- x$get_inverse()

#If so, return the inverse from the cache and skip the computation.  

  
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  

  data <- x$get()

#Otherwise, check if the new matrix is invertible. If it is not invertible, print out an error message and stop the computation.

 if(det(data)==0)
 {message("The matrix is not inversible.") 

   
   }
  
#If not,invert the matrix and reset the value
 else
{
  inverse_matrix <- solve(data, ...)
  x$set_inverse(inverse_matrix)
   inverse_matrix
  
}
   
}



