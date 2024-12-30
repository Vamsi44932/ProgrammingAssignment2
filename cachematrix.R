## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:

#The main goal of this function is to define the speacial matrix

makeCacheMatrix <- function(x = matrix()) {
  
  # defined InverseMatrix as Null
  inverseMatrix <- NULL 
  
  
  #Data manipulation functions
  #Set function assigns the given matrix to the already created matrix in special object
  set <- function(y) {
    
    #<<- : this is used to update the variable of the parent environment
    x <<- y
    
    # As set function is updating the data and current inverse matrix is not valid anymore
    inverseMatrix <<- NULL
  }
  
  #This function provides the matrix
  get <- function() {
   #'x' is the matrix which will be returning from this function 
    x
  }
  
  #set_inverse function assigns the inverse matrix to the inverse variable
  set_inverse <- function(inverse) {
    
   ##As we are accessing the variable from parent eniviroment again assigned with this '<<-'
   inverseMatrix <<- inverse
   }
  
  #get_inverse pulls out the inversed matrix from the object
  get_inverse <- function() inverseMatrix
  
  #This helps in returning the data
  list(set = set, get = get,
       set_inverse = set_inverse,
       get_inverse = get_inverse)
  
}


## Write a short comment describing this function
#cacheSolve checks whether the given matrix is inversed or not. Based on the status it calculates the inverse of a matrix
cacheSolve <- function(x, ...) {
  
  #pulluing out the inverse matrix from special object and storing in the local variable 'Inv'
  Inv <- x$get_inverse()
  
  #checks if the matrix is Null or not. If it is not Null then it should return the Inv
  if(!is.null(Inv)) {
    
    #This throws an information saying Inverse is getting from the cached
      message("getting cached Inverse")
    
    #This returns the Inv
      return (Inv) 
  }
  
  #if the above Inv is null then we need to get the matrix and need to calculate the Inv of the given matrix
  Data <-x$get()
  
  #This calculated the Inversion of the matrix
  Inv <- solve(Data)
  
  #This sets the above Inversed matrix to the object
  x$set_inverse(Inv)
  
  #This returns the above Inversed matrix
  Inv
}


#Taken the example square matrix
m <- matrix(1:4, nrow = 2, ncol = 2)

#special object is created using the makeCacheMatrix 
my_matrix <- makeCacheMatrix(m)

#Calculate the inverse of the matrix
Inverse <- cacheSolve(my_matrix)
Inverse

#As we are callig the cacheSolve function again it is provides the cached inverse information.
Inverse <- cacheSolve(my_matrix)
Inverse




