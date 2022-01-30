## Put comments here that give an overall description of what your
## functions do

## These functions were written for completing the peer-graded assignment in Coursera: R Programming from John Hopkins University
## github-Dembappe

## Write a short comment describing this function

## makeCacheMatrix: it creates a special "matrix" object that can cache its inverse.

## 3 General Steps:
## 1. Initialize the 2 objects: x(empty matrix) & invMatrix(NULL)
## 2. Define the functions within object: getters(retrieve data) and setters(mutate data values)
## 3. Assign the functions created as an element within a list() and return to the parent environment

## These functions work only for invertible square matrices

makeCacheMatrix <- function(x = matrix()) {  #Step 1a: initialize as a function argument as 'matrix'.
  invMatrix <- NULL                          #Step 1b: initialize 'invMatrix' as NULL to hold the value of the inversed matrix.
  setMatrix <- function(y) {                 #Step 2a: set object 'y' as an input argument which assigns to the
    x <<- y                                  #'x' object(matrix) in the parent environment.
    invMatrix <<- NULL                       #Step 2b: this code resets the value that had been cached by 'cacheSolve' by 
  }                                          #resetting 'invMatrix' to NULL. 
                                             
  
  getMatrix <- function() x                  #Step 2c: define the getter for matrix 'x' function to return the value of matrix argument.
  
  setInverse <- function(inverse) invMatrix <<- inverse     #Step 2d: define the setter for the inverse 'x' and assign the input argument('inverse')
                                                            #to the value of 'invMatrix' in the parent environment.
  getInverse <- function() invMatrix                        #Step 2e: define the getter for the 'invMatrix' function to return the its value.
  
  #Step 3: assign each of the 4 functions as an element with a name and within a list(), and return it to the parent environment
  list(setMatrix = setMatrix,                           
       getMatrix = getMatrix,                           
       setInverse = setInverse,             
       getInverse = getInverse)             
  
  #naming the list elements so that we can use the '$' operator to access the functions
}                                           

## Write a short comment describing this function

## cacheSolve: it computes the inverse matrix returned by the makeCacheMatrix. If the inverse has already
## been calculated and the matrix has not changed, it will retrieve the inverse from the cache.

## 4 General Steps:
## 1. Initialize the single argument with an ellipsis to allow the caller to pass additional arguments if needed.
## 2. Retrieve the inverse matrix of 'x' 
## 3. Check to see whether the result of 'x' is NULL. 
## 4. If the result of the checking is false, 'cacheSolve' gets the input matrix and calculates the inverse using solve(),
## and uses the setInverse() function in the input object. 

cacheSolve <- function(x, ...) {                        #Step 1: Initialize the argument 'x' with ellipsis
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()                           #Step 2: Retrieve the inverse matrix of 'x' by assigning 'x$getInverse' to 'invMatrix' variable
  if(!is.null(invMatrix)){                              #Step 3: Check if it's TRUE that it is not NULL, it will return the cached inverse matrix
    message("getting cached data")
    return(invMatrix)
  }
  
  #Step 4: if the result of !is.null is FALSE, then calculate the inverse using solve() and print the inv
  NewMatrix <- x$getMatrix()             #Step 4a: get the matrix by using 'x$getMatrix' function and assign it into the variable
  invMatrix <- solve(NewMatrix, ...)     #Step 4b: inverse the matrix by using the solve() function along with the additional arguments
  x$setInverse(invMatrix)                #Step 4c: set the value of 'invMatrix' as setInverse
  invMatrix                              #Step 4d: return the inverse value
}
