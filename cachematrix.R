## Put comments here that give an overall description of what your
## functions do

## These functions were written for completing the peer-graded assignment in Coursera: R Programming from John Hopkins University
## github-Dembappe

## Write a short comment describing this function

## makeCacheMatrix: it creates a special "matrix" object that can cache its inverse.

## 3 General Steps:
## 1. Initialize the 2 objects: x(empty matrix) & inv(NULL)
## 2. Define the functions within object: getters(retrieve data) and setters(mutate data values)
## 3. Assign the functions created as an element within a list() and return to the parent environment

## Note:
## The <<- operator is to assign the value on the right side of the operator to an object in the parent 
## environment by the object on the left side of the operator

## These functions work only for invertible square matrices

makeCacheMatrix <- function(x = matrix()) {  #Step 1a: initialize as a function argument as 'matrix'.
  inv <- NULL                                #Step 1b: initialize 'inv' as NULL to hold the value of the inversed matrix.
  set <- function(y) {                       #Step 2a: set object other than 'x', in this case 'y' as an input. So, it 
    x <<- y                                  #will assign the input argument to the 'x' object(matrix) in the parent environment.
    inv <<- NULL                             #Step 2b: this code clears any value that had been cached by 'cacheSolve' by 
  }                                          #resetting 'inv' to NULL. Whenever 'x' is reset, the value of 'inv' cached in the memory
                                             #is cleared, forcing 'cacheSolve' to recalculate the inverse rather than retrieving wrong value.
  
  get <- function() x                       #Step 2c: define the getter for matrix 'x' function to return the value of matrix argument.
  
  setinverse <- function(inverse) inv <<- inverse     #Step 2d: define the setter for the inverse 'x' and assign the input argument('inverse')
                                                      #to the value of 'inv' in the parent environment.
  getinverse <- function() inv                        #Step 2e: define the getter for the 'inv' function to return the its value.
  
  #Step 3: assign each of the 4 functions as an element within a list(), and return it to the parent environment
  list(set = set,                           #gives the name 'set' to the set() function
       get = get,                           #gives the name 'get' to the get() function
       setinverse = setinverse,             #gives the name 'setinverse' to the setinverse() function
       getinverse = getinverse)             #gives the name 'getinverse' to the getinverse() function
  
  #naming the list elements so that we can use the '$' operator to access the functions
}                                           
                     

## Write a short comment describing this function

## cacheSolve: it computes the inverse matrix returned by the makeCacheMatrix. If the inverse has already
## been calculated and the matrix has not changed, it will retrieve the inverse from the cache.

## 4 General Steps:
## 1. Initialize the single argument with an ellipsis to allow the caller to pass additional arguments if needed.
## 2. Retrieve the inverse matrix of 'x' 
## 3. Check to see whether the result of 'x' is NULL. Since the function in makeCacheMatrix sets the cached inverse
## to be NULL whenever new values are assigned. If the value is not NULL, the cached inverse is valid and can be returned
## to the parent environment.

## 4. If the result of the checking is false, 'cacheSolve' gets the input matrix and calculates the inverse using solve(),
## and uses the setinverse() function in the input object. Finally, it will return the value of the inverse to the parent
## environment by printing the inverse ('inv').

cacheSolve <- function(x, ...) {                        #Step 1: Initialize the argument 'x' with ellipsis
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                                 #Step 2: Retrieve the inverse matrix of 'x' by assigning 'x$getinverse' to 'inv' variable
  if(!is.null(inv)){                                    #Step 3: Check if it's TRUE that it is not NULL, it will return the cached inverse matrix
    message("getting cached data")
    return(inv)
  }
  
  #Step 4: if the result of !is.null is FALSE, then calculate the inverse using solve() and print the inv
  MatrixData <- x$get()             #Step 4a: get the matrix by using 'x$get' function and assign it into the variable
  inv <- solve(MatrixData, ...)     #Step 4b: inverse the matrix by using the solve() function along with the additional arguments
  x$setinverse(inv)                 #Step 4c: set the value of 'inv' as set inverse
  inv                               #Step 4d: return the inverse value
}
