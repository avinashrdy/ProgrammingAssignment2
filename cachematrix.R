## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# All the code is taken as reference from the given example 
        m <- NULL # initializing m
        # set: acts as a constructor to initialize the values every time function call is made
        set <- function(y){
                   x <<- y
                   m <<- NULL
              }
        getmatrix <- function() x #returns the matrix
        # setinverse: Assigns value of the solved inverse of the matrix which is calculate in 'cacheSolve' function
        # As the inverse is calculated in different fuction, the scope of the variable changes. Hence,
        # '<<-' operator is used to assign the value of object that is created in different environment
        setinverse <- function(solve) m <<- solve  
        # getinverse: Returns the value of m
        getinverse <- function()  m
        # Using the list, it is returing all the values so that these can be accessible to the 'cacheSolve' function
        list(set=set, get=getmatrix, 
             setinverse=setinverse,
             getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
             # All the explaination assumes variable 'x' in this function contains the result of 'makeCacheMatrix' function
        # m: This contains the value returned by the 'getinvrerse' object which inturn contains the value of 
        # the inverse of the matrix if it is calculated already else it contains 'NULL'
        m <- x$getinverse() 
        # The 'if' function below checks if the inverse of the matrix is available. If so, then function returns that value
        # If not then the code inside if is skipped
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # getmatrix: This is used to get the matrix which we passed to 'makeCacheMatrix' function
        getmatrix <- x$get()
        # m: This calculates inverse of the matrix
        m <- solve(getmatrix, ...)
        x$setinverse(m) # assigning inverse of  matrix to the variable in 'makeCacheMatrix' function 
        m
}
