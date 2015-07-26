## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The makeCacheMatrix function is used to store the inverse of a given matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) 
        {
            message("** Setting the source matrix **")
            x <<- y
            m <<- NULL
        }
        
        get <- function() 
        {
           message("** Getting the source matrix **")
           x
        }
        
        setsolve <- function(solve)
        {
            message("** Saving the inverse **")    
            m <<- solve
        }
        
        getsolve <- function()
        {
            message("** Getting the inverse **")
            m
        }
        
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)

}
## Write a short comment describing this function
## The cacheSolve function will return the inverse of matrix 'x'.
## The result of the the inverse will be returned in 'm'
## If there is an existing inverse for 'x' already stored in cache, it will return the value without calculating it


cacheSolve <- function(x, ...) 
  {
        
        ## Return a matrix that is the inverse of a given matrix 'x'
        m <- x$getsolve()
  
        if(!is.null(m)) 
        {
           message("** Stored inverse exists, getting cached data instead **")
            return(m)
        }
  
        message("** Stored inverse does not exist, calculating the inverse  **")
        data <- x$get()
        
        m <- solve(data, ...)
        x$setsolve(m)
        m
  }

##Example on how to run

##> source_matrix <- matrix(2:5, nrow = 2, ncol = 2)
##> source_matrix
##     [,1] [,2]
##[1,]    2    4
##[2,]    3    5
##> cached_matrix = makeCacheMatrix(source_matrix)
##> cached_matrix$get()
##** Getting the source matrix **
##     [,1] [,2]
##[1,]    2    4
##[2,]    3    5
##> cacheSolve(cached_matrix)
##** Getting the inverse **
##** Stored inverse does not exist, calculating the inverse  **
##** Getting the source matrix **
##** Saving the inverse **
##     [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1
##> cacheSolve(cached_matrix)
##** Getting the inverse **
##** Stored inverse exists, getting cached data instead **
##     [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1

##> source_matrix2 <- matrix(3:6, nrow = 2, ncol = 2)
##> source_matrix2
##     [,1] [,2]
##[1,]    3    5
##[2,]    4    6
##> cached_matrix2 = makeCacheMatrix(source_matrix2)
##> cached_matrix2$get()
##** Getting the source matrix **
##     [,1] [,2]
##[1,]    3    5
##[2,]    4    6
##> cacheSolve(cached_matrix2)
##** Getting the inverse **
##** Stored inverse does not exist, calculating the inverse  **
##** Getting the source matrix **
##** Saving the inverse **
##     [,1] [,2]
##[1,]   -3  2.5
##[2,]    2 -1.5
##> cacheSolve(cached_matrix2)
##** Getting the inverse **
##** Stored inverse exists, getting cached data instead **
##     [,1] [,2]
##[1,]   -3  2.5
##[2,]    2 -1.5
##> cacheSolve(cached_matrix)
##** Getting the inverse **
##** Stored inverse exists, getting cached data instead **
##     [,1] [,2]
##[1,] -2.5    2
##[2,]  1.5   -1