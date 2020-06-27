## This function returns another 4 functions(closures) namely set, get, setinverse and getinverse.
## set sets the data to the variables x and inv, whenever a fresh data is set, it assigns it to x and make the 'inv' variable NULL as it has tobe calculated fresh.

makeCacheMatrix <- function(x = matrix()) {
	 inv <- NULL
        set <- function(y){
               x <<-y
               inv <-NULL
                
                
        }
        get <- function()x
        setinverse <- function(inverse){
                inv <<- inverse
                
        }
        getinverse <- function(){
                inv
        }
        
        list(set=set, get=get, setinverse = setinverse, getinverse= getinverse)
        
        
        
        
	
	

}


## This function calcualtes the inverse of the matrix supplied using solve function and assigs the value to the setinverse function of the makecachematrix. Together along
## with the function above, if the matrix is new then it calculates the inverse first time; second time onwards it extracts it from the cache.

cacheSolve <- function(x, ...) {
	
	inv <- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
                
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinverse(inv)
        inv
        
        ## Return a matrix that is the inverse of 'x'
}
