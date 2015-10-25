makeCacheMatrix <- function(M = matrix()){
###############################################################################
# Creates and Returns Matrix List Object which stores its own Inverse and 
# contains Functions to Access that Data.
# 
# Args:
#   M - The Data of the Matrix itself
# 
# Returns:
#   A List containing Functions for Getting and Setting both the Core Matrix
#   Data (in "M") and the Value of the Inverse (as if it were an object)
###############################################################################
    
    i <- NULL;##The Inverse of this Matrix
    
    #Resets the Matrix Data:
    setData <- function(M.new){
        M <<- M.new;
        i <<- NULL;##Data is New; Inverse is now Unknown
    }
    
    #Returns the Stored Matrix Data:
    getData <- function() M;
    
    
    #Caches the Given Matrix Inverse:
    setInverse <- function(inv) i <<- inv;
    
    #Returns the Inverse of the Matrix:
    getInverse <- function() i;
    
    #Store Functions to a List and Return:
    list(setData=setData, getData=getData,
         setInverse=setInverse, getInverse=getInverse);
}



cacheSolve <- function(mo){
###############################################################################
# Returns the Inverse for the Given Matrix List Object (as created in 
# "makeCacheMatrix"). If the Matrix Object already contains a value for the
# Inverse, that value is returned; however, if the Matrix Object's Inverse value
# is "NULL", then the Inverse will be Calculated for the Matrix Object's Data,
# Set as the Matrix Object's Inverse, and Returned.
# 
# Args:
#   mo - Matrix Object to Compute the Inverse of. MUST be invertible.
# 
# Returns:
#   The Inverse of the Data of the Given Matrix Object
###############################################################################
    
    i <- mo$getInverse();##Inverse of the Matrix or "NULL" if not Computed
    
    #Check whether Inverse has Already been Solved and Cached:
    if(!is.null(i)){
        return(i);#Return Cached Inverse
    }
    
    data <- mo$getData();##Core Matrix Data of Given Object
    
    #Compute Inverse:
    i <- solve(mo);
    #Cache Result:
    mo$setInverse(i);
    #Return Calculated Result:
    i;
}