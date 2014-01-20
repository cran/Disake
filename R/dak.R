dak <-
function(x,t,h,ker)
# INPUTS:
#   "x" the target
#   "t" the single value where the function is computed
#   "h" the bandwidth.It is in (0,1]for the binomial kernel
#   "ker" the kernel function: "dirac" Dirac, "pois" Poisson, "bino" Binomial, "nebi" Negative Binomial
 
# OUTPUT:
# Returns the discrete associated kernel estimation at t.
	{if(ker=="dirac"){	
			 result<-t
       			 Logic1 <- (t==x) 
			 Logic0 <- (t!=x)
       			 result[Logic0]<-0
        		 result[Logic1]<- 1 
							
        		return(result)
			}
    



 else
	  
	if(ker=="bino"){	
			result <- t
       			 Logic0 <- (t <= x+1) # support Sx={0,1,...,x+1}
			Logic1 <- (x+1 < t)
       			 tval <- result[Logic0]
			result[Logic1]=0
        		result[Logic0]<- dbinom(tval,x+1,(x+h)/(x+1))  # Binomial discrete associated kernel 
											
        		return(result)
			}
	   else
		if(ker=="pois"){
			result <- dpois(t,x+h)	# Poisson discrete associated kernel 
			return(result)
		}
	   else
		if(ker=="nebi"){
		 result <- dnbinom(t, x+1,(x+1)/(2*x+1+h))  # Negative Binomial kernel 
		return(result)

		}
}

