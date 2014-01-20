pmfe <-
function(ker,x,h,Vec){
# INPUTS:
#   "ker" kernel function: "bino" Binomial,	"pois" Poisson, 
#                                  "nebi" Negative Binomial         
#   "x" single value or grid where the kernel estimation is computed
#   "h" bandwidth
#   "Vec" sample of data 
# OUTPUT:Returns a list containing:
#    "C" normalisation constant
#    "Estimated_values" vector containing the estimated function
# 		        after normalization
    n <- length(x)
    if(is.null(x)) 
    x=0:(max(Vec))
    m=matrix(0,n,length(Vec))
    for(i in 1:n){
         m[i,]= dak(x[i],Vec,h,ker)
	        }
    res<-apply(m,1,mean)

    result<-res/sum(res)
   return(list(C=sum(res),Estimated_values=result))
  }

