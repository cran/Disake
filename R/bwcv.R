bwcv <-
function(Vec,seq_bws=NULL,ker){
# INPUTS:
#   "Vec" 	sample of data 
#   "seq_bws"   sequence of bandwidths where 
#		the CV function will be evaluated (and minimized)
#   "ker"      kernel function: "bino" = Binomial,"pois"=Poisson, 
#                                  "nebi" = Negative Binomial         
#    
# OUTPUT:Returns a list containing:
#   "hcv" :    the CV bandwidth 
#   "CV"      : the cross validation values 
#   "seq_bws": sequence of the bandwiths used
if(is.null(seq_bws)) 
	seq_bws=seq(0.001,1, length.out=1000) 
CV1=function(Vec,h,ker){ # to compute the first term #
        result=rep(0,length(h))
	x=0:(max(Vec)+3)
        n <- length(x)
        Dak<-Vectorize(dak,vectorize.args=c('x','t')) 
      for(k in 1:length(h)){
         m=matrix(0,n,length(Vec))
           m <-outer(x,Vec,Dak,h[k],ker)
              res<-apply(m,1,mean)
          result[k]=sum(res^2)
    }
  return(result)
  }
CV2=function(Vec,h,ker){ #to  compute the second term #
      result=rep(0,length(h))
      n <- length(Vec)
       Dak<-Vectorize(dak,vectorize.args=c('x','t'))
      for(k in 1:length(h)){
         m=matrix(0,n,n)
           m <-outer(Vec,Vec,Dak,h[k],ker)          
	      diag(m)<-0
              res<-apply(m,1,sum)
          result[k]=(2/((n-1)*n))*sum(res)
    }
  return(result)
  }
	#to  compute the optimal bandwidth#
   	CV= CV1(Vec,seq_bws,ker)-CV2(Vec,seq_bws,ker)    
	index<-which.min(CV)
	hcv<-seq_bws[index]
	return(list(hcv=hcv,CV=CV,seq_bws=seq_bws))
}

