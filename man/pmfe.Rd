\name{pmfe}
\alias{pmfe}

\title{Function for kernel estimation of the probability mass function.
}

\description{Computes the value of the kernel estimator of the probability mass function in a single value or in a
grid.Three different standard discrete associated kernels are available: Poison, Binomial and Negative binomial.

}
\usage{pmfe(ker, x, h, Vec)
}

\arguments{\item{ker}{
The kernel function: "pois" Poisson, "bino" Binomial, "nebi" Negative Binomial. The case of Dirac kernel is obvious.
}
  \item{x}{
A single value or a grid where the kernel estimation is computed
}
  \item{h}{
The bandwidth parameter.
}
  \item{Vec}{
The data sample
}
}
\details{The sum the estimated values on the support is closed to 1. So, we first, need to normalize  before using the  estimator.
}
\value{
   Returns a list containing:
 
 \item{C }{The normalizing constant}
 \item{Estimated_values }{A vector of estimated values of the function}
 
}
\references{


Abdous, B. and Kokonendji, C.C. (2009).  Consistency and asymptotic normality for discrete associated kernel estimator, African
Diaspora Journal of Mathematics 8,  63 - 70.

Aitchison, J. and Aitken, C.G.G. (1976).  Multivariate binary discrimination by the kernel method, Biometrika 63, 413-420.

Kokonendji, C.C. and Senga Kiesse, T. (2011). Discrete associated kernel method and extensions.
Statistical Methodology 8,  497 - 516.

Kokonendji, C.C., Senga Kiesse, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function.
Journal of Nonparametric Statistics 19,  241 - 254.


C.C. Kokonendji, S.S. Zocchi, Extensions of discrete triangular distributions and boundary bias in kernel estimation for
discrete functions, Statistics & Probability Letters 80 (2010) 1655 - 1662.

Zougab, N., Adjabi, S. and Kokonendji, C.C. (2012). Binomial kernel and Bayes local bandwidth in discrete functions estimation. 
Journal of Nonparametric Statistics 24, 783 - 795.



}
\author{
W. E. Wansouwe , C. C. Kokonendji and D.T. Kolyang 
}


\examples{

## 60 sample data obtained from a mixture of 
## 2 Poissons distributions with means 0.6 and 9:f=0.3*P(0.6)+0.7*P(9)
## The sample size is not large. The naive estimator is not appropriate.

V<-c(0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,2,4,4,4,5,6,6,6,6,6,7,7,7,7,
7,8,8,8,8,9,9,9,9,10,10,10,10,10,10,10,10,11,12,12,12,12,12,12,
13,14,15,16,16,17)

## When using the binomial kernel, we can take x=0:max(Vec). 
## For other kernels, one needs to allow computing up to max(Vec) 
x<-0:max(V)
##The bandwidth can be the one obtained by cross validation.
h<-0.081
##We choose the binomial kernel.

est<-pmfe("bino",x,h,Vec=V)
##To obtain the normallizing constant:
est$C 
## we can also plot the graphics and compare the naive estimator
N<-c(11,5,1,0,3,1,5,5,4,4,8,1,6,1,1,1,2,1) 
f0<-c(N/60,rep(0,length(x)-length(N)))
f=0.3*dpois(x,0.6)+0.7*dpois(x,9)
plot(x-0.2,f,type="h", ylim=c(0,0.2),ylab="Estimation",
main="Discrete smoothing",xlab="x",lwd=1,lty=2,cex.axis=0.70,cex.lab=0.70,cex.main=0.70)
lines(x,f0,type="h",lwd=1,col="black")
lines(x+0.2,est$Estimated_values,type="h",lwd=1,col="blue")

function (ker, x, h, Vec) 
{
    n <- length(x)
    if (is.null(x)) 
        x = 0:(max(Vec))
    m = matrix(0, n, length(Vec))
    for (i in 1:n) {
        m[i, ] = dak(x[i], Vec, h, ker)
    }
    res <- apply(m, 1, mean)
    result <- res/sum(res)
    return(list(C = sum(res), Estimated_values = result))
  }
}

