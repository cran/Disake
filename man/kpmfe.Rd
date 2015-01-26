\encoding{UTF-8}
\name{kpmfe}
\alias{kpmfe}

\title{
Function for kernel estimation of the probability mass function
}
\description{
The function estimates the p.m.f. in a single value or in a
grid using discrete associated kernels. Three different  kernels are available: DiracDU, Binomial and Discrete Triangular.
}
\usage{
kpmfe(Vec, h, ker, a = 1, c = 2)
}

\arguments{
 

  \item{Vec}{
The data sample.
}
\item{h}{
The bandwidth or smoothing parameter.
}
 \item{ker}{
The associated kernel: "dirDU" DiracDU,"bino" Binomial, "triang" Discrete triangular.
}

  \item{a}{
The arm in Discrete Triangular kernel. The default value is 1.
}
  
  \item{c}{
The number of categories in DiracDU. The default value is 2.
}
}
\details{ 
The  kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined in the above sections.
We recall that in general, the sum of the estimated values on the support is not equal to 1. In practice, we calculate the  normalizing constant \eqn{C_n} before computing the estimated p.m.f. \eqn{\tilde{f}_n}{\tilde{f}_n}; see Kokonendji and Senga Kiessé (2011).

The bandwidth parameter in the function is obtained using the  cross-validation technique for the three kernels. For Binomial kernel, the local Bayesian approach is also implemented and is recommanded to select the bandwidth; see Zougab et al. (2012).

}
\value{
Returns a list containing:
  \item{n }{The number of observations.}
 \item{support}{The  support of the pmf fn.}
 \item{C_n }{The normalizing constant.}
 \item{ISE_0 }{The integrated square error.}
\item{f_0 }{ A vector of (x,f0(x)).}
\item{f_n }{A vector of (x,fn(x)).}
\item{f0 }{ The empirical p.m.f.}
\item{fn }{The estimated p.m.f. containing estimated values after normalization.}

 
}
\references{

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function.
\emph{Journal of Nonparametric Statistics}  \bold{19},  241 - 254.

Zougab, N., Adjabi, S. and Kokonendji, C.C. (2012). Binomial kernel and Bayes local bandwidth in discrete functions estimation. 
\emph{Journal of Nonparametric Statistics}  \bold{24}, 783 - 795.


}
\author{
W. E. Wansouwé, C. C. Kokonendji and D. T. Kolyang 
}


\examples{
## A sample data with n=60.
V<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,7,7,
7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,12,12,10,12,12,
13,14,15,16,16,17,0,12)


##The bandwidth can be the one obtained by cross validation.
h<-0.081
## We choose Binomial kernel.

est<-kpmfe(Vec=V,h,"bino")
##To obtain the normalizing constant:
est$C_n


}

