\encoding{UTF-8}
\name{sumry}
\alias{sumry}

\title{
A brief summary of the results
}
\description{
For a given data sample, the function gives a summary of the results of five computed values: p.m.f. \eqn{f_0}{f_0}, estimated values \eqn{ \tilde{f}_n}{\tilde{f}_n}, \eqn{ISE_0}{ISE_0}, normalizing constant  \eqn{C_n}{C_n} and smoothing parameter  \eqn{h}{h}; see Kokonendji and Senga Kiessé (2011), and also Zougab et al. (2012).
}
\usage{
sumry(Vec,type_bw, ker = "bino", h = NULL,  a = 1, c = 2)
}

\arguments{
  \item{Vec}{
The data sample.
}
 
  \item{ker}{
The kernel function: "dirDU" DiracDU,"bino" Binomial, "triang" Discrete Triangular.
}
  \item{h}{
The bandwidth or smoothing parameter. If NULL,it is calculated depending on the value of type_bw.
}
  \item{type_bw}{
The technique of bandwidth selection: Bayesian or cross-validation.
}
  \item{a}{
The arm in the Discrete Triangular kernel.
}
  \item{c}{
The number of categories in DiracDU.
}
}

\value{

Returns a list containing:

 \item{n }{The number of observations.}
 \item{support_of_f_n}{The  support of the p.m.f. fn.}
 \item{f_0 }{The couples (x,f0(x)).}
 \item{f_n }{The couples (x,fn(x)).}
 \item{ISE_0 }{The integrated square error.}
 \item{C_n }{The normalizing constant.}
 \item{h }{The bandwith parameter used to compute fn.}

}
\references{


Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.


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

sumry(V,type_bw="CV")

}
