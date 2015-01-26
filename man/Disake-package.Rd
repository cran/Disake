\encoding{UTF-8}
\name{Disake-package}
\alias{Disake-package}
\alias{Disake}
\docType{package}
\title{
Discrete associated kernel estimators
}
\description{
 
Discrete smoothing of probability mass function (p.m.f.) is performed using three discrete associated kernels: DiracDU, Binomial and Discrete Triangular. Two automatic bandwidth selection procedures are implemented: the cross-validation method for the three kernels and the local Bayesian approach for Binomial kernel. Note that DiracDU is used for categorical data, Binomial kernel is appropriate  for count data with small or moderate sample sizes, and  Discrete Triangular kernel is recommanded for count data with large sample sizes.
}
\details{
\describe{
\item{The estimated p.m.f.:}{ The  kernel estimator \eqn{\widehat{f}_n}{\widehat{f}_n} of  \eqn{f}{f} is defined as

\deqn{\widehat{f}_n(x) = \frac{1}{n}\sum_{i=1}^{n}{K_{x,h}(X_i)},} where \eqn{K_{x,h}} is one of the kernels defined below.
 In practice, we first calculate the  normalizing constant

\deqn{{C}_n =   \sum_{x\in N}{\widehat{f}_n(x)},} which is not generaly equal to 1. This constant \eqn{{C}_n} is 1 only for Dirac and DiracDU. The estimated p.m.f. is then \eqn{\tilde{f}_n=\widehat{f}_n/C_n}{\tilde{f}_n=\widehat{f}_n/C_n}. 

The integrated squared error (ISE) defined by \deqn{{ISE}_0 =   \sum_{x\in N}{{\{\tilde{f}_n(x)} - f_0(x)\}^2}} is the criteria used to measure the smoothness of the kernel estimator \eqn{\tilde{f}_n}{\tilde{f}_n} with the empirical p.m.f. \eqn{f_0}{f_0}. 
 See Kokonendji and Senga Kiessé (2011).

   }

}

  	Given a data sample, the \pkg{Disake} package allows to compute the p.m.f. using one of the three kernel functions: DiracDU, Binomial and Discrete Triangular. The bandwidth parameter is calculated using the cross-validation technique \code{\link{CVbw}}. When the kernel function is Binomial, the bandwidth parameter is also computed using the local Bayesian procedure \code{\link{Baysbw}}. The kernel functions \code{\link{kf}} are defined below.


	\describe{

		    
	        \item{Binomial kernel :}{ Let \eqn{x\in  N:= \{0, 1, \ldots \}} and \eqn{{S}_x = \{0, 1, \ldots, x + 1\}}. The Binomial kernel is defined on the support \eqn{{S}_x} by
			 

\deqn{B_{x,h}(y) = \frac {(x+1)!} {y!(x+1-y)!}\left(\frac{x+h}{x+1}\right)^y\left(\frac{1-h}{x+1}\right)^{(x+1-y)}1_{S_{x}}(y),}


where \eqn{ h\in(0, 1]} and \eqn{1_A}{1[A]} denotes the indicator function of A. Note that \eqn{B_{x,h}}{B_[x,h]} is the p.m.f. of the Binomial distribution with its number of trials \eqn{ x+1} and its success probability \eqn{(x+h)/(x+1)}. See Kokonendji and Senga Kiessé (2011).

    }

  


 \item{DiracDU kernel :}{ For fixed number of categories \eqn{c\in \{2,3,...\} }, we define  \eqn{{S}_{c} = \{0, 1, \ldots, c-1\}}. The DiracDU kernel is defined on \eqn{{S}_{c}} by 
      \deqn{DU_{x,h;c}(y) = (1 - h)1_{\{x\}}(y)+\frac {h} {c-1}1_{S_{c}\setminus\{x\}}(y),}
    
 where \eqn{x\in {S}_{c} } and  \eqn{ h\in(0, 1]}. See Kokonendji and Senga Kiessé (2011), and also Aitchison and Aitken (1976) for multivariate case.
   }




\item{Discrete Triangular kernel:}{ For fixed arm \eqn{a\in  N}, we define \eqn{{S}_{x,a} = \{x-a,\ldots, x, \ldots, x + a\}}.  The Discrete Triangular kernel is defined on  \eqn{{S}_{x,a}} by
 \deqn{DT_{x,h;a}(y) = \frac {(a+1)^h - |y-x|^h} {P(a,h)}1_{S_{x,a}}(y),}
			 where \eqn{x\in  N},  \eqn{h>0} and \eqn{P(a,h)=(2a+1)(a+1)^h - 2(1+2^h+ \cdots +a^h)} is the normalizing constant. For \eqn{a=0}, the Discrete Triangular kernel \eqn{DT_{x,h;0}}{DT_[x,h;0]} corresponds to the Dirac kernel on \eqn{x}; see Kokonendji et al. (2007), and also Kokonendji and Zocchi (2010) for an asymmetric version of Discrete Triangular. 

    }


\item{The bandwidth selection:}{ Two functions are implemented to select the bandwidth: cross-validation and local Bayesian procedure. The cross-validation technique  \code{\link{CVbw}} is used for DiracDU, Binomial and Discrete Triangular kernels; see Kokonendji and Senga Kiessé (2011). The local Bayesian procedure  \code{\link{Baysbw}} is implemented to select the bandwidth for Binomial kernel; see Zougab et al. (2012).

   }   

  }


 }
\author{
W. E. Wansouwé , C. C. Kokonendji and D. T. Kolyang 

Maintainer: W. E. Wansouwé <ericwansouwe@gmail.com>
}
\references{


Aitchison, J. and Aitken, C.G.G. (1976). Multivariate binary discrimination by the kernel method, \emph{Biometrika}  \bold{63}, 413 - 420.

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
\emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function,
\emph{Journal of Nonparametric Statistics}  \bold{19},  241 - 254.

Kokonendji, C.C. and Zocchi, S.S. (2010). Extensions of discrete triangular distribution and boundary bias in kernel estimation for discrete functions,  \emph{Statistics and Probability Letters}  \bold{80}, 1655 - 1662.

 Zougab, N., Adjabi, S. and Kokonendji, C.C. (2012). Binomial kernel and Bayes local bandwidth in discrete functions estimation, \emph{ Journal of Nonparametric Statistics }  \bold{24}, 783 - 795.
}

\keyword{ package }

