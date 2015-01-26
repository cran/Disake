\encoding{UTF-8}
\name{CVbw}
\alias{CVbw}

\title{
Cross-validation function for bandwidth selection
}
\description{
The function allows to calculate the optimal bandwidth using the cross-validation method. Three kernels are available: DiracDU, Binomial and Discrete Triangular. For Binomial kernel, the bandwidth is always less than 1; see Kokonendji and Senga Kiessé (2011), and also Kokonendji et al. (2007).

}
\usage{
CVbw(Vec, seq_bws = NULL, ker, a = 1, c = 2)
}

\arguments{
  \item{Vec}{
The sample data.
}
  \item{seq_bws}{
The sequence of bandwidths where the cross-validation is computed. If NULL, the procedure defines a sequence of bandwidths.

}
  \item{ker}{
The associated kernel: "dirDU" DiracDU, "bino" Binomial, "triang" Discrete Triangular.
}
  \item{a}{
The  arm in Discrete Triangular kernel. The default value is 1.
}
 
  \item{c}{
The number of categories in DiracDU kernel.  The default value is 2.
}
}
\details{
The selection of the bandwidth parameter is crucial. If the bandwidth is small, we will obtain an
undersmoothed estimator, with high variability. On the contrary, if the value  is big, the
resulting estimator will be very smooth and farther from the function that we are trying to estimate.
The cross-validation function defined in the above sections is used to compute the optimal bandwidth for the three kernels. 
            
}
\value{
Returns a list containing:
  \item{hcv }{The optimal bandwidth obtained by cross-validation.}
  \item{CV }{The values of the cross-validation function in the sequence of bandwidths.}
  \item{seq_bws}{The sequence of bandwidths used.}
}
\references{

Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
 \emph{Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function,
 \emph{Journal of Nonparametric Statistics}  \bold{19},  241 - 254.

}
\author{
W. E. Wansouwé,  C. C. Kokonendji and D. T. Kolyang 
}
\examples{
## Data can be simulated data or real data
## We use real data 
## and then compute the cross validation. 
Vec<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,7,7,
7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,12,12,10,12,12,
13,14,15,16,16,17,0,12)
\dontrun{
CV<-CVbw(Vec,NULL,"bino")
CV$hcv
}
##The cross validation function can be also ploted.
\dontrun{
plot(CV$seq_bws,CV$CV, type="l")
}

}

