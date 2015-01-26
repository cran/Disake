\encoding{UTF-8}
\name{Baysbw}
\alias{Baysbw}

\title{
Local Bayesian function for bandwidth selection
}
\description{The function computes the optimal bandwidth using the local Bayesian approach.
}
\usage{
Baysbw(Vec)
}

\arguments{
  \item{Vec}{
The data sample.
}
}
\details{
The Bayesian approach for variable bandwidth selection using Binomial
kernel has been introduced by Zougab et al. (2012).
The authors studied three Bayesian procedures to select the optimal bandwidth: 
the global, the local and the adaptive methods and they  showed that the local procedure
is better than the two others; see Zougab et al. (2013).
 
 

}
\value{
Returns the optimal bandwidth computed through local Bayesian procedure.
}
\references{

Zougab, N., Adjabi, S. and Kokonendji, C.C. (2012). Binomial kernel and Bayes local bandwidth in discrete functions estimation,  \emph{Journal of Nonparametric Statistics}  \bold{24}, 783 - 795.

Zougab, N., Adjabi, S. and Kokonendji, C.C. (2013). A Bayesian approach to bandwidth selection in univariate associate kernel estimation,  \emph{Journal of Statistical Theory and Practice}  \bold{7}, 8 - 23.

}
\author{
W. E. Wansouwé,  C. C. Kokonendji and D. T. Kolyang 
}

\examples{
## Data can be simulated data or real data
## and bandwidth is computed through the local Bayesian procedure.
Vec<-c(10,0,1,0,4,0,6,0,0,0,1,1,1,2,4,4,5,6,6,6,6,7,1,7,0,7,7,
7,8,0,8,12,8,8,9,9,0,9,9,10,10,10,10,0,10,10,11,12,12,10,12,12,
13,14,15,16,16,17,0,12)
 
h_Bays<-Baysbw(Vec)
}

