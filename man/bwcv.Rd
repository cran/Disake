\name{bwcv}
\alias{bwcv}

\title{Cross validation method for bandwidth selection
}
\description{
The selection of the bandwidth parameter is crucial. If the bandwidth is small, we will obtain an
undersmoothed estimator, with high variability. On the contrary, if the value  is big, the
resulting estimator will be very smooth and farther from the function that we are trying to estimate. The function allows to compute the optimal bandwidth through the cross validation without bias (or leave one-out technique). Three standard kernels estimators are available: Binomial, Poisson and negative binomial. For the binomial kernel,the bandwith  is less than 1.
}
\usage{
bwcv(Vec, seq_bws = NULL, ker)
}

\arguments{
  \item{Vec}{
The sample data.
}
  \item{seq_bws}{
The sequences of bandwidths where the Cross validation is computed. If NULL, the procedure defines a sequence of 1000 points, from 0.001 to 1.

}
  \item{ker}{
The kernel function:"pois" Poisson, "bino" Binomial, "nebi" Negative Binomial
}
}
\details{
The cross-validation without bias is computed. 
              

}
\value{

  Returns a list containing:
  \item{hcv }{the optimal bandwidth obtained by cross validation.}
  \item{CV }{The values of the cross-validation function in the bandwidths sequence.}
  \item{seq_bws}{The sequence of bandwidths used.}
}
\references{

Kokonendji, C.C. and Senga Kiesse, T. (2011). Discrete associated kernel method and extensions.
Statistical Methodology 8,  497 - 516.

Kokonendji, C.C. and Senga Kiesse, T. (2011). Discrete associated kernel method and extensions.
Statistical Methodology 8,  497 - 516.

}
\author{
W. E. Wansouwe,  C. C. Kokonendji and D.T. Kolyang 
}

\examples{
## Data can be simulated data or real data
## We simulate data from poisson with mean 10 
## and then compute the cross validation. 
Vec<-rpois(50,10)
bws<-seq(0,1, by=0.001)
\dontrun{
CV<-bwcv(Vec,bws,"pois")
CV$hcv
}
##The cross validation function can be also ploted.
\dontrun{
plot(CV$seq_bws,CV$CV, type="l")
}
function (Vec, seq_bws = NULL, ker) 
{
    if (is.null(seq_bws)) 
        seq_bws = seq(0.001, 1, length.out = 1000)
    CV1 = function(Vec, h, ker) {
        result = rep(0, length(h))
        x = 0:(max(Vec) + 3)
        n <- length(x)
        Dak <- Vectorize(dak, vectorize.args = c("x", "t"))
        for (k in 1:length(h)) {
            m = matrix(0, n, length(Vec))
            m <- outer(x, Vec, Dak, h[k], ker)
            res <- apply(m, 1, mean)
            result[k] = sum(res^2)
        }
        return(result)
    }
    CV2 = function(Vec, h, ker) {
        result = rep(0, length(h))
        n <- length(Vec)
        Dak <- Vectorize(dak, vectorize.args = c("x", "t"))
        for (k in 1:length(h)) {
            m = matrix(0, n, n)
            m <- outer(Vec, Vec, Dak, h[k], ker)
            diag(m) <- 0
            res <- apply(m, 1, sum)
            result[k] = (2/((n - 1) * n)) * sum(res)
        }
        return(result)
    }
    CV = CV1(Vec, seq_bws, ker) - CV2(Vec, seq_bws, ker)
    index <- which.min(CV)
    hcv <- seq_bws[index]
    return(list(hcv = hcv, CV = CV, seq_bws = seq_bws))
  }
}

