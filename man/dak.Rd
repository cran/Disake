\name{dak}
\alias{dak}

\title{Discrete associated kernel function
}
\description{
Computes the value of the discrete associate kernel. In the discrete estimation, the target and the bandwidth are intrinsicaly linked to the discrete kernel. Dirac kernel and three standard discrete kernels are performed: Binomial, Poisson, Negative binomial kernels. 

}
\usage{
dak(x, t, h, ker)
}

\arguments{
  \item{x}{ The target.

}
  \item{t}{
A single value or the grid  where the discrete associated kernel function is computed.
}
  \item{h}{
The bandwidth or smoothing parameter.
}
  \item{ker}{
The kernel function: "dirac" Dirac, "pois" Poisson, "bino" Binomial, "nebi" Negative Binomial.
}
}
\details{
The bandwidth and the targuet are instrinsicaly linked to the kernel. Parameters of r.v. of each standard discrete associate kernel  are: Dirac:D(x),
  Binomial: B(x+1,(x+h)/(x+1)),  Poisson: P(x+h) and Negative Binomial: NB(x+1,(x+1)/(2x+1+h))

}
\value{
Returns the value of the discrete associated kernel estimation at t according to the target and the bandwidth.  .
}
\references{

Kokonendji, C.C. and Senga Kiesse, T. (2011). Discrete associated kernel method and extensions.
Statistical Methodology 8,  497 - 516.

Kokonendji, C.C., Senga Kiesse, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function.
Journal of Nonparametric Statistics 19,  241 - 254.

}
\author{ 
W. E. Wansouwe, C. C. Kokonendji and D.T. Kolyang.

}
\examples{
x<-4
h<-0.1
t<-0:10
dak(x,t,h,"bino")

function (x, t, h, ker) 
{
    if (ker == "dirac") {
        result <- t
        Logic1 <- (t == x)
        Logic0 <- (t != x)
        result[Logic0] <- 0
        result[Logic1] <- 1
        return(result)
    }
    else if (ker == "bino") {
        result <- t
        Logic0 <- (t <= x + 1)
        Logic1 <- (x + 1 < t)
        tval <- result[Logic0]
        result[Logic1] = 0
        result[Logic0] <- dbinom(tval, x + 1, (x + h)/(x + 1))
        return(result)
    }
    else if (ker == "pois") {
        result <- dpois(t, x + h)
        return(result)
    }
    else if (ker == "nebi") {
        result <- dnbinom(t, x + 1, (x + 1)/(2 * x + 1 + h))
        return(result)
    }
  }
}

