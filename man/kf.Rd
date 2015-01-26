\encoding{UTF-8}
\name{kf}
\alias{kf}

\title{
Discrete associated kernel function
}
\description{
This function computes the discrete associated kernel function. 
}
\usage{
kf(x, t, h, ker, a = 1, c = 2)
}

\arguments{
  \item{x}{
The target.
}
  \item{t}{
A single value or the grid  where the discrete associated kernel function is computed.
}
  \item{h}{
The bandwidth or smoothing parameter.
}
  \item{ker}{
The associated kernel: "dirDU" DiracDU,"bino" Binomial, "triang" Discrete Triangular kernel.
}
  
  \item{a}{
The arm in Discrete Triangular kernel. The default value is 1.
}
  \item{c}{
The number of categories in DiracDU kernel. The default value is 2.
}
}
\details{
The associated kernel is one of the three which have been defined in the sections above : DiracDU, Binomial and Discrete Triangular; see Kokonendji and Senga Kiessé (2011), and also Kokonendji et al. (2007).
            
}

\value{
Returns the value of the discrete associated kernel function at t according to the target and the bandwidth.
}
\references{


Kokonendji, C.C. and Senga Kiessé, T. (2011). Discrete associated kernel method and extensions,
 \emph{ Statistical Methodology}  \bold{8},  497 - 516.

Kokonendji, C.C., Senga Kiessé, T. and Zocchi, S.S. (2007). Discrete triangular distributions and non-parametric estimation for
probability mass function,
 \emph{ Journal of Nonparametric Statistics }  \bold{19},  241 - 254.

}
\author{
W. E. Wansouwé, C. C. Kokonendji and D. T. Kolyang
}

\examples{
x<-4
h<-0.1
t<-0:10
kf(x,t,h,"bino")

}

