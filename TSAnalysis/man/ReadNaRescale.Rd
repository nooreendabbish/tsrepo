\name{ReadNaRescale}
\alias{ReadNaRescale}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ReadNaRescale(filename.a, filename.b)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename.a}{
%%     ~~Describe \code{filename.a} here~~
}
  \item{filename.b}{
%%     ~~Describe \code{filename.b} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename.a, filename.b) 
{
    c1 <- read.csv(filename.a)
    c2 <- read.csv(filename.b)
    c1[is.na(c1)] <- 0
    c2[is.na(c2)] <- 0
    c1.rescaled <- Rescaler(c1, c2)
    c2.rescaled <- Rescaler(c2, c1)
    ts.cols <- list(c1.rescaled, c2.rescaled)
    return(ts.cols)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
