\name{FeaturesMaker}
\alias{FeaturesMaker}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
FeaturesMaker(matrix.input, a.thresh, r.thresh)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix.input}{
%%     ~~Describe \code{matrix.input} here~~
}
  \item{a.thresh}{
%%     ~~Describe \code{a.thresh} here~~
}
  \item{r.thresh}{
%%     ~~Describe \code{r.thresh} here~~
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
function (matrix.input, a.thresh, r.thresh) 
{
    number.features <- 9
    matrix.in <- matrix.input[, 2:ncol(matrix.input)]
    rows.in <- nrow(matrix.in)
    cols.in <- ncol(matrix.in)
    output.matrix <- matrix(, nrow = cols.in, ncol = number.features)
    colnames(output.matrix) <- list("Mins", "Means", "Medians", 
        "Mean", "StartValue", "t.absThr", "t.relThr", "t.absDec", 
        "t.relDec")
    output.matrix[, 1] <- apply(matrix.in, 2, min)
    output.matrix[, 2] <- apply(matrix.in, 2, mean)
    output.matrix[, 3] <- apply(matrix.in, 2, median)
    output.matrix[, 4] <- apply(matrix.in, 2, max)
    output.matrix[, 5] <- as.numeric((matrix.in[1, ]))
    output.matrix[, 6] <- apply(matrix.in, 2, function(x) TimeToThresh(x, 
        a.thresh))
    output.matrix[, 7] <- apply(matrix.in, 2, function(x) TimeToRel(x, 
        r.thresh))
    print(output.matrix[, 7])
    decaytime.temp <- apply(matrix.in, 2, function(x) TimeToDec(x, 
        r.thresh))
    output.matrix[, 8] <- decaytime.temp - output.matrix[, 6]
    decayrel.temp <- apply(matrix.in, 2, function(x) TimeDecRel(x, 
        r.thresh))
    output.matrix[, 9] <- decayrel.temp - output.matrix[, 7]
    return(output.matrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
