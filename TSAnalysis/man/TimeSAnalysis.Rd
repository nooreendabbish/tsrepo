\name{TimeSAnalysis}
\alias{TimeSAnalysis}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
TimeSAnalysis(names, save.dir, thresholds, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{names}{
%%     ~~Describe \code{names} here~~
}
  \item{save.dir}{
%%     ~~Describe \code{save.dir} here~~
}
  \item{thresholds}{
%%     ~~Describe \code{thresholds} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (names, save.dir, thresholds, ...) 
{
    input.list <- GetEllipsis(...)
    ts.cols.paired <- list()
    for (k in seq(1, length(input.list), by = 2)) {
        ts.cols.paired[[k]] <- ReadNaRescale(as.character(input.list[[k]]), 
            as.character(input.list[[k + 1]]))
    }
    feat.list <- FeatureFinder(ts.cols.paired, thresholds[1], 
        thresholds[2])
    GraphMaker1(ts.cols.paired, names, save.dir)
    ksout <- DoKSTest(ts.cols.paired)
    pcaout <- GraphMaker2(feat.list, names, save.dir)
    list(ks.results = ksout, features.pca.results = pcaout)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
