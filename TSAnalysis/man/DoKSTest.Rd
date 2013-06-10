\name{DoKSTest}
\alias{DoKSTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
DoKSTest(input.list, time.x = 120)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input.list}{
%%     ~~Describe \code{input.list} here~~
}
  \item{time.x}{
%%     ~~Describe \code{time.x} here~~
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
function (input.list, time.x = 120) 
{
    ks.return <- list(length = length(input.list))
    for (k in 1:length(input.list)) {
        time.points <- input.list[[k]][[1]][, 1]
        time.look <- which(time.points > 120)
        print(time.look[1])
        print(input.list[[k]][[2]][time.look[1], ])
        ks.return[[k]] <- ks.test(input.list[[k]][[1]][time.look[1], 
            ], as.numeric(input.list[[k]][[2]][time.look[1], 
            ]))
    }
    return(ks.return)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
