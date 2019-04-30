\name{oberauer01}
\alias{oberauer01}
\docType{data}
\title{
  Oberauer & Kliegl's (2001) set-size effect on accuracy in WM updating
}
\description{
  Trial-level data for Oberauer & Kliegl's (2001) set-size effect
  on accuracy in working memory updating, using simple arithemtic
  operations on digits.
}
\usage{data("oberauer01")}
\format{
  A data frame with 2304 observations on the following 4 variables.
  \describe{
    \item{\code{subject}}{Unique ID number for each subject.}
    \item{\code{study}}{Part of study: 1-2}
    \item{\code{size}}{Set size: 1-4 in study part 1, 4-6 in study part
    2}
    \item{\code{trial}}{Trial number: 1-261}
    \item{\code{acc}}{Proportion correct}
  }
}
\details{

  These are the trial-level data collected by Oberauer & Kliegl (2001),
  listed as benchmark dataset in working memory by Oberauer et
  al. (2018, BM1.1), and archived in Oberauer (2019). They comprise the
  proportion correct scores for 18 young adults from Germany. In the
  first part of the experiment, set sizes 1-4 were investigated; in the
  second part, the set sizes were 4-6.

  Participants were presented with X digits in a circle. Each of of
  these were then updated with a simple arithmetic operation
  (e.g. "+3"). The order of updates was always clockwise from the same
  starting point. Memory for each updated location was then probed, in a
  random order.

  NOTE: The presentation times for each of the arithmetic operations
  varied according to an adaptive procedure. The maximum presentation
  time was 6000 ms. Following Oberauer et al. (2018), only these
  maximally long trials were included in the benchmark. Hence, trial
  numbers in this dataset are not continuous, and represent a subset of
  the trials participants experienced.

  NOTE: Oberauer & Kliegl (2001) also tested older adults. Following
  Oberauer et al. (2018), these data are not included in this benchmark.

}
\source{

  Oberauer, K. (2019). BenchmarksWM. https://github.com/oberauer/BenchmarksWM

}
\references{ 

  Oberauer, K., Lewandowsky, S., Awh, E., Brown, G.D.A, Conway, A.,
  Cowan, N., Donkin, C., Farrell, S., Hitch, G.J., Hurlstone, M.J., Ma,
  W.J., Morey, C.C., Nee, D.E., Schweppe, J., Vergauwe, E., and Ward,
  G. (2018). Benchmarks for models of short-term and working
  memory. \emph{Psychological Bulletin, 144}, 885-958.

  Oberauer, K., & Kliegl, R. (2001). Beyond resources: Formal models of
  complexity effects and age differences in working memory. \emph{The Euro-
  pean Journal of Cognitive Psychology, 13}, 187–215.
  
}
\examples{

    ## Reproducing Figure 2D of Oberauer et al. (2018).
    data(oberauer01)
    library(tidyverse)
    ober01sum  <- oberauer01  \%>\%
        group_by(study, size) \%>\%
        summarise(acc = mean(acc))
    ober01sum \%>\%
        ggplot(aes(x=size, y=acc, color=factor(study))) +
        geom_point() + geom_line() + ylim(0.5,1)

}

\keyword{datasets}
