\name{rslvd_mammals}
\alias{rslvd_mammals}
\docType{data}
\title{Resolved mammalian names from taxaResolve()}
\description{
  A dataframe of names not present in Bininda-Edmonds et al. (2007) supertree, resolved using this package's \code{taxaResolve()}.
}
\usage{
   data(rslvd_mammals)
}
\format{
  \code{rslvd_mammals} is a \code{dataframe}
}
\source{
   Bininda-Edmonds et al. (2007) The Delayed Rise of Present-Day Mammals. Nature, 446(7135): 507-512
}
\examples{
data(rslvd_mammals)
# select lineage of random entry
rnd <- sample(1:nrow(rslvd_mammals), 1)
rslvd_mammals$lineage[rnd]
}
\keyword{datasets}
