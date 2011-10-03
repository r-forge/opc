\name{factoring}
\alias{factoring}
\title{Function that factors for form creation}
\description{
  Offers alternatives for the number of forms to create
}
\usage{
factoring(x)
}

\arguments{
  \item{x}{vector of stimuli}
}
\details{
  The function will use the vector of stimuli and determine the number of forms that could be created.
}
\value{
  Text is written to the screen.
}
\references{Cohen, H.S. & Davison, M.L. (1973). Jiffy-Scale: A FORTRAN IV Program for Generating Ross-Ordered Paired Comparisons. Behavioral Science, 18(1), 76.
 
Ross, R.T. (1934). Optimimum Orders for the Presentation of Pairs in the Method of Paired Comparisons. Journal of Educational Psychology, 25(5), 375-382.	
	}
\author{ Julio Cabrera - cabre031@umn.edu

	Stacy Karl - karlx028@umn.edu

	Andrew Zieffler - zief002@umn.edu}

\seealso{ \code{\link{politicians}},\code{\link{ross.ordering}}, \code{\link{balance.pairs}}, \code{\link{table2}}}
\examples{

##Load Politician DataSet##
data(politicians)
factoring(politicians)
