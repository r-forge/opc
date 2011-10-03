\name{forms}
\alias{forms}
\title{Function that creates forms}
\description{
  Creates n forms
}
\usage{
ross.forms(x, forms=2, paper=FALSE)
}

\arguments{
  \item{x}{vector of stimuli}
  \item{forms}{number of forms to create}
  \item{paper}{logical to recreate example in paper}
}
\details{
  The function will create the forms based on a vector of stimuli.
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
ross.ordering(politicians)
