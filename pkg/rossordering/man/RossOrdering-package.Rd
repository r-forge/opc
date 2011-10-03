\name{RossOrdering-package}
\alias{RossOrdering-package}
\alias{RossOrdering}
\docType{package}
\title{
Ross Ordering
}
\description{
 This package has been developed at the University of Minnesota - Twin Cities to include functions that produce the Ross Order for any number of stimuli.  This is most useful for paired comparison judgements.}
\details{
\tabular{ll}{
Package: \tab RossOrdering\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2009-05-01\cr
License: \tab GPL (>=2) \cr
LazyLoad: \tab yes\cr
}
Index:

\code{\link{RossOrdering}} A package for paired comparisons.

Functions:

\code{\link{table1}} Table 1 is filled with values between 1 and numstim + 1 (if numstim is even) or numstim (if numstim is odd)

\code{\link{table2}} Table 2 is filled with values between 2 and numstim1 + 1.

\code{\link{balance.pairs}}  This function produces the Ross Order for any number of stimuli. 

\code{\link{ross.ordering}}  This function exchanges the corresponding number in the balance.pairs matrix with the names of your sitmuli.

Datasets included in the ross ordering package:

\code{\link{politicians}} Example data frame with 8 politicians' names for Ross Ordering Pairing
}


\author{ Julio Cabrera - cabre031@umn.edu

	Stacy Karl - karlx028@umn.edu
	
	Andrew Zieffler - zief002@umn.edu
	
	Department of Educational Psychology<br>
	University of Minnesota<br>
	Minneapolis, MN

Maintainer: Julio Cabrera <cabre031@umn.edu>
}

\references{Cohen, H.S. & Davison, M.L. (1973). Jiffy-Scale: A FORTRAN IV Program for Generating Ross-Ordered Paired Comparisons.  Behavioral Science, 18(1), 76.
 
Ross, R.T. (1934). Optimimum Orders for the Presentation of Pairs in the Method of Paired Comparisons. Journal of Educational Psychology, 25(5), 375-382.

}



\examples{
##Load data.frame##
data(politicians)
ross.ordering(politicians)

#Create a column of stimuli#
animals <- rbind("cat", "mouse", "rabbit", "horse", "elephant", "moose", "rock")
animals.dataframe <- as.data.frame(animals)
ross.ordering(animals.dataframe)

#Number Stimuli##
numbers <- rbind(4,5,8,2,16)
numbers.df <- as.data.frame(numbers)
ross.ordering(numbers.df)
}
