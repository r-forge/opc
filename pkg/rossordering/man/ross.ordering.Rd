\name{ross.ordering}
\alias{ross.ordering}
\title{Ross Ordering Function}
\description{
	This function exchanges the corresponding number in the balance.pairs matrix with the names of your sitmuli.
}
\usage{
ross.ordering(x)
}

\arguments{
  \item{x}{data frame with the stimuli listed in one column}
}
\details{
  Note that if your stimuli are numbers instead of names then the output produced will contain numbers.
}
\value{
 For a data frame, the number of stimuli in your dataset will return Ross Ordered matrix with number of rows equal to numstim x (numstim -1) / 2 and two columns.  This matrix contains all possible pairs excluding the duplicate pairs.  The stimuli in this matrix will be balanced, meaning that a stimulus will appear the same number of times in column 1 as column 2, as well as will be spaced apart within the column.

}
\references{Cohen, H.S. & Davison, M.L. (1973). Jiffy-Scale: A FORTRAN IV Program for Generating Ross-Ordered Paired Comparisons. Behavioral Science, 18(1), 76.
 
Ross, R.T. (1934). Optimimum Orders for the Presentation of Pairs in the Method of Paired Comparisons. Journal of Educational Psychology, 25(5), 375-382.	
	}
\author{ Julio Cabrera - cabre031@umn.edu

	Stacy Karl - karlx028@umn.edu

	Andrew Zieffler - zief002@umn.edu}

\seealso{ \code{\link{politicians}},\code{\link{balance.pairs}}, \code{\link{table1}}, \code{\link{table2} }}
\examples{
##Load Politician DataSet##
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

## The function is currently defined as
function(x){
	mylength <- length(x)
	x1 <- as.character(x)
	my.ordering <- balance.pairs(table1(mylength), table2(mylength), mylength)
	mylength2 <- length(my.ordering[,1])
	dummy.ordering <- my.ordering
	
	for (i in 1:mylength2){
		my.ordering[i,1] <- x1[dummy.ordering[i,1]]
		my.ordering[i,2] <- x1[dummy.ordering[i,2]]
		}
	my.ordering	
	}
}
