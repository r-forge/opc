\name{table2}
\alias{table2}

\title{Function that creates Table 2}
\description{
  Table 2 is filled with values between 2 and numstim1 + 1. 
}
\usage{
table2(numstim)
}

\arguments{
  \item{numstim}{ number of stimuli}
}
\details{
   If numstim is odd, then numstim1=numstim.  If numstim is even, then numstim1 = numstim + 1.
}
\value{
  This will output a matrix with number of rows equal to (numstim1 + 1)/2 and number of columns equal to (numstim1 - 1).  Table 2 is an array of columns; the first column is filled in consecutive order beginning with the number 2; the first two inbetween even and odd columns are filled with the first column's row value + 1.  The even and odd pair of columns directly following the previous pair of even and odd columns are the previous column's row value + 1; finally, the last column follows the inbetween sequence, but instead of outputting two columns, it only outputs one column.
}
\references{Cohen, H.S. & Davison, M.L. (1973). Jiffy-Scale: A FORTRAN IV Program for Generating Ross-Ordered Paired Comparisons. Behavioral Science, 18(1), 76.
 
Ross, R.T. (1934). Optimimum Orders for the Presentation of Pairs in the Method of Paired Comparisons. Journal of Educational Psychology, 25(5), 375-382.	
	}
\author{ Julio Cabrera - cabre031@umn.edu

	Stacy Karl - karlx028@umn.edu

	Andrew Zieffler - zief002@umn.edu}

\seealso{ \code{\link{politicians}},\code{\link{ross.ordering}}, \code{\link{balance.pairs}}, \code{\link{table1}}}

\examples{
##Load Politician DataSet##
data(politicians)
numstim <- length(politicians$Politicians)
table2(numstim)

## The function is currently defined as
function (numstim){
       ifelse(numstim\%\%2==0, numstim1 <- numstim+1, numstim1 <- numstim)
       numrow <- (numstim1 + 1)/2
       numcol <- numstim1 - 1

       blank.table2 <- matrix(nrow=numrow, ncol=numcol)

       ### Fill in the first column of table 2 ###

       for(row in 1:numrow){
               value <- row + 1
               blank.table2[row,1] <- value
               }

               value <- 1  ### Reset the value to 1

               ### Fill in the middle of the table ###

               for(evencol in 2:(numcol-2)){
                       if(evencol\%\%2==0){       ### Check to see that evencol is an even number ###
                               oddcol <- evencol + 1
                               value <- value + 1

                               for (row in 1:numrow){
                                       value2 <- value + row
                                       blank.table2[row,evencol] <- value2
                                       blank.table2[row,oddcol] <- value2
                                       }


                               }
                       else
                               evencol <- evencol + 2   ### If evencol is odd add 1 and reloop ###
                       }

               ### Fill in the last column ###

               for(row in 1:numrow){
                       value2 <- value + row + 1
                       blank.table2[row,numcol] <- value2
                       }

       blank.table2   ### Output table 2

       }
}
