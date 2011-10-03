\name{table1}
\alias{table1}
\title{Function that creates Table 1}
\description{
  Table 1 is filled with values between 1 and numstim + 1 (if numstim is even) or numstim (if numstim is odd)
}
\usage{
table1(numstim)
}

\arguments{
  \item{numstim}{number of stimuli}
}
\details{
  If numstim is odd, then numstim1=numstim.  If numstim is even, then numstim1 = numstim + 1.
}
\value{
  This will output a matrix with number of rows equal to (numstim1 + 1)/2 and number of columns equal to (numstim1 - 1).  Table1 is filled with values between 1 and numstim1.  First, taking two columns at a time, the same value is put into each column of the same row until the value is 2.  Then the columns are completed beginning with the value being numstim1.  When the whole table is filled, the value at the beginning and the end of all odd columns is assigned to 1.
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
numstim <- length(politicians$Politicians)
table1(numstim)

## The function is currently defined as
function (numstim){
       ifelse(numstim\%\%2==0, numstim1 <- numstim+1, numstim1 <- numstim)   ### Check that the number of stimuli is odd
       
       numrow <- (numstim1 + 1)/2
       numcol <- numstim1 - 1
       blank.table <- matrix(nrow=numrow, ncol=numcol)   ### Set up a blankmatrix of correct dimension

       for(evencol in 2:numstim1){
               if(evencol\%\%2==0){   ### Check that evencol is even

               oddcol <- evencol - 1
               partcol <- evencol / 2


               for(row in 1:partcol){
                       value <- partcol - row + 2
                       blank.table[row,evencol] <- value
                       blank.table[row,oddcol] <- value
                       }

               partcol <- partcol + 1
               value2 <- 0

               for(leftover in partcol:numrow){
                       blank.table[leftover,evencol] <- numstim1 - value2
                       blank.table[leftover,oddcol] <- numstim1 - value2
                       value2 <- value2 + 1
                       }

               blank.table[1,oddcol] <- 1
               blank.table[numrow,oddcol] <- 1

               }

               else
                       evencol <- evencol+1   ### If evencol is odd add 1 and re-loop
               }

       blank.table   ### Output table 1
       }
}
