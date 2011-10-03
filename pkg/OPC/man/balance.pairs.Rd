\name{balance.pairs}
\alias{balance.pairs}
\title{ Balancing Pairs Function }
\description{
  This function produces the Ross Order for any number of stimuli. This function also balances the pairs so that the same stimulus shows up an equal amount of times on both sides.}
\usage{
balance.pairs(table1, table2, numstim)
}
\arguments{
  \item{table1}{Table 1 is filled with values between 1 and numstim + 1 (if numstim is even) or numstim (if numstim is odd) }
  \item{table2}{Table 2 is an array of columns; the first column is filled in consecutive order beginning with the number 2; the first two inbetween even and odd columns are filled with the first column's row value + 1.  The even and odd pair of columns directly following the previous pair of even and odd columns are the previous column's row value + 1; finally, the last column follows the inbetween sequence, but instead of outputting two columns, it only outputs one column.}
  \item{numstim}{numstim is the number of stimuli}
}
\value{
  For a data frame, the number of stimuli input will return a matrix with number of rows equal to numstim x (numstim -1) / 2 and two columns.  This matrix contains all possible pairs excluding the duplicate pairs.  The stimuli in this matrix will be balanced, meaning that a stimulus will appear the same number of times in column 1 as column 2, as well as will be spaced apart within the column. Note that the names of your stimuli will not be produced, only the corresponding number.
  }
\references{Cohen, H.S. & Davison, M.L. (1973). Jiffy-Scale: A FORTRAN IV Program for Generating Ross-Ordered Paired Comparisons.  Behavioral Science, 18(1), 76.
 
Ross, R.T. (1934). Optimimum Orders for the Presentation of Pairs in the Method of Paired Comparisons.  Journal of Educational Psychology, 25(5), 375-382.	
	}
\author{ Julio Cabrera - cabre031@umn.edu

	Stacy Karl - karlx028@umn.edu

	Andrew Zieffler - zief002@umn.edu}

\seealso{ \code{\link{politicians}},\code{\link{ross.ordering}}, \code{\link{table1}}, \code{\link{table2}}}
\examples{
##Load Politician DataSet##
data(politicians)
numstim <- length(politicians$Politicians)
balance.pairs(table1(numstim), table2(numstim), numstim)

##FUNCTION THAT PRINTS OUT THE PAIRS IN ROSS ORDER##

function (table1, table2, numstim){
       pairing1 <- as.vector(table1)
       pairing2 <- as.vector(table2)
       pair <- cbind(pairing1, pairing2)
       ifelse(numstim\%\%2==0, numstim1 <- numstim+1, numstim1 <- numstim)		
       numstim2 <- numstim1 + 1
	
 if(numstim\%\%2==0){
       	for(i in 1:length(pairing1)){
               if(i\%\%numstim2==0){
                       pair[i,] <- NA
                       }
				pair2 <- replace(pair, pair > numstim1, NA)
       			pre.ross.ordering <- data.frame(na.omit (pair2))
               }

       k1 <- (numstim1-1)/2 - 1
       k2 <- (numstim1 + 1)/2
       irow <- 0

       for(i in 1:k1){
               irow <- irow + (numstim1 - 1)/2

               for(j in 1:k2){
                       irow <- irow + 1
                       temp <-pre.ross.ordering[irow,1]
                       pre.ross.ordering[irow,1] <- pre.ross.ordering[irow,2]
                       pre.ross.ordering[irow,2] <- temp
                       }

               }

               irow <- length(pre.ross.ordering$pairing1) - k2 + 1

               if(pre.ross.ordering[irow,1]==1 || pre.ross.ordering[irow,2]==1) {
                       temp <- pre.ross.ordering[irow,1]
                       pre.ross.ordering[irow,1] <- pre.ross.ordering[irow,2]
                       pre.ross.ordering[irow,2] <- temp
                       pre.ross.ordering <- pre.ross.ordering
                       }


                        pro2 <- replace(pre.ross.ordering, pre.ross.ordering > numstim, NA)
                       pre.ross.ordering2 <-data.frame(na.omit (pro2))
                       pre.ross.ordering2

  }
               else
               {
						for(i in 1:length(pairing1)){
               if(i\%\%numstim2==0){
                       pair[i,] <- NA
                       }
			pair2 <- replace(pair, pair > numstim1, NA)
       			pre.ross.ordering <- data.frame(na.omit (pair2))
               }

       k1 <- (numstim1-1)/2 - 1
       k2 <- (numstim1 + 1)/2
       irow <- 0

       for(i in 1:k1){
               irow <- irow + (numstim - 1)/2

               for(j in 1:k2){
                       irow <- irow + 1
                       temp <-pre.ross.ordering[irow,1]
                       pre.ross.ordering[irow,1] <- pre.ross.ordering[irow,2]
                       pre.ross.ordering[irow,2] <- temp
                       }

               }

               irow <- length(pre.ross.ordering$pairing1) - k2 + 1

               if(pre.ross.ordering[irow,1]==1 || pre.ross.ordering[irow,2]==1) {
                       temp <- pre.ross.ordering[irow,1]
                       pre.ross.ordering[irow,1] <- pre.ross.ordering[irow,2]
                       pre.ross.ordering[irow,2] <- temp
                       pre.ross.ordering
                       }                       }

       }
}
