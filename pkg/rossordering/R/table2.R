`table2` <-
function (numstim){
       ifelse(numstim%%2==0, numstim1 <- numstim+1, numstim1 <- numstim)
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
                       if(evencol%%2==0){       ### Check to see that evencol is an even number ###
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

 