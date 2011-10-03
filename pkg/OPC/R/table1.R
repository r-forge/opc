`table1` <-
function (numstim){
       ifelse(numstim%%2==0, numstim1 <- numstim+1, numstim1 <- numstim)   ### Check that the number of stimuli is odd
       
       numrow <- (numstim1 + 1)/2
       numcol <- numstim1 - 1
       blank.table <- matrix(nrow=numrow, ncol=numcol)   ### Set up a blankmatrix of correct dimension

       for(evencol in 2:numstim1){
               if(evencol%%2==0){   ### Check that evencol is even

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
       
       
       