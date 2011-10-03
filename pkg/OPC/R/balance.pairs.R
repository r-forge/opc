`balance.pairs` <-
function (table1, table2, numstim){
       pairing1 <- as.vector(table1)
       pairing2 <- as.vector(table2)
       pair <- cbind(pairing1, pairing2)
       ifelse(numstim%%2==0, numstim1 <- numstim+1, numstim1 <- numstim)		
       numstim2 <- numstim1 + 1
	
 if(numstim%%2==0){
       	for(i in 1:length(pairing1)){
               if(i%%numstim2==0){
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
               if(i%%numstim2==0){
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