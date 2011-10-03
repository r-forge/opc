`ross.ordering` <-
function(x){
	mylength <- length(x[[1]])
	x1 <- as.character(x[[1]])
	my.ordering <- balance.pairs(table1(mylength), table2(mylength), mylength)
	mylength2 <- length(my.ordering[,1])
	dummy.ordering <- my.ordering
	
	for (i in 1:mylength2){
		my.ordering[i,1] <- x1[dummy.ordering[i,1]]
		my.ordering[i,2] <- x1[dummy.ordering[i,2]]
		}
	my.ordering
	
	
	
	
	}
