'matrix.sampling' <- function (x, optimal.blocking=FALSE, paper=FALSE){
	
	
	#rownames(new) <- 1:length(new[,1])
	#new

	numstim <- nrow(x)
	### For odd number of stimuli
	
	if(numstim%%2 == 1){
		new <- ross.ordering(x, paper)
		numblock <- numstim
		numstim.block <- (numstim-1)/2
		
		cat("\n\n")
		for(i in 1:numblock){
			cat("Block",i)
			cat("\n------------------------------------------------------------\n")
			for(j in (i*numstim.block-(numstim.block-1)):(i*numstim.block)){
				write.table(new[j,], col.names=FALSE, quote=FALSE, sep="\t\t")
				}  ## End embedded for-loop
			cat("------------------------------------------------------------\n\n\n")
			
			}  ## End for-loop
		
		}  ## End if-statement for odd number of stimuli
	
	### For even number of stimuli
	
	else{
		new <- ross.ordering(x, paper)
		numblock <- numstim - 1
		numstim.block <- (numstim)/2 

		tempblock <- numstim + 1

		
		
			new.blocking <- c(1, seq(2, numstim, by=2), seq(3, numstim+1, by=2))
			new.ordering <- data.frame()
		
			for (block in new.blocking){
				new.ordering <- rbind(new.ordering, new[(numstim.block*block-numstim.block+1):(numstim.block*block),])
				}  ## End for-loop
	
			new <- na.omit(new.ordering)	## Balanced, ordered arrangement of stimuli	
		
		

		if(optimal.blocking==FALSE){
		## Pretty Print the balanced, ordered stimuli
		
		cat("\n\n")
		for(i in 1:numblock){
			cat("Block",i)
			cat("\n------------------------------------------------------------\n")
			for(j in (i*numstim.block-(numstim.block-1)):(i*numstim.block)){
				write.table(new[j,], col.names=FALSE, quote=FALSE, sep="\t\t")
				}  ## End embedded for-loop
			cat("------------------------------------------------------------\n\n\n")
			
			}	## End for loop	
		}  ## End if-statement for optimal blocking==FALSE
		
		else{
			swap.pairs.old <- seq(from = 1, to = (numstim.block*numblock), by = numstim.block)
			swap.pairs.new <- seq(from = (1+numstim.block), to = (numstim.block*numblock+numstim.block), by = numstim.block) %% (numstim.block*numblock)
			new[swap.pairs.old,] <- new[swap.pairs.new,]
			## Pretty Print the balanced, ordered stimuli
		
		cat("\n\n")
		for(i in 1:numblock){
			cat("Block",i)
			cat("\n------------------------------------------------------------\n")
			for(j in (i*numstim.block-(numstim.block-1)):(i*numstim.block)){
				write.table(new[j,], col.names=FALSE, quote=FALSE, sep="\t\t")
				}  ## End embedded for-loop
			cat("------------------------------------------------------------\n\n\n")
			
			}	## End for loop	
			
		}  ## End else-statement for optimal blocking==TRUE
		
		
		
	}  ## End else-statement	for even number of stimuli

}  ## End function


