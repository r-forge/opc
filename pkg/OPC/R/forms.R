`ross.forms` <- function(x, forms=2, paper=FALSE){
  
  numstim <- length(x[[1]]) ###Number of stimuli as determined by the number of rows in the file--9
  numblock <- numstim ###Number of stimuli will be equal to the number of blocks -- 9
  numstim.block <- (numstim-1)/2 ###The number of pairs in each block -- 4
  
  if(numstim%%2==0){  ### Check to see if the number of stimuli is even
  	
  	   
    
  }  ## End if-statement for even number of stimuli

### For odd number of stimuli
### This will split the last block evenly into the appropriate  number of forms
  else{
    max.forms <- (numstim-1)/2 ## Maximum number of forms is determined -- 4
    new <- ross.ordering(x, paper)  ## Perform Ross ordering
    my.seq <- seq(from=1, to=numstim.block, by=(max.forms/forms)) ###create a sequence starting at 1 to the number of pairs in each block
		
  for(i in 1:forms){ #for each form...1 and 2
  	 cat("\n\n")   
    cat("FORM",i) #Write "Form 1" and "Form 2"
    cat("\n\n")    
    cat("Block", numblock)
    cat("\n------------------------------------------------------------\n")
    start <- (numstim-1)*numstim.block + my.seq[i]  ###start with the first pair in the last block
    end <- start+numstim.block/forms-1  ###end with the pair of the last block for given form

    anchor <- as.matrix(new[start:end,])  ###set up matrix of stimuli included from last block
    
  

    colnames(anchor) <- c("", "")
    rownames(anchor) <- rep("", times = nrow(anchor))
    print(anchor, quote=FALSE)		
	cat("------------------------------------------------------------\n\n\n")
    
    which.block <- seq(from=1, to=numblock-1, by=1)
  
    for(stacy in 1:length(which.block)){
      crap.block <- which.block[stacy]
      one <- c()
    
		## Get the stimuli names in the jth block    
      for(j in (numstim.block*(crap.block-1)+1):(crap.block*numstim.block)){
        one <- c(one, new[j,1], new[j,2])
		  }  ## End for-loop

    bs <- as.vector(as.matrix(anchor))  ## Stimuli names from the last block on Form i
    test <- sum(match(one,bs), na.rm=T)  ## Check the number of matches
    actual <- ((length(bs)-1)+1)*mean(c(1, length(bs))) #sums the matches

    if (test!=actual) {
      cat("Block",which.block[stacy])
      cat("\n------------------------------------------------------------\n")
      my.block <- matrix(one, ncol=2, byrow=T)
      colnames(my.block) <- c("", "")
      rownames(my.block) <- rep("", times = numstim.block)
      print(my.block, quote=FALSE)
      cat("------------------------------------------------------------\n\n\n")
      }  ## End if-statement			
	
    }  ## End for-loop
			
  }  ## End forms for loop			

}  ## End Else			

}  ## End Function