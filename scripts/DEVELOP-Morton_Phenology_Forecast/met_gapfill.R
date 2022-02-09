met.gapfill <- function(met.data, met.var){
  val.vec <- met.data[,met.var]
  
  val.na <- which(is.na(val.vec))
  
  if(length(val.na)==0){
    print("Done: No gapfilling necessary.")
    # break
    # return
  } 
  
  while(length(val.na)>0){
    if(val.na[1]==1){ 
      stop("Missing first data point! Stop and re-evaluate")
      # break
    }
    
    gap.now <- val.na[1]
    if(length(val.na)>1){
      for(i in 2:length(val.na)){
        if(val.na[i] == gap.now[length(gap.now)]+1){
          gap.now <- c(gap.now, val.na[i])
        } else break 
      }
    }
    
    obs.start <- min(gap.now)-1 # The last observation you had before the break
    obs.next <-  max(gap.now)+1 # The next observation you have
    
    # Create a straight line through the missing data
    obs.fill <- seq(met.data[obs.start, met.var], met.data[obs.next, met.var], length.out = length(gap.now)+2)
    obs.fill <- obs.fill[2:(length(obs.fill)-1)] # Trim out the observed start/end
    
    # Put the gap filled data back in palce
    val.vec[gap.now] <- obs.fill
    
    # Update our missing values
    val.na <- which(is.na(val.vec))
    
    # return(met.data)
    if(length(val.na)==0){ 
      print(paste0(met.var, " gapfilling complete"))
      return(val.vec)
      break
    }
    if(val.na[length(val.na)]==nrow(met.data)){ 
      warning("Missing last data point! Stop and re-evaluate")
      return(val.vec)
      break
    }
    
  }
}
