met.gapfill <- function(met.data, met.var){
  val.na <- which(is.na(met.data[,met.var]))
  
  if(length(val.na)==0){
    return("Done: No gapfilling necessary.")
    # return
  } 
  
  while(length(val.na)>0){
    if(val.na[1]==1) return(warning("Missing first data point! Stop and re-evaluate"))
    
    gap.now <- val.na[1]
    if(length(val.na)>1){
      for(i in 2:length(val.na)){
        if(val.na[i] == gap.now[length(gap.now)]+1){
          gap.now <- c(gap.now, val.na[i])
        }
      }
    }
    
    obs.start <- min(gap.now)-1 # The last observation you had before the break
    obs.next <-  max(gap.now)+1 # The next observation you have
    
    # Create a straight line through the missing data
    obs.fill <- seq(met.data[obs.start, met.var], met.data[obs.next, met.var], length.out = length(gap.now)+2)
    obs.fill <- obs.fill[2:(length(obs.fill)-1)] # Trim out the observed start/end
    
    # Put the gap filled data back in palce
    met.data[gap.now, met.var] <- obs.fill
    
    # Update our missing values
    val.na <- which(is.na(met.data[,met.var]))
    
    if(length(val.na)==0){ return(paste0(met.var, " gapfilling complete"))}
    if(val.na[length(val.na)]==nrow(met.data)) return(warning("Missing last data point! Stop and re-evaluate"))
    
  }
}
