best <- function(state, outcome) {
	outcomes <- c('heart attack', 'heart failure', 'pneumonia')
## Read outcome data
	measures <- read.csv('DATA/outcome-of-care-measures.csv', colClasses = 'character')
	
## Check that state and outcome are valid
	if(! state %in% measures$State)  stop('invalid state')
	if(! outcome %in% outcomes)  stop('invalid outcome')
	
## Return hospital name in that state with lowest 30-day death rate
	
	# col_pos[match(outcome, outcomes)] : 11 or 17 or 23
	col_pos <- c(11, 17, 23)[match(outcome, outcomes)]		
	
	# f : state로 거름
	f <- measures[measures$State==state & measures[col_pos]!='Not Available',]					
	
	x <- f[,c(2,col_pos)]	# name, 30daymortality...
	x <- x[order(as.numeric(x[,2]), x[,1]),]
	head(x[,1], 1)
}
