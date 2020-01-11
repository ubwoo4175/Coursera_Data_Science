rankhospital <- function(state, outcome, num = 'best') {
	outcomes <- c('heart attack', 'heart failure', 'pneumonia')
## Read outcome data
	measures <- read.csv('DATA/outcome-of-care-measures.csv', colClasses = 'character')
	
## Check that state and outcome are valid
	if(! state %in% measures$State)  stop('invalid state')
	if(! outcome %in% outcomes)  stop('invalid outcome')
	
## Return hospital name in that state with the given rank (30-day death rate)
	
	# col_pos[match(outcome, outcomes)] : 11 or 17 or 23
	col_pos <- c(11, 17, 23)[match(outcome, outcomes)]
	
	# f : state로 거르고, 값이 없는 것 제외
	f <- measures[measures$State==state & measures[col_pos]!='Not Available',]
	
	x <- f[,c(2,col_pos)]	# name, 30daymortality...
	x <- x[order(as.numeric(x[,2]), x[,1]),]
	names(x) <- c('Hospital.Name', 'Rate')
	
	if(num=='best') num <- 1
	else if(num=='worst') num <- nrow(x)
	
	x[num,1]
}
