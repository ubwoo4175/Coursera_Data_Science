rankall <- function(outcome, num = 'best') {
	outcomes <- c('heart attack', 'heart failure', 'pneumonia')
## Read outcome data
	measures <- read.csv('DATA/outcome-of-care-measures.csv', colClasses = 'character')
	
## Check that outcome is valid
	if(! outcome %in% outcomes)  stop('invalid outcome')
	
## For each state, find the hospital of the given rank
## Return a data frame with the hospital names and the (abbreviated) state name
	
	# col_pos[match(outcome, outcomes)] : 11 or 17 or 23
	col_pos <- c(11, 17, 23)[match(outcome, outcomes)]
	
	states <- as.factor(measures$State)
	r <- data.frame('hospital'='a', 'state'=levels(states), stringsAsFactors = FALSE)
	for(i in levels(states)) {
		# f : state로 거르고, 값이 없는 것 제외
		f <- measures[measures$State==i & measures[col_pos]!='Not Available',]
		
		x <- f[,c(2,col_pos)]	# name, 30daymortality...
		x <- x[order(as.numeric(x[,2]), x[,1]),]
		names(x) <- c('Hospital.Name', 'Rate')
		
		if(num=='best') n <- 1
		else if(num=='worst') n <- nrow(x)
		else n <- num
		
		r[r$state==i,1] <- x[n,1]
	}
	r
}
