df1 <- read.delim("http://people.brunel.ac.uk/~mastjjb/jeb/orlib/files/cap42.txt")
line = trimws(df1[1, ])
line = as.numeric(strsplit(substr(line, 1, nchar(line)-1), " ")[[1]])
capacityFixedCost <- data.frame("Capacity"=numeric(), "FixedCost" = numeric())
Demand = c()
transportatinCosts = c()
for (i in seq_along(df1[,1])){
  line <- trimws(df1[i,])
  line_length <- nchar(line)
  string = as.numeric(strsplit(substr(line, 1, nchar(line)), " ")[[1]])
  if(substr(line, line_length, line_length) == "."){
    #line <- as.numeric(strsplit(substr(line, 1, nchar(line)-1), " ")[[1]])
    capacityFixedCost = rbind(capacityFixedCost, data.frame("Capacity" = string[1], "FixedCost"=string[2]))

  }else if(length(string) == 1){
    Demand <- append(Demand, string)
  }else{
    transportatinCosts = append(transportatinCosts, string)
  }
}
transCosts <- t(matrix(transportatinCosts, nrow(capacityFixedCost),length(Demand)))

