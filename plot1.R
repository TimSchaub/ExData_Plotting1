getFile <- function (){
  
  #open file handle
  file_in <- file("household_power_consumption.txt", "r")
  
  #get first line out (headers) and split them using ";" & cbind the splits
  theHeaders <- do.call("cbind", strsplit(readLines(file_in, n=1, ), ";"))
  
  
  #get first real dataset
  data <- readLines(file_in, n = 1)
  
  #check for required data between 2007-02-01 and 2007-02-02
  filtered <- grep ("^[1|2]/2/2007", data)
  
  #get the recordset/string, split and rbind it
  targetData <- data.frame(do.call("rbind", strsplit(data[filtered], ";")), stringsAsFactors = FALSE)
  
  #repeat in loop for 1000 recordsets each until no more data available
  
  
  while (length(data) > 0) {
    data <- readLines(file_in, n = 1000)
    newFiltered <- grep ("^[1|2]/2/2007", data)
    
    targetData <- rbind(targetData, data.frame(do.call("rbind", strsplit(data[newFiltered], ";")), stringsAsFactors = FALSE))

  }
  
  #close file
  close(file_in)
  
  
  #adding column headers and formating date and time columns
  names(targetData) <- theHeaders
  targetData$Date <- as.Date(targetData$Date, format='%d/%m/%Y')
  targetData$Time <- strptime(paste(targetData$Date, " ", targetData$Time), format='%Y-%m-%d %H:%M:%S')
  
  
  #return
  return(targetData)
  
}

df <- getFile()
hist(as.numeric(df$Global_active_power), col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.copy(png, width=480, height=480, file="plot1.png")
dev.off()
