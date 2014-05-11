# Function to generate plot2
plot2<-function()
{
    #read the first 2 colunmns of the data set - Date and Time
    d<-read.csv('.\\data\\household_power_consumption.txt',sep=';',colClasses=c(NA,NA,"NULL","NULL","NULL","NULL","NULL","NULL","NULL"))
    #concatenate the data and time as characters
    eDate<-paste(d$Date,d$Time)
    #generate DataTime class objects by using strptime and as.Date function
    eDate<-as.Date(strptime(paste(d$Date,d$Time),"%d/%m/%Y %H:%M:%S"))
    #set the start date
    startDate<-as.Date("2007-02-01")
    #set the end date
    endDate<-as.Date("2007-02-03")
    #find the rows containing data between the start date and the end date
    c<-(eDate>=startDate & eDate<endDate)
    #load the rest of the dataset
    d<-read.csv('.\\data\\household_power_consumption.txt',sep=';',header=TRUE,colClasses=c("NULL","NULL",NA,NA,NA,NA,NA,NA,NA),na.string="?")
    #retrieve data only for dates between the start date and end date
    selectedData<-data.frame(eDate[c==TRUE],d[c==TRUE,])
    #open a png device
    png(filename = "plot2.png", width = 480, height = 480, units = "px")    
    #generate the plot
    plot(selectedData$Global_active_power,type='l',xaxt="n",ann=FALSE)
    #add weekdays as the x-axis label
    axis(1,at=seq(1,2880,1439),lab=weekdays(seq(startDate,endDate,by='1 day'),1))
    #set the title and y axis label
    title("Plot 2", ylab="Global Active Power (kilowatts)")
    #turn off the png device
    dev.off()
}