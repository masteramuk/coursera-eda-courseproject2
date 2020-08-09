plot1R <- function()
{
    #importing the library
    library("data.table")
    setwd(paste(getwd(),"/exdata_data_NEI_data/",sep=""))
          
    #it is assume that the file is already exist
    print(getwd());
    SCC <- data.table::as.data.table(x = readRDS(file = "Source_Classification_Code.rds"));
    NEI <- data.table::as.data.table(x = readRDS(file = "summarySCC_PM25.rds"));
    NEI[, Emissions := lapply(.SD, as.numeric), .SDcols = c("Emissions")];
    
    totalNEI <- NEI[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("Emissions"), by = year]
    
    png(filename='plot1.png')
    
    barplot(totalNEI[, Emissions]
            , names = totalNEI[, year]
            , xlab = "Years", ylab = "Emissions"
            , main = "Emissions over the Years")
    
    dev.off()
}