plot4R <- function()
{
    #importing the library
    library(dplyr)
    library(ggplot2)
    library(scales)
    library(data.table)
    library("stringr");
    
    if( str_detect(getwd(), "exdata_data_NEI_data") ){
        print('In the right directory');
    } else {
        setwd(paste(getwd(),"/exdata_data_NEI_data/",sep=""))
    }
    
    #it is assume that the file is already exist
    print(getwd());
    
    # Load the NEI & SCC data frames.
    SCC <- readRDS("Source_Classification_Code.rds")
    NEI <- readRDS("summarySCC_PM25.rds") 
    
    png(filename='plot4.png')
    
    coal_SCC <- SCC[grep("[Cc][Oo][Aa][Ll]", SCC$EI.Sector), "SCC"]
    coal_NEI <- NEI %>% filter(SCC %in% coal_SCC)
    coal_summary <- coal_NEI %>% group_by(year) %>% summarise(Emissions = sum(Emissions))
    c_plot <- ggplot(coal_summary, aes(x=year, y=round(Emissions/1000,2), label=round(Emissions/1000,2), fill=year)) +
        geom_bar(stat="identity") + ylab(expression('PM'[2.5]*' Emissions in Kilotons')) + xlab("Year") +
        geom_label(aes(fill = year),colour = "white", fontface = "bold") +
        ggtitle("Coal Combustion Emissions, 1999 to 2008.")
    print(c_plot)
    
    dev.set(which = 2)
    dev.copy(which = 4)
    dev.off()
}