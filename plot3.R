plot3R <- function()
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
    
    png(filename='plot3.png')
    
    nei.baltimore <- NEI %>% filter(fips == "24510") %>% group_by(type, year) %>% summarize(Annual.Total = sum(Emissions));
    nei.baltimore$type <- factor(nei.baltimore$type, levels = c("ON-ROAD", "NON-ROAD", "POINT", "NONPOINT")) # Re-order factor levels so they plot in the order we wish
    ggplot(nei.baltimore, aes(x = factor(year), y = Annual.Total, fill = type)) + 
        geom_bar(stat = "identity") + 
        facet_grid(. ~ type) + 
        xlab("Year") + 
        ylab(expression("Total Tons of PM"[2.5]*" Emissions")) + 
        ggtitle(expression("Total Tons of PM"[2.5]*" Emissions in Baltimore by Source Type")) +
        theme(axis.text.x=element_text(angle = 90, vjust = 0.5, hjust = 1)) +
        scale_y_continuous(labels = comma) +
        guides(fill = FALSE)
    dev.set(which = 2)
    dev.copy(which = 4)
    dev.off()
}