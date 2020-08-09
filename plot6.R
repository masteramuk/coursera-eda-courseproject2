plot6R <- function()
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
    
    
    fips_lookup <- data.frame(fips = c("06037", "24510"), county = c("Los Angeles", "Baltimore"))
    
    vehicles_SCC <- SCC[grep("[Vv]ehicle", SCC$EI.Sector), "SCC"]
    vehicle_emissions <- NEI %>%
        filter(SCC %in% vehicles_SCC & fips %in% fips_lookup$fips) %>%
        group_by(fips, year) %>%
        summarize(Emissions = sum(Emissions))
    
    vehicle_emissions <- merge(vehicle_emissions, fips_lookup)
    
    png(filename="plot6.png")
    
    plot6 <- ggplot(vehicle_emissions, aes(x = factor(year), y = round(Emissions/1000, 2), 
                                              label=round(Emissions/1000,2), fill = year)) +
        geom_bar(stat = "identity") + facet_grid(. ~ county) +
        ylab(expression('PM'[2.5]*' Emissions in Kilotons')) + xlab("Year") +
        geom_label(aes(fill = year),colour = "white", fontface = "bold") +
        ggtitle("Los Angeles vs Baltimore Vehicle Emissions.")
    print(plot6)
    
    dev.set(which = 2)
    dev.copy(which = 4)
    dev.off()
}