## Plot for Question n.2

# Reading the data from the source files
# Files should be in the working directory
# Due to file size it might take a few seconds. Patience ;-)
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")





# The actual plot
# Opening the device
png("plot2.png", width = 480, height = 480)
# Plotting the 



# Closing off the device
dev.off()
