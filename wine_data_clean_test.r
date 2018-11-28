wine_data <- read.csv("C:/Users/Admin/Documents/Bilal - R/Wine_Data_Unclean.csv", stringsAsFactors = F)

wine_region <- regmatches(wine_data$variety_and_region, regexpr("/.+", wine_data$variety_and_region))

region_fixed <- sub("/ ", "", wine_region)
wine_data$Region <- region_fixed

replacement <- sub("/.+", "", wine_data$variety_and_region)
wine_data$variety_and_region <- replacement

wine_region2 <- regmatches(wine_data$Region, regexpr("/.+", wine_data$Region))

region_fixed2 <- sub("/ ","", wine_region2)
wine_data$Sub_Region <- region_fixed2

region_fixed3 <- sub("/.+", "", wine_data$Region)
wine_data$Region <- region_fixed3

colnames(wine_data)[10]<-"Variety"