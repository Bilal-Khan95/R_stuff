# Extract titles from column, separate out into their own columns and remove old column
# Change filepath as needed

data <- read.csv("C:/Users/tknow/Desktop/R Course/Data Cleaning Exercise/Wine_Data_Unclean.csv")

# Remove extra ID columns

data$X <- NULL
data$X.1 <- NULL

# Define Regex for pattern matching

regex <- "(.*?)/"

# Add a '/' to the end of the variable, to make Regex-ing easier

data$variety_and_region <- paste(data$variety_and_region, "/")

# Process each column:
#  + Locate matches
#  + Add matches to a new column (defined on function call)
#  + Replace matched substring with blanks

extract_variable <- function(x) {
  
  matches <- regexpr(regex,data$variety_and_region)
  
  matches_list <- regmatches(data$variety_and_region,matches)
  
  data[x] <- matches_list
  
  data$variety_and_region <- sub(regex, "", data$variety_and_region)
  
  return(data)

}

# Run once for each title type
# eg. 3 Titles = 3 function calls, each with a different String parameter (header)

data <- extract_variable('variety')
data <- extract_variable('region')
data <- extract_variable('sub_region')

# Remove original (empty) column

data$variety_and_region <- NULL

# Remove '/' from each column

data$variety <- gsub("/","",data$variety)
data$region <- gsub("/","",data$region)
data$sub_region <- gsub("/","",data$sub_region)

# Add desired output filepath
write.csv("")
