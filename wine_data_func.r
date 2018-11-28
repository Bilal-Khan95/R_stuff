table_cleaner <- function(table_location, column_name, new_column_name1, new_column_name2){

  data_table <- read.csv(table_location, stringsAsFactors = F)
  
  clean_vector1 <- regmatches(data_table[,column_name], regexpr("/.+", data_table[,column_name]))
  
  clean_vector1_fixed <- sub("/ ","", clean_vector1)
  data_table[,new_column_name1] <- clean_vector1_fixed
  
 
  if(new_column_name2 != " "){
    clean_vector2 <- regmatches(data_table[,new_column_name1], regexpr("/.+", data_table[,new_column_name1]))
    clean_vector2_fixed <- sub("/ ","", clean_vector2)
    data_table[,new_column_name2] <- vec_region2_clean
  }
  
  clean_vector3 <- sub("/.+", "", data_table[,new_column_name1])
  data_table[,new_column_name1] <- clean_vector3
  
  
  replacement <- sub("/.+", "", data_table[,column_name])
  data_table[,column_name] <- replacement
  
  return(data_table)
}

#table_location <- readline(prompt ="Enter full file path:" )
table_location <- "C:/Users/Admin/Documents/data_table.csv"

data <- table_cleaner(table_location, "variety_and_region", "Region", "Sub-Region")

write.csv(data, "Wine_Data_Clean.csv")
