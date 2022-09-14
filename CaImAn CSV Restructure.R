library(readr)
library(writexl)
dataset <- read_table2("FM07_2_6_spatialDS_traces.csv", col_names = FALSE)

write.table(dataset, "FM07_2_6_traces_noheader.csv", sep=",", col.names = FALSE, row.names = FALSE)




