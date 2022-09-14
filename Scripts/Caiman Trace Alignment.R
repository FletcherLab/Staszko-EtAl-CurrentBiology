library(readxl); library(xlsx); library(tidyverse); library(readr); library(beepr)#read in library readxl, if you have it


#concatenate sheets from multiple files====
Sheet_Merge <- function(Sheetz = "Neuron.C_raw", #the name of the sheet you want to pull from
                        Folder = "./", #the path to the folder where all your .xlsx sheets live
                        Merge_Name = "Merged_C_Raw_file"){ #name you want for the result file
  Xnam <- gsub(pattern = ".xlsx", replacement = "", x = list.files(path = Folder, pattern = ".xlsx", full.names = F)) #pull a list of just the names of the .xlsx file in the folder
  Xpath <- list.files(path = Folder, pattern = ".xlsx", full.names = T) #pull a list of full paths to .xlsx files
  for (i in 1:length(Xpath)){ #start a loop to read in the target sheets
    assign(x = Xnam[i], value = read_excel(path = Xpath[i], sheet = Sheetz, col_names = F)) #read the sheets and write them to the environment
  }
  
  WB_Name <- paste(Folder, Merge_Name, ".xlsx", sep = "") #Create the name of the workbook
  TempWb <- createWorkbook(type = "xlsx") #Create the temporary WB
  for (i in Xnam){ #Loop through your sheets, writing them to WB
    assign(x = "TempSheet",value = createSheet(wb = TempWb, sheetName = i)) #create a new sheet in the WB
    addDataFrame(x = as.data.frame(eval(parse(text = i))), sheet = TempSheet, col.names = F, row.names = F) #write data to it
  }
  saveWorkbook(wb = TempWb, file = WB_Name) #save your work
}

#Sheet_Merge(Folder = "./", Merge_Name = "Merged_C_Raw_File") #POC call to the function

#Read and sort data from CellReg ====
#assuming it is in the format that I got some stuff from Max
Trace_Master = "./Animal_Traces.csv"                         #Path to the trials sheets
Cell_Master = "./Animal_CellReg.xlsx"  #Path to the CellReg legend for the day you are aligning
                                           #how many datapoints do you have/want?


Sheet1 = read.csv(Trace_Master) #first, use SheetSplit to read in your cells and trial
Frame_N = ncol(Sheet1)  
Sheetz <- "Sheet1"
Cell_ID <- read_excel(Cell_Master, col_names = F) #read in the cell IDs from CellReg(?) 
Cell_ID <- Cell_ID[,Cell_ID %>% t() %>% complete.cases()] #Cut the sheet to just the cell IDs
colnames(Cell_ID) <- Sheetz #Rename the columns

#Find out where the cells live
Cells <- matrix(data = 0, nrow = max(Cell_ID), ncol = ncol(Cell_ID)) #create a matrix to load the cell locations in
for (i in 1:ncol(Cell_ID)){            #Start a loop to check each column (trial) of CellID
  for (ii in 1:max(Cell_ID)){          #Subloop looking for each cell
    A <- which(x = (Cell_ID[,i]==ii))  #Pull the coordinates for each cell ID
    if(length(A) != 0){                #Check that you actually have a cell
      Cells[ii,i] <- A}                #Write to the matrix
  }}



#Reorder the cells, create blanks
for (i in 1:length(Sheetz)){ #Start a loop to write each sheet
  TempMat <- matrix(data = 0, nrow = nrow(Cell_ID), ncol = Frame_N) #load up a temporary matrix to dump the cells into
  for (ii in 1:length(Cells[,i])) { #start a subloop to pull the traces from their coordinates
    RowN <- Cells[ii,i] #Identify the coordinate
    if (RowN != 0){ #make sure it's there
      TempMat[RowN,] <- as.vector(x = eval(parse(text = Sheetz[i]))[ii,1:Frame_N], mode = "numeric") #Pull the indicated vector and write it to the tempmat
    }
  }
  assign(x = paste(Sheetz[i], "_Aligned", sep = ""), value = as.data.frame(TempMat)) #write the data from tempmat to the environment
}
Sheetz_Mat <- paste(Sheetz, "_Aligned", sep = "") #make a list of the new DF names

#write some permanent files
for (i in Sheetz_Mat){
  write.csv(x = eval(parse(text = i)), file = paste(i, ".csv", sep = ""))
}


beep(2)
