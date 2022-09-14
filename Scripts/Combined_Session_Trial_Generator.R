#Run the code below in console prior to loading libraries
# It allows to write very large Excel sheets to file
# This script is used to generate traces for specific behavioral timepoints of
# taste trials from a session long recording.


# options(java.parameters = "- Xmx1024m")
#Import Data
library(readr)
library(writexl)
dataset <- read_csv("./datafile.csv")

#Create Data frame
DF <- data.frame(dataset[1:nrow(dataset),2:ncol(dataset)])
Cell_Num <- data.frame(dataset[1:nrow(dataset),1])
colnames(Cell_Num) <-("Cell_num")


#Generate Individual Trial Data Frames
Baseline <- data.frame(DF[,1:2000])
H201 <- data.frame(DF[,1943:2393])
H202 <- data.frame(DF[,4152:4602])
H203 <- data.frame(DF[,8114:8564])
NaCl1 <- data.frame(DF[,9203:9653])
NaCl2 <- data.frame(DF[,16437:16887])
Quinine1 <- data.frame(DF[,7027:7477])
Quinine2 <- data.frame(DF[,20435:20885])
Citric1 <- data.frame(DF[,3031:3481])
Citric2 <- data.frame(DF[,17560:18010])
Shutter1 <- data.frame(DF[,13164:13614])
Shutter2 <- data.frame(DF[,26590:27040])
Sucrose1 <- data.frame(DF[,10289:10739])
Sucrose2 <- data.frame(DF[,21525:21975])
Sacc1 <- data.frame(DF[,14257:14707])
Sacc2 <- data.frame(DF[,23715:24165])
Umami1 <- data.frame(DF[,15345:15795])
Umami2 <- data.frame(DF[,22624:23074])



#Create Dataframe with Trial Number, crop to be same length. For 15 FPS 449 = roughly 5 sec pre, 30 sec post
BT <- cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="B", Baseline[,1:449])
HT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="H", H201[,1:449])
HT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="H", H202[,1:449])
HT3 <-cbind(Cell_num = Cell_Num, Trial_num=3, Tastant="H", H203[,1:449])
CT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="C", Citric1[,1:449])
CT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="C",Citric2[,1:449])
NT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="N", NaCl1[,1:449])
NT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="N", NaCl2[,1:449])
QT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Q", Quinine1[,1:449])
QT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Q", Quinine2[,1:449])
SacT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Sac", Sacc1[,1:449])
SacT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Sac", Sacc2[,1:449])
Shu1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Shu", Shutter1[,1:449])
Shu2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Shu", Shutter2[,1:449])
Suc1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Suc", Sucrose1[,1:449])
Suc2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Suc", Sucrose2[,1:449])
U1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="U", Umami1[,1:449])
U2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="U", Umami2[,1:449])


#Create Dataframe with Trial Number, crop to be same length. For 30 FPS 901 = roughly 5 sec pre, 30 sec post
#BT <- cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="B", Baseline[,1:901])
#HT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="H", H201[,1:901])
#HT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="H", H202[,1:901])
#HT3 <-cbind(Cell_num = Cell_Num, Trial_num=3, Tastant="H", H203[,1:901])
#CT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="C", Citric1[,1:901])
#CT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="C",Citric2[,1:901])
#NT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="N", NaCl1[,1:901])
#NT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="N", NaCl2[,1:901])
#QT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Q", Quinine1[,1:901])
#QT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Q", Quinine2[,1:901])
#SacT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Sac", Sacc1[,1:901])
#SacT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Sac", Sacc2[,1:901])
#Shu1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Shu", Shutter1[,1:901])
#Shu2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Shu", Shutter2[,1:901])
#Suc1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="Suc", Sucrose1[,1:901])
#Suc2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="Suc", Sucrose2[,1:901])
#U1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="U", Umami1[,1:901])
#U2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="U", Umami2[,1:901])

#Rename columns to all match
names(HT1) <- names(BT)
names(HT2) <- names(BT)
names(HT3) <- names(BT)
names(CT1) <- names(BT)
names(CT2) <- names(BT)
names(NT1) <- names(BT)
names(NT2) <- names(BT)
names(QT1) <- names(BT)
names(QT2) <- names(BT)
names(SacT1) <- names(BT)
names(SacT2) <- names(BT)
names(Shu1) <- names(BT)
names(Shu2) <- names(BT)
names(Suc1) <- names(BT)
names(Suc2) <- names(BT)
names(U1) <- names(BT)
names(U2) <- names(BT)


#For Taste Days
AllStim <- rbind(BT,HT1,HT2,HT3,CT1,CT2,NT1,NT2,QT1,QT2,SacT1,SacT2,Shu1,Shu2,Suc1,Suc2,U1,U2)



Final_AllStim <-cbind (Animal="AnimalNum", Day_num = 5, AllStim)

#Write to excel file
write_xlsx(Final_AllStim, "TrialAlignedData.xlsx")



#For Water Training
#Import Data
library(readr)
library(writexl)
dataset <- read_csv("./Data.csv")

#Create Data frame
DF <- data.frame(dataset[1:nrow(dataset),2:ncol(dataset)])
Cell_Num <- data.frame(dataset[1:nrow(dataset),1])
colnames(Cell_Num) <-("Cell_num")

#Generate Individial Trial Data Frames
Baseline <- data.frame(DF[,1:1000])
H201 <- data.frame(DF[,1943:2393])
H202 <- data.frame(DF[,2131:2581])
H203 <- data.frame(DF[,2511:2961])
H204 <- data.frame(DF[,2694:3144])
H205 <- data.frame(DF[,2878:3328])
H206 <- data.frame(DF[,3061:3511])
H207 <- data.frame(DF[,3293:3743])
H208 <- data.frame(DF[,3476:3926])
H209 <- data.frame(DF[,3669:4119])
H2010 <- data.frame(DF[,4846:5296])
H2011 <- data.frame(DF[,5044:5494])
H2012 <- data.frame(DF[,5274:5724])
H2013 <- data.frame(DF[,5482:5932])
H2014 <- data.frame(DF[,5753:6203])
H2015 <- data.frame(DF[,5957:6407])
H2016 <- data.frame(DF[,7932:8382])
#H2017 <- data.frame(DF[,58266:58715])

#Create Dataframe with Trial Number, crop to be same length. For 15 FPS 449 = roughly 5 sec pre, 30 sec post
BT <- cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="B", Baseline[,1:449])
HT1 <-cbind(Cell_num = Cell_Num, Trial_num=1, Tastant="H", H201[,1:449])
HT2 <-cbind(Cell_num = Cell_Num, Trial_num=2, Tastant="H", H202[,1:449])
HT3 <-cbind(Cell_num = Cell_Num, Trial_num=3, Tastant="H", H203[,1:449])
HT4 <-cbind(Cell_num = Cell_Num, Trial_num=4, Tastant="H", H204[,1:449])
HT5 <-cbind(Cell_num = Cell_Num, Trial_num=5, Tastant="H", H205[,1:449])
HT6 <-cbind(Cell_num = Cell_Num, Trial_num=6, Tastant="H", H206[,1:449])
HT7 <-cbind(Cell_num = Cell_Num, Trial_num=7, Tastant="H", H207[,1:449])
HT8 <-cbind(Cell_num = Cell_Num, Trial_num=8, Tastant="H", H208[,1:449])
HT9 <-cbind(Cell_num = Cell_Num, Trial_num=9, Tastant="H", H209[,1:449])
HT10 <-cbind(Cell_num = Cell_Num, Trial_num=10, Tastant="H", H2010[,1:449])
HT11 <-cbind(Cell_num = Cell_Num, Trial_num=11, Tastant="H", H2011[,1:449])
HT12 <-cbind(Cell_num = Cell_Num, Trial_num=12, Tastant="H", H2012[,1:449])
HT13 <-cbind(Cell_num = Cell_Num, Trial_num=13, Tastant="H", H2013[,1:449])
HT14 <-cbind(Cell_num = Cell_Num, Trial_num=14, Tastant="H", H2014[,1:449])
HT15 <-cbind(Cell_num = Cell_Num, Trial_num=15, Tastant="H", H2015[,1:449])
HT16 <-cbind(Cell_num = Cell_Num, Trial_num=16, Tastant="H", H2016[,1:449])
HT17 <-cbind(Cell_num = Cell_Num, Trial_num=17, Tastant="H", H2017[,1:449])

#Rename columns to all match
names(HT1) <- names(BT)
names(HT2) <- names(BT)
names(HT3) <- names(BT)
names(HT4) <- names(BT)
names(HT5) <- names(BT)
names(HT6) <- names(BT)
names(HT7) <- names(BT)
names(HT8) <- names(BT)
names(HT9) <- names(BT)
names(HT10) <- names(BT)
names(HT11) <- names(BT)
names(HT12) <- names(BT)
names(HT13) <- names(BT)
names(HT14) <- names(BT)
names(HT15) <- names(BT)
names(HT16) <- names(BT)
names(HT17) <- names(BT)

#Create Dataframe with all Stimuli
#AllStim <- rbind(BT,HT1,HT2,HT3,HT4,HT5,HT6,HT7,HT8,HT9,HT10,HT11,HT12,HT13,HT14,HT15,HT16,HT17)
AllStim <- rbind(BT,HT1,HT2,HT3,HT4,HT5,HT6,HT7,HT8,HT9,HT10,HT11,HT12,HT13,HT14,HT15,HT16)



Final_AllStim <-cbind (Animal="AnimalNumber", Day_num = 5, AllStim)

#Write to excel file
write_xlsx(Final_AllStim, "TrialAlignedData.xlsx")