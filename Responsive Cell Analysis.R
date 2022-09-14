#For Aligning Without Zeros

# libraries
library(fpc)
library(mclust)
library(fastICA)
library(RColorBrewer)
library(gplots)
library(zoo)
library(ggplot2)
library(tidyverse)
#library(openxlsx)
library(writexl)
library(beepr)
library(plyr)

# read data file
df.out <- read.table("./pathID6.txt",header=FALSE) #path to pathID input file
n.files<-length(df.out$V1)

#import and name data frames for each file
lhs<-paste("df.GC",df.out$V1,sep="")
rhs<-paste("read.csv(levels(df.out$V2)[",1:n.files,"])",sep="")
eq<-paste(paste(lhs,rhs,sep="<-"),collapse=";")
eval(parse(text=eq))

#initialize variables for tt/raw sig
bit<-numeric(n.files)
bite<-numeric(n.files)
bitten<-numeric(n.files)
biter<-numeric(n.files)
bites<-numeric(n.files)
biting<-numeric(n.files)
eat<-numeric(n.files)
ate<-numeric(n.files)
eating<-numeric(n.files)
r<-numeric(n.files)
mint<-numeric(n.files)
maxt<-numeric(n.files)
a<-numeric(n.files)

#determine tt for each data frame and create raw sig
for (d in 1:n.files){
  bit[d]<-paste("df.GC",df.out$V1[d],sep="")
  frm<-eval(parse(text=bit[d]))
  tcols<-dim(frm)[2]
  #extracting time from column title
  timecols<-names(frm[6:tcols])
  c.timecols<-length(timecols)
  tt<-numeric(c.timecols)
  for (j in 1:c.timecols){
    l<-nchar(timecols[j])
    tt[j]<-as.numeric(substr(timecols[j],2,l))
  }
  lzs<-paste("tt",df.out$V1[d],sep="")
  assign(lzs,tt)
  f.cell<-as.factor(frm$Cell_num)
  c.all<-length(f.cell)  #total number of traces
  nam<-paste("c.all",df.out$V1[d],sep="")
  assign(nam,c.all)
  sig<-as.matrix(frm[,6:tcols])
  lx<-paste("sig",df.out$V1[d],sep="")
  assign(lx,sig)
  mint[d]<-min(tt)
  maxt[d]<-max(tt)
}
#b=time interval of the fastest frame rate
#a=maximum time of the df to be interpolated


for (d in 1:n.files){
  bite[d]<-paste("sig",df.out$V1[d],sep="")
  frame<-eval(parse(text=bite[d]))
  
  bitten[d]<-paste("tt",df.out$V1[d],sep="")
  framed<-eval(parse(text=bitten[d]))
  
  biter[d]<-paste("c.all",df.out$V1[d],sep="")
  framer<-eval(parse(text=biter[d]))
  
  b<-min(mint)
  a<-maxt[d]
  c<-min(maxt)
  ts<-length(which(tt<c))
  ttx<-seq(b,a,b)
  xx<-length(ttx)
  sigx<-numeric(framer*xx)
  dim(sigx)<-c(framer,xx)
  for (j in 1:framer) {
    sigx[j,]<-spline(framed,frame[j,],xout=ttx)$y
  }
  lxs<-paste("sigx",df.out$V1[d],sep="")
  assign(lxs,sigx)
  
} # end d

for (d in 1:n.files){
  bit[d]<-paste("df.GC",df.out$V1[d],sep="")
  frm<-eval(parse(text=bit[d]))
  
  bites[d]<-paste("sigx",df.out$V1[d],sep="")
  frame<-eval(parse(text=bites[d]))
  
  alpha<-as.data.frame(frm[,1:5])
  beta<-as.data.frame(frame)
  left<-paste("df",df.out$V1[d],sep="")
  eps<-cbind(alpha,beta)
  assign(left,eps)
  
  biting[d]<-paste("df",df.out$V1[d],sep="")
  framing<-eval(parse(text=biting[d]))
  
} # end d


#GUST=combined matrix of all cells and tastes
GUST<-rbind(df.GCLM01[,1:255]) #data frame should be df.GC followed by AnimalName used in pathID file, in this case Animal Name is LM01
#GUST dataframe is cropped based on frame rate and number of frames put into trial alignment to reduce data size

# define factor variables, etc
f.anml<-as.factor(GUST$Animal)
f.day<-as.factor(GUST$Day_num)
f.cell<-as.factor(GUST$Cell_num)
f.trial<-as.factor(GUST$Trial_num)
f.taste<-as.factor(GUST$Tastant)


# constants
c.all<-length(f.anml)  #total number of traces
sig<-as.matrix(GUST[,6:255]) #make matrix of traces without descriptive info
t.base<-5.0 #time baseline
t.resp<-10.0 #time response
sigcols<-dim(sig)[2]

sf<-0.066667 #frame rate/time per frame. i.e. 5FPS is 0.2 seconds per frame
tGUST<-ttx[225] #time
t<-seq(sf,tGUST,sf)
smooth<-numeric(c.all)
list<-numeric(c.all)
b<-31#denotes number of columns in baseline period
base<-numeric(c.all*b)
dim(base)<-c(c.all,b)
SD<-numeric(c.all)
thresh1<-numeric(c.all)
thresh2<-numeric(c.all)
thresh3<-numeric(c.all)
thresh4<-numeric(c.all)
thresh_decrease1<-numeric(c.all)
thresh_decrease2<-numeric(c.all)
thresh_decrease3<-numeric(c.all)
thresh_decrease4<-numeric(c.all)

#sig_smooth=smoothing over just taste time, change columns for frames responding to stimulus time
num<-length(as.numeric(rollmean(sig[1,75:150],3))) #traces were cropped to 5 seconds pre, 5 seconds taste, 5 seconds post at 15
sig_smooth<-numeric(c.all*num)
dim(sig_smooth)<-c(c.all,num)
for (j in 1:c.all){
  sig_smooth[j,]<-rollmean(sig[j,75:150],3)
}

#smooth across entire trace
#sig_smooth_all=smoothed for entire length
num<-length(as.numeric(rollmean(sig[1,1:225],3)))
sig_smooth_all<-numeric(c.all*num)
dim(sig_smooth_all)<-c(c.all,num)
for (j in 1:c.all){
  sig_smooth_all[j,]<-rollmean(sig[j,1:225],3)
}

f<-numeric(c.all)
f_decrease <- numeric(c.all)
#setting SD thresholds


sig_smooth_all_baseline <- sig_smooth_all[,45:75]
baseline_mean<-numeric(c.all)

for (j in 1:c.all){
  baseline_mean[j]<-mean(sig_smooth_all_baseline[j,])
}

#setting SD thresholds using original GC script: finds numbers that are 2, 2.5, 3, 3.5 SD above the mean of 
#smoothed responses occuring during baseline (period prior to t.base)
for (j in 1:c.all){
  f[j]<-baseline_mean[j]
  base[j,]<-sig_smooth_all_baseline[j,]
  SD[j]<-sd(base[j,])
  thresh1[j]<-((2*SD[j])+f[j])
  thresh2[j]<-((2.5*SD[j])+f[j])
  thresh3[j]<-((3*SD[j])+f[j])
  thresh4[j]<-((3.5*SD[j])+f[j])
}

#setting decreased SD threshold
for (j in 1:c.all){
  f_decrease[j]<-baseline_mean[j]
  base[j,]<-sig_smooth_all_baseline[j,]
  SD[j]<-sd(base[j,])
  thresh_decrease1[j]<-((f_decrease[j]-2*SD[j]))
  thresh_decrease2[j]<-((f_decrease[j]-2.5*SD[j]))
  thresh_decrease3[j]<-((f_decrease[j]-3*SD[j]))
  thresh_decrease3[j]<-((f_decrease[j]-3.5*SD[j]))
}

#generate dataframe of enhanced responses only
# define factor variables, etc
c.all3<-nrow(GUST)
f.anml3<-as.factor(GUST$Animal)
f.day3<-as.factor(GUST$Day_num)
f.cell3<-as.factor(GUST$Cell_num)
f.trial3<-as.factor(GUST$Trial_num)
f.taste3<-as.factor(GUST$Tastant)

sig_delta_smooth_alldata_responders<-numeric(c.all3*num)
dim(sig_delta_smooth_alldata_responders)<-c(c.all3,num)
f.taste3<-as.factor(GUST$Tastant)
f.IDstar3<-numeric(length=c.all3)
f.cellIDstar3<-unique(cbind(f.anml3, f.day3, f.trial3, f.cell3))
n.cellIDstar3<-length(f.cellIDstar3[,1])

#find maximum response (single point), time of maximum response, and column # of maximum resp during taste
sig_smooth_sub<-sig_smooth_all[,75:150]
t.max<-numeric(c.all3)
t_sig<-t[75:150]
for (j in 1:c.all){
  smooth[j]<-max(sig_smooth_sub[j,])
  t.max[j]<-subset(t_sig,smooth[j]==sig_smooth_sub[j,])
  list<-apply(sig_smooth_sub,1,which.max)
}

list2<-apply(sig_smooth_all[,75:150],1,which.max)


#make a list with the maximum response value (single point) during taste

tastemax <- apply(sig_smooth_sub, 1, max)

# loop to find unique cell IDs
for (j in 1:c.all3){
  ID<-c(f.anml[j], f.day[j], f.trial[j],f.cell[j])
  for (k in 1:n.cellIDstar3){
    if(f.cellIDstar3[k,1]==ID[1] &
       f.cellIDstar3[k,2]==ID[2] &
       f.cellIDstar3[k,3]==ID[3] &
       f.cellIDstar3[k,4]==ID[4]) f.IDstar3[j]<-k
  } # end k
} # end j

#find the average of the maximum resp +/- one second, 15 FPS
max_resp_taste<-numeric(c.all3)
for (j in 1:c.all3){
  if (list2[j]==min(list2)|list2[j]==min(list2)+1|list2[j]==min(list2)+2|list2[j]==min(list2)+3|list2[j]==min(list2)+4|list2[j]==min(list2)+5|list2[j]==min(list2)+6|list2[j]==min(list2)+7|list2[j]==min(list2)+8|list2[j]==min(list2)+9|list2[j]==min(list2)+10|list2[j]==min(list2)+11|list2[j]==min(list2)+12|list2[j]==min(list2)+13|list2[j]==min(list2)+14|list2[j]==min(list2)+15) {
      max_resp_taste[j]<-((sig_smooth_sub[j,list2[j]] + sig_smooth_sub[j,(list2[j]+1)] + sig_smooth_sub[j,(list2[j]+2)] + sig_smooth_sub[j,(list2[j]+3)]+ sig_smooth_sub[j,(list2[j]+4)]+ sig_smooth_sub[j,(list2[j]+5)]+ sig_smooth_sub[j,(list2[j]+6)]+ sig_smooth_sub[j,(list2[j]+7)]+ sig_smooth_sub[j,(list2[j]+8)]+ sig_smooth_sub[j,(list2[j]+9)]+ sig_smooth_sub[j,(list2[j]+10)]+ sig_smooth_sub[j,(list2[j]+11)]+ sig_smooth_sub[j,(list2[j]+12)]+ sig_smooth_sub[j,(list2[j]+13)]+ sig_smooth_sub[j,(list2[j]+14)]+ sig_smooth_sub[j,(list2[j]+15)])/16)
  } else if (list2[j]==max(list2)|list2[j]==max(list2)-1|list2[j]==max(list2)-2|list2[j]==max(list2)-3|list2[j]==max(list2)-4|list2[j]==max(list2)-5|list2[j]==max(list2)-6|list2[j]==max(list2)-7|list2[j]==max(list2)-8|list2[j]==max(list2)-9|list2[j]==max(list2)-10|list2[j]==max(list2)-11|list2[j]==max(list2)-12|list2[j]==max(list2)-13|list2[j]==max(list2)-14|list2[j]==max(list2)-15) {
      max_resp_taste[j]<-((sig_smooth_sub[j,list2[j]] + sig_smooth_sub[j,(list2[j]-1)] + sig_smooth_sub[j,(list2[j]-2)] + sig_smooth_sub[j,(list2[j]-3)] + sig_smooth_sub[j,(list2[j]-4)] + sig_smooth_sub[j,(list2[j]-5)] + sig_smooth_sub[j,(list2[j]-6)] + sig_smooth_sub[j,(list2[j]-7)]+ sig_smooth_sub[j,(list2[j]-8)]+ sig_smooth_sub[j,(list2[j]-9)]+ sig_smooth_sub[j,(list2[j]-10)]+ sig_smooth_sub[j,(list2[j]-11)]+ sig_smooth_sub[j,(list2[j]-12)]+ sig_smooth_sub[j,(list2[j]-13)]+ sig_smooth_sub[j,(list2[j]-14)]+ sig_smooth_sub[j,(list2[j]-15)])/16)
  } else {
    max_resp_taste[j]<-((sig_smooth_sub[j,list2[j]] + sig_smooth_sub[j,(list2[j]+1)] + sig_smooth_sub[j,(list2[j]-1)] + sig_smooth_sub[j,(list2[j]+2)] + sig_smooth_sub[j,(list2[j]-2)] + sig_smooth_sub[j,(list2[j]+3)] + sig_smooth_sub[j,(list2[j]-3)]+ sig_smooth_sub[j,(list2[j]+4)] + sig_smooth_sub[j,(list2[j]-4)]+ sig_smooth_sub[j,(list2[j]+5)] + sig_smooth_sub[j,(list2[j]-5)]+ sig_smooth_sub[j,(list2[j]+6)] + sig_smooth_sub[j,(list2[j]-6)]+ sig_smooth_sub[j,(list2[j]+7)] + sig_smooth_sub[j,(list2[j]-7)]+ sig_smooth_sub[j,(list2[j]+8)] + sig_smooth_sub[j,(list2[j]-8)]+ sig_smooth_sub[j,(list2[j]+9)] + sig_smooth_sub[j,(list2[j]-9)]
                         + sig_smooth_sub[j,(list2[j]+10)] + sig_smooth_sub[j,(list2[j]-10)]+ sig_smooth_sub[j,(list2[j]+11)] + sig_smooth_sub[j,(list2[j]-11)]+ sig_smooth_sub[j,(list2[j]+12)] + sig_smooth_sub[j,(list2[j]-12)]+ sig_smooth_sub[j,(list2[j]+13)] + sig_smooth_sub[j,(list2[j]-13)]+ sig_smooth_sub[j,(list2[j]+14)] + sig_smooth_sub[j,(list2[j]-14)]+ sig_smooth_sub[j,(list2[j]+15)] + sig_smooth_sub[j,(list2[j]-15)])/31)
  }
} # end j

for (j in 1:c.all3){
  if (tastemax[j]!=0) {
    sig_delta_smooth_alldata_responders[j]<-((max_resp_taste[j]-baseline_mean[j]))
  } else {
    sig_delta_smooth_alldata_responders[j]<-0
  }
} # end j

#Find all significant responses 

t.all<-2.5 #set threshold
tk<-numeric(t.all)
#MAT=maximum above threshold
MAT5<-numeric(c.all3)
for (k in 1:t.all){
  tk[k]<-paste("thresh",k,sep="")
  TK<-eval(parse(text=tk[k]))
  for (j in 1:c.all3){
    #sig_resp[j,]<-sig[j,which(t.base>t)]
    #FAT[j]<-length(which(sig_resp[j,]>TK[j]))
    MAT5[j]<-ifelse(max_resp_taste[j]>TK[j],1,0)
    test4<-paste("MAT",k,sep="")
    assign(test4,MAT5)
  }
}



#making non threshold responses. responses 0
for (j in 1:c.all3){
  if (MAT5[j]>0) {
    sig_delta_smooth_alldata_responders[j]<-sig_delta_smooth_alldata_responders[j]
  } else {
    sig_delta_smooth_alldata_responders[j]<-0
  }
} # end j


#making  deltaF below 1 responses 0
for (j in 1:c.all3){
  if (sig_delta_smooth_alldata_responders[j]>1) {
    sig_delta_smooth_alldata_responders[j]<-sig_delta_smooth_alldata_responders[j]
  } else {
    sig_delta_smooth_alldata_responders[j]<-0
  }
} # end j


#make table of neuron responses
responses_final_alldata_responders<-as.data.frame(sig_delta_smooth_alldata_responders[,1])
responses_final_alldata3<-sig_delta_smooth_alldata_responders[,1]
table_responders<-tapply(responses_final_alldata3,list(f.IDstar3,f.taste3),mean)
df.responders<-data.frame(table_responders)
colnames(f.cellIDstar3) <- c("Animal", "f.day","f.trial","f.cell")
Q_responders<- cbind(f.cellIDstar3,df.responders)

Q <- Q_responders[,5:13]

m.best<-numeric(length(row.names(Q)))
cell<-length(m.best)


# find tastant with max response per cellID, all stimuli
for (j in 1:cell){
  best<-apply(Q,1,which.max)
  m.best[j]<-Q[j,best[j]]
}

#resp_cell_count <- nrow(tasteresp)

best.df <- as.data.frame(best)
best.df.info <-cbind(f.cellIDstar3,best.df)
best.df <- best.df.info


tastecols <- c(2,4,5,8)
tastecols_full <-c(2,4,5,6,8,9)

Q_all_taste <-as.data.frame(Q[,tastecols_full])
Q_basic_taste <-as.data.frame(Q[,tastecols])
Q2 <-as.data.frame(Q[,tastecols_full])
Q3 <-as.data.frame(Q[,tastecols])
colnames(Q)<-c("B", "C", "H", "N", "Q", "Sac", "Shu", "Suc", "U")
colnames2 <- c("C","N","Q","Sac","Suc","U")
colnames3 <- c("C","N","Q","Suc")
colnames(Q_basic_taste) <- colnames3


colnames(f.cellIDstar3) <- c("f.anml", "f.day","f.trial","f.cell")
Q.info <-cbind(f.cellIDstar3,Q)
Q.allstim.df <-Q.info

Q2.info <-cbind(f.cellIDstar3,Q2)
Q.alltaste.df <-Q2.info
Q2.df <- as.data.frame(Q2.info)

Q3.info <-cbind(f.cellIDstar3,Q3)
Q.basictaste.df <-Q3.info
Q3.df <- as.data.frame(Q3.info)



#N/S calculation = Qbest/Qsecondbest

#norm Q to max of best responses, All stimuli

cell<-length(m.best)
Qnorm<-numeric(cell*9)
dim(Qnorm)<-c(cell,9)


for (j in 1:cell){
  Qnorm[j,1]<-Q[j,1]/m.best[j]
  Qnorm[j,2]<-Q[j,2]/m.best[j]
  Qnorm[j,3]<-Q[j,3]/m.best[j]
  Qnorm[j,4]<-Q[j,4]/m.best[j]
  Qnorm[j,5]<-Q[j,5]/m.best[j]
  Qnorm[j,6]<-Q[j,6]/m.best[j]
  Qnorm[j,7]<-Q[j,7]/m.best[j]
  Qnorm[j,8]<-Q[j,8]/m.best[j]
  Qnorm[j,9]<-Q[j,9]/m.best[j]
}

colnames(Qnorm)<-colnames(Q)
Qnorm.info <-cbind(f.cellIDstar3,Qnorm)
Qnorm_allstim <-Qnorm.info

################################################################################################


#find cells present in taste days, find cells specific to each day for all stim
Q_tastedays_allstim_D1 <- Q.allstim.df[(Q.allstim.df$f.day==1),]
Q_tastedays_allstim_D3 <- Q.allstim.df[(Q.allstim.df$f.day==3),]
Q_tastedays_allstim_D5 <- Q.allstim.df[(Q.allstim.df$f.day==5),]

cellmatch_tastedays_allstim <- match_df(Q_tastedays_allstim_D1, Q_tastedays_allstim_D3, on="f.cell")
cellmatch_tastedays_allstimfinal <- match_df(cellmatch_tastedays_allstim, Q_tastedays_allstim_D5, on="f.cell")

Q_tastedays_allstim_tracked <- rbind(cellmatch_tastedays_allstimfinal, Q_tastedays_allstim_D1[Q_tastedays_allstim_D1$f.cell %in% cellmatch_tastedays_allstimfinal$f.cell,],
                                     Q_tastedays_allstim_D3[Q_tastedays_allstim_D3$f.cell %in% cellmatch_tastedays_allstimfinal$f.cell,], Q_tastedays_allstim_D5[Q_tastedays_allstim_D5$f.cell %in% cellmatch_tastedays_allstimfinal$f.cell,])

Q_tastedays_allstim_D1_Novela <- rbind(Q_tastedays_allstim_tracked,Q_tastedays_allstim_D3,Q_tastedays_allstim_D5)
Q_tastedays_allstim_D3_Novela <- rbind(Q_tastedays_allstim_tracked,Q_tastedays_allstim_D1,Q_tastedays_allstim_D5)
Q_tastedays_allstim_D5_Novela <- rbind(Q_tastedays_allstim_tracked,Q_tastedays_allstim_D3,Q_tastedays_allstim_D1)

Q_tastedays_allstim_D1_novel <- Q_tastedays_allstim_D1[!Q_tastedays_allstim_D1$f.cell %in% Q_tastedays_allstim_D1_Novela$f.cell,]
Q_tastedays_allstim_D3_novel <- Q_tastedays_allstim_D3[!Q_tastedays_allstim_D3$f.cell %in% Q_tastedays_allstim_D3_Novela$f.cell,]
Q_tastedays_allstim_D5_novel <- Q_tastedays_allstim_D5[!Q_tastedays_allstim_D5$f.cell %in% Q_tastedays_allstim_D5_Novela$f.cell,]

Q_tastedays_allstim_novel <- rbind(Q_tastedays_allstim_D1_novel, 
                                   Q_tastedays_allstim_D3_novel,
                                   Q_tastedays_allstim_D5_novel)

Tracked_and_novel_tastedays_allstim <- rbind(Q_tastedays_allstim_tracked,Q_tastedays_allstim_novel)

Q_tastedays_allstim_D1_shared <- Q_tastedays_allstim_D1[!Q_tastedays_allstim_D1$f.cell %in% Tracked_and_novel_tastedays_allstim$f.cell,]
Q_tastedays_allstim_D3_shared <- Q_tastedays_allstim_D3[!Q_tastedays_allstim_D3$f.cell %in% Tracked_and_novel_tastedays_allstim$f.cell,]
Q_tastedays_allstim_D5_shared <- Q_tastedays_allstim_D5[!Q_tastedays_allstim_D5$f.cell %in% Tracked_and_novel_tastedays_allstim$f.cell,]

Q_tastedays_allstim_shared <- rbind(Q_tastedays_allstim_D1_shared, 
                                    Q_tastedays_allstim_D3_shared, 
                                    Q_tastedays_allstim_D5_shared)

best_tracked_tastedays_allstim <- best.df[best.df$f.cell %in% Q_tastedays_allstim_tracked$f.cell,]
best_new_tastedays_allstim <- best.df[best.df$f.cell %in% Q_tastedays_allstim_novel,]
best_shared_tastedays_allstim <- best.df[best.df$f.cell %in% Q_tastedays_allstim_shared]


#find cells present in taste days, find cells specific to each day for all taste
Q_tastedays_alltaste_D1 <- Q.alltaste.df[(Q.alltaste.df$f.day==1),]
Q_tastedays_alltaste_D3 <- Q.alltaste.df[(Q.alltaste.df$f.day==3),]
Q_tastedays_alltaste_D5 <- Q.alltaste.df[(Q.alltaste.df$f.day==5),]

cellmatch_tastedays_alltaste <- match_df(Q_tastedays_alltaste_D1, Q_tastedays_alltaste_D3, on="f.cell")
cellmatch_tastedays_alltastefinal <- match_df(cellmatch_tastedays_alltaste, Q_tastedays_alltaste_D5, on="f.cell")

Q_tastedays_alltaste_tracked <- rbind(cellmatch_tastedays_alltastefinal, Q_tastedays_alltaste_D1[Q_tastedays_alltaste_D1$f.cell %in% cellmatch_tastedays_alltastefinal$f.cell,],
                                      Q_tastedays_alltaste_D3[Q_tastedays_alltaste_D3$f.cell %in% cellmatch_tastedays_alltastefinal$f.cell,], Q_tastedays_alltaste_D5[Q_tastedays_alltaste_D5$f.cell %in% cellmatch_tastedays_alltastefinal$f.cell,])

Q_tastedays_alltaste_D1_Novela <- rbind(Q_tastedays_alltaste_tracked,Q_tastedays_alltaste_D3,Q_tastedays_alltaste_D5)
Q_tastedays_alltaste_D3_Novela <- rbind(Q_tastedays_alltaste_tracked,Q_tastedays_alltaste_D1,Q_tastedays_alltaste_D5)
Q_tastedays_alltaste_D5_Novela <- rbind(Q_tastedays_alltaste_tracked,Q_tastedays_alltaste_D3,Q_tastedays_alltaste_D1)

Q_tastedays_alltaste_D1_novel <- Q_tastedays_alltaste_D1[!Q_tastedays_alltaste_D1$f.cell %in% Q_tastedays_alltaste_D1_Novela$f.cell,]
Q_tastedays_alltaste_D3_novel <- Q_tastedays_alltaste_D3[!Q_tastedays_alltaste_D3$f.cell %in% Q_tastedays_alltaste_D3_Novela$f.cell,]
Q_tastedays_alltaste_D5_novel <- Q_tastedays_alltaste_D5[!Q_tastedays_alltaste_D5$f.cell %in% Q_tastedays_alltaste_D5_Novela$f.cell,]

Q_tastedays_alltaste_novel <- rbind(Q_tastedays_alltaste_D1_novel, 
                                    Q_tastedays_alltaste_D3_novel,
                                    Q_tastedays_alltaste_D5_novel)

Tracked_and_novel_tastedays_alltaste <- rbind(Q_tastedays_alltaste_tracked,Q_tastedays_alltaste_novel)

Q_tastedays_alltaste_D1_shared <- Q_tastedays_alltaste_D1[!Q_tastedays_alltaste_D1$f.cell %in% Tracked_and_novel_tastedays_alltaste$f.cell,]
Q_tastedays_alltaste_D3_shared <- Q_tastedays_alltaste_D3[!Q_tastedays_alltaste_D3$f.cell %in% Tracked_and_novel_tastedays_alltaste$f.cell,]
Q_tastedays_alltaste_D5_shared <- Q_tastedays_alltaste_D5[!Q_tastedays_alltaste_D5$f.cell %in% Tracked_and_novel_tastedays_alltaste$f.cell,]

Q_tastedays_alltaste_shared <- rbind(Q_tastedays_alltaste_D1_shared, 
                                     Q_tastedays_alltaste_D3_shared, 
                                     Q_tastedays_alltaste_D5_shared)

best_tracked_tastedays_alltaste <- best.df[best.df$f.cell %in% Q_tastedays_alltaste_tracked$f.cell,]
best_new_tastedays_alltaste <- best.df[best.df$f.cell %in% Q_tastedays_alltaste_novel,]
best_shared_tastedays_alltaste <- best.df[best.df$f.cell %in% Q_tastedays_alltaste_shared]

#find cells present in days taste days, find cells specific to each day for basic taste
Q_tastedays_basictaste_D1 <- Q.basictaste.df[(Q.basictaste.df$f.day==1),]
Q_tastedays_basictaste_D3 <- Q.basictaste.df[(Q.basictaste.df$f.day==3),]
Q_tastedays_basictaste_D5 <- Q.basictaste.df[(Q.basictaste.df$f.day==5),]

cellmatch_tastedays_basictaste <- match_df(Q_tastedays_basictaste_D1, Q_tastedays_basictaste_D3, on="f.cell")
cellmatch_tastedays_basictastefinal <- match_df(cellmatch_tastedays_basictaste, Q_tastedays_basictaste_D5, on="f.cell")

Q_tastedays_basictaste_tracked <- rbind(cellmatch_tastedays_basictastefinal, Q_tastedays_basictaste_D1[Q_tastedays_basictaste_D1$f.cell %in% cellmatch_tastedays_basictastefinal$f.cell,],
                                        Q_tastedays_basictaste_D3[Q_tastedays_basictaste_D3$f.cell %in% cellmatch_tastedays_basictastefinal$f.cell,], Q_tastedays_basictaste_D5[Q_tastedays_basictaste_D5$f.cell %in% cellmatch_tastedays_basictastefinal$f.cell,])

Q_tastedays_basictaste_D1_Novela <- rbind(Q_tastedays_basictaste_tracked,Q_tastedays_basictaste_D3,Q_tastedays_basictaste_D5)
Q_tastedays_basictaste_D3_Novela <- rbind(Q_tastedays_basictaste_tracked,Q_tastedays_basictaste_D1,Q_tastedays_basictaste_D5)
Q_tastedays_basictaste_D5_Novela <- rbind(Q_tastedays_basictaste_tracked,Q_tastedays_basictaste_D3,Q_tastedays_basictaste_D1)

Q_tastedays_basictaste_D1_novel <- Q_tastedays_basictaste_D1[!Q_tastedays_basictaste_D1$f.cell %in% Q_tastedays_basictaste_D1_Novela$f.cell,]
Q_tastedays_basictaste_D3_novel <- Q_tastedays_basictaste_D3[!Q_tastedays_basictaste_D3$f.cell %in% Q_tastedays_basictaste_D3_Novela$f.cell,]
Q_tastedays_basictaste_D5_novel <- Q_tastedays_basictaste_D5[!Q_tastedays_basictaste_D5$f.cell %in% Q_tastedays_basictaste_D5_Novela$f.cell,]

Q_tastedays_basictaste_novel <- rbind(Q_tastedays_basictaste_D1_novel, 
                                      Q_tastedays_basictaste_D3_novel,
                                      Q_tastedays_basictaste_D5_novel)

Tracked_and_novel_tastedays_basictaste <- rbind(Q_tastedays_basictaste_tracked,Q_tastedays_basictaste_novel)

Q_tastedays_basictaste_D1_shared <- Q_tastedays_basictaste_D1[!Q_tastedays_basictaste_D1$f.cell %in% Tracked_and_novel_tastedays_basictaste$f.cell,]
Q_tastedays_basictaste_D3_shared <- Q_tastedays_basictaste_D3[!Q_tastedays_basictaste_D3$f.cell %in% Tracked_and_novel_tastedays_basictaste$f.cell,]
Q_tastedays_basictaste_D5_shared <- Q_tastedays_basictaste_D5[!Q_tastedays_basictaste_D5$f.cell %in% Tracked_and_novel_tastedays_basictaste$f.cell,]

Q_tastedays_basictaste_shared <- rbind(Q_tastedays_basictaste_D1_shared, 
                                       Q_tastedays_basictaste_D3_shared, 
                                       Q_tastedays_basictaste_D5_shared)

best_tracked_tastedays_basictaste <- best.df[best.df$f.cell %in% Q_tastedays_basictaste_tracked$f.cell,]
best_new_tastedays_basictaste <- best.df[best.df$f.cell %in% Q_tastedays_basictaste_novel,]
best_shared_tastedays_basictaste <- best.df[best.df$f.cell %in% Q_tastedays_basictaste_shared]

#find cells present in all days, find cells specific to each day for all stim
Q_D1 <- Q.allstim.df[(Q.allstim.df$f.day==1),]
Q_D2 <- Q.allstim.df[(Q.allstim.df$f.day==2),]
Q_D3 <- Q.allstim.df[(Q.allstim.df$f.day==3),]
Q_D4 <- Q.allstim.df[(Q.allstim.df$f.day==4),]
Q_D5 <- Q.allstim.df[(Q.allstim.df$f.day==5),]
#Q_D6 <- Q.allstim.df[(Q.allstim.df$f.day==6),]
#Q_D7 <- Q.allstim.df[(Q.allstim.df$f.day==7),]
#Q_D8 <- Q.allstim.df[(Q.allstim.df$f.day==8),]
#Q_D9 <- Q.allstim.df[(Q.allstim.df$f.day==9),]
#Q_D10 <- Q.allstim.df[(Q.allstim.df$f.day==10),]
#Q_D11 <- Q.allstim.df[(Q.allstim.df$f.day==11),]

cellmatch <- match_df(Q_D1, Q_D2, on="f.cell")
cellmatch2 <- match_df(cellmatch, Q_D3, on="f.cell")
cellmatch3 <-match_df(cellmatch2,Q_D4,on="f.cell")
cellmatchfinal <- match_df(cellmatch3, Q_D5, on="f.cell")
#cellmatch5 <- match_df(cellmatch4, Q_D6, on="f.cell")
#cellmatch6 <- match_df(cellmatch5, Q_D7, on="f.cell")
#cellmatch7 <- match_df(cellmatch6, Q_D8, on="f.cell")
#cellmatch8 <- match_df(cellmatch7, Q_D9, on="f.cell")
#cellmatch9 <- match_df(cellmatch8, Q_D10, on="f.cell")
#cellmatchfinal <- match_df(cellmatch9, Q_D11, on="f.cell")


Q_alldays_allstim_tracked <- rbind(Q_D1[Q_D1$f.cell %in% cellmatchfinal$f.cell,],
                                   Q_D2[Q_D2$f.cell %in% cellmatchfinal$f.cell,],  Q_D3[Q_D3$f.cell %in% cellmatchfinal$f.cell,],
                                    Q_D4[Q_D4$f.cell %in% cellmatchfinal$f.cell,], Q_D5[Q_D5$f.cell %in% cellmatchfinal$f.cell,])

Q_alldays_D1_Novela <- rbind(Q_alldays_allstim_tracked,Q_D2,Q_D3,Q_D4,Q_D5)
Q_alldays_D2_Novela <- rbind(Q_alldays_allstim_tracked,Q_D1,Q_D3, Q_D4,Q_D5)
Q_alldays_D3_Novela <- rbind(Q_alldays_allstim_tracked,Q_D1,Q_D2, Q_D4, Q_D5)
Q_alldays_D4_Novela <- rbind(Q_alldays_allstim_tracked,Q_D3,Q_D2, Q_D1, Q_D5)
Q_alldays_D5_Novela <- rbind(Q_alldays_allstim_tracked,Q_D3,Q_D2, Q_D1, Q_D4)

Q_alldays_D1_novel <- Q_D1[!Q_D1$f.cell %in% Q_alldays_D1_Novela$f.cell,]
Q_alldays_D2_novel <- Q_D2[!Q_D2$f.cell %in% Q_alldays_D2_Novela$f.cell,]
Q_alldays_D3_novel <- Q_D3[!Q_D3$f.cell %in% Q_alldays_D3_Novela$f.cell,]
Q_alldays_D4_novel <- Q_D4[!Q_D4$f.cell %in% Q_alldays_D4_Novela$f.cell,]
Q_alldays_D5_novel <- Q_D5[!Q_D5$f.cell %in% Q_alldays_D5_Novela$f.cell,]
#Q_alldays_D6_novel <- Q_D6[!Q_D6$f.cell %in% Q_alldays_D6_Novela$f.cell,]
#Q_alldays_D7_novel <- Q_D7[!Q_D7$f.cell %in% Q_alldays_D7_Novela$f.cell,]
#Q_alldays_D8_novel <- Q_D8[!Q_D8$f.cell %in% Q_alldays_D8_Novela$f.cell,]
#Q_alldays_D9_novel <- Q_D9[!Q_D9$f.cell %in% Q_alldays_D9_Novela$f.cell,]
#Q_alldays_D10_novel <- Q_D10[!Q_D10$f.cell %in% Q_alldays_D10_Novela$f.cell,]
#Q_alldays_D11_novel <- Q_D11[!Q_D11$f.cell %in% Q_alldays_D11_Novela$f.cell,]




Q_alldays_allstim_novel <- rbind(Q_alldays_D1_novel,
                                 Q_alldays_D2_novel, 
                                 Q_alldays_D3_novel,  Q_alldays_D4_novel, Q_alldays_D5_novel)

Tracked_and_novel <- rbind(Q_alldays_allstim_tracked,Q_alldays_allstim_novel)

Q_alldays_D1_shared <- Q_D1[!Q_D1$f.cell %in% Tracked_and_novel$f.cell,]
Q_alldays_D2_shared <- Q_D2[!Q_D2$f.cell %in% Tracked_and_novel$f.cell,]
Q_alldays_D3_shared <- Q_D3[!Q_D3$f.cell %in% Tracked_and_novel$f.cell,]
Q_alldays_D4_shared <- Q_D4[!Q_D4$f.cell %in% Tracked_and_novel$f.cell,]
Q_alldays_D5_shared <- Q_D5[!Q_D5$f.cell %in% Tracked_and_novel$f.cell,]
#Q_alldays_D6_shared <- Q_D6[!Q_D6$f.cell %in% Tracked_and_novel$f.cell,]
#Q_alldays_D7_shared <- Q_D7[!Q_D7$f.cell %in% Tracked_and_novel$f.cell,]
#Q_alldays_D8_shared <- Q_D8[!Q_D8$f.cell %in% Tracked_and_novel$f.cell,]
#Q_alldays_D9_shared <- Q_D9[!Q_D9$f.cell %in% Tracked_and_novel$f.cell,]
#Q_alldays_D10_shared <- Q_D10[!Q_D10$f.cell %in% Tracked_and_novel$f.cell,]
#Q_alldays_D11_shared <- Q_D11[!Q_D11$f.cell %in% Tracked_and_novel$f.cell,]

Q_alldays_allstim_shared <- rbind(Q_alldays_D1_shared,
                                  Q_alldays_D2_shared, 
                                  Q_alldays_D3_shared, Q_alldays_D4_shared, Q_alldays_D5_shared)

best_tracked_alldays <- best.df[best.df$f.cell %in% Q_alldays_allstim_tracked$f.cell,]
best_new_alldays <- best.df[best.df$f.cell %in% Q_alldays_allstim_novel,]
best_shared_alldays <- best.df[best.df$f.cell %in% Q_alldays_allstim_shared]



#########################Tracking reduced responders

sig_delta_smooth_alldata_reducedresponders<-numeric(c.all3*num)
dim(sig_delta_smooth_alldata_reducedresponders)<-c(c.all3,num)



#make a list with the maximum response value (single point) during taste
list3<-apply(sig_smooth_all[,75:150],1,which.min)
tastemin <- apply(sig_smooth_sub, 1, min)

# loop to find unique cell IDs
for (j in 1:c.all3){
  ID<-c(f.anml[j], f.day[j], f.trial[j],f.cell[j])
  for (k in 1:n.cellIDstar3){
    if(f.cellIDstar3[k,1]==ID[1] &
       f.cellIDstar3[k,2]==ID[2] &
       f.cellIDstar3[k,3]==ID[3] &
       f.cellIDstar3[k,4]==ID[4]) f.IDstar3[j]<-k
  } # end k
} # end j

#find the average of the maximum resp +/- one second, 15 FPS
min_resp_taste<-numeric(c.all3)
for (j in 1:c.all3){
  if (list3[j]==min(list3)|list3[j]==min(list3)+1|list3[j]==min(list3)+2|list3[j]==min(list3)+3|list3[j]==min(list3)+4|list3[j]==min(list3)+5|list3[j]==min(list3)+6|list3[j]==min(list3)+7|list3[j]==min(list3)+8|list3[j]==min(list3)+9|list3[j]==min(list3)+10|list3[j]==min(list3)+11|list3[j]==min(list3)+12|list3[j]==min(list3)+13|list3[j]==min(list3)+14|list3[j]==min(list3)+15) {
    min_resp_taste[j]<-((sig_smooth_sub[j,list3[j]] + sig_smooth_sub[j,(list3[j]+1)] + sig_smooth_sub[j,(list3[j]+2)] + sig_smooth_sub[j,(list3[j]+3)]+ sig_smooth_sub[j,(list3[j]+4)]+ sig_smooth_sub[j,(list3[j]+5)]+ sig_smooth_sub[j,(list3[j]+6)]+ sig_smooth_sub[j,(list3[j]+7)]+ sig_smooth_sub[j,(list3[j]+8)]+ sig_smooth_sub[j,(list3[j]+9)]+ sig_smooth_sub[j,(list3[j]+10)]+ sig_smooth_sub[j,(list3[j]+11)]+ sig_smooth_sub[j,(list3[j]+12)]+ sig_smooth_sub[j,(list3[j]+13)]+ sig_smooth_sub[j,(list3[j]+14)]+ sig_smooth_sub[j,(list3[j]+15)])/16)
  } else if (list3[j]==max(list3)|list3[j]==max(list3)-1|list3[j]==max(list3)-2|list3[j]==max(list3)-3|list3[j]==max(list3)-4|list3[j]==max(list3)-5|list3[j]==max(list3)-6|list3[j]==max(list3)-7|list3[j]==max(list3)-8|list3[j]==max(list3)-9|list3[j]==max(list3)-10|list3[j]==max(list3)-11|list3[j]==max(list3)-12|list3[j]==max(list3)-13|list3[j]==max(list3)-14|list3[j]==max(list3)-15) {
    min_resp_taste[j]<-((sig_smooth_sub[j,list3[j]] + sig_smooth_sub[j,(list3[j]-1)] + sig_smooth_sub[j,(list3[j]-2)] + sig_smooth_sub[j,(list3[j]-3)] + sig_smooth_sub[j,(list3[j]-4)] + sig_smooth_sub[j,(list3[j]-5)] + sig_smooth_sub[j,(list3[j]-6)] + sig_smooth_sub[j,(list3[j]-7)]+ sig_smooth_sub[j,(list3[j]-8)]+ sig_smooth_sub[j,(list3[j]-9)]+ sig_smooth_sub[j,(list3[j]-10)]+ sig_smooth_sub[j,(list3[j]-11)]+ sig_smooth_sub[j,(list3[j]-12)]+ sig_smooth_sub[j,(list3[j]-13)]+ sig_smooth_sub[j,(list3[j]-14)]+ sig_smooth_sub[j,(list3[j]-15)])/16)
  } else {
    min_resp_taste[j]<-((sig_smooth_sub[j,list3[j]] + sig_smooth_sub[j,(list3[j]+1)] + sig_smooth_sub[j,(list3[j]-1)] + sig_smooth_sub[j,(list3[j]+2)] + sig_smooth_sub[j,(list3[j]-2)] + sig_smooth_sub[j,(list3[j]+3)] + sig_smooth_sub[j,(list3[j]-3)]+ sig_smooth_sub[j,(list3[j]+4)] + sig_smooth_sub[j,(list3[j]-4)]+ sig_smooth_sub[j,(list3[j]+5)] + sig_smooth_sub[j,(list3[j]-5)]+ sig_smooth_sub[j,(list3[j]+6)] + sig_smooth_sub[j,(list3[j]-6)]+ sig_smooth_sub[j,(list3[j]+7)] + sig_smooth_sub[j,(list3[j]-7)]+ sig_smooth_sub[j,(list3[j]+8)] + sig_smooth_sub[j,(list3[j]-8)]+ sig_smooth_sub[j,(list3[j]+9)] + sig_smooth_sub[j,(list3[j]-9)]
                         + sig_smooth_sub[j,(list3[j]+10)] + sig_smooth_sub[j,(list3[j]-10)]+ sig_smooth_sub[j,(list3[j]+11)] + sig_smooth_sub[j,(list3[j]-11)]+ sig_smooth_sub[j,(list3[j]+12)] + sig_smooth_sub[j,(list3[j]-12)]+ sig_smooth_sub[j,(list3[j]+13)] + sig_smooth_sub[j,(list3[j]-13)]+ sig_smooth_sub[j,(list3[j]+14)] + sig_smooth_sub[j,(list3[j]-14)]+ sig_smooth_sub[j,(list3[j]+15)] + sig_smooth_sub[j,(list3[j]-15)])/31)
  }
} # end j

for (j in 1:c.all3){
  if (tastemin[j]!=0) {
    sig_delta_smooth_alldata_reducedresponders[j]<-((min_resp_taste[j]-baseline_mean[j]))
  } else {
    sig_delta_smooth_alldata_reducedresponders[j]<-0
  }
} # end j

#Find all significant responses 

t.all<-2.5 #set threshold
tk<-numeric(t.all)
#MAT=maximum above threshold
MAT5<-numeric(c.all3)
for (k in 1:t.all){
  tk[k]<-paste("thresh_decrease",k,sep="")
  TK<-eval(parse(text=tk[k]))
  for (j in 1:c.all3){
    #sig_resp[j,]<-sig[j,which(t.base>t)]
    #FAT[j]<-length(which(sig_resp[j,]>TK[j]))
    MAT5[j]<-ifelse(max_resp_taste[j]<TK[j],1,0)
    test4<-paste("MAT",k,sep="")
    assign(test4,MAT5)
  }
}



#making non threshold responses. responses 0
for (j in 1:c.all3){
  if (MAT5[j]>0) {
    sig_delta_smooth_alldata_reducedresponders[j]<-sig_delta_smooth_alldata_reducedresponders[j]
  } else {
    sig_delta_smooth_alldata_reducedresponders[j]<-0
  }
} # end j


#making DeltaF below 1 responses 0
for (j in 1:c.all3){
  if (sig_delta_smooth_alldata_reducedresponders[j]<(-1)) {
    sig_delta_smooth_alldata_reducedresponders[j]<-sig_delta_smooth_alldata_reducedresponders[j]
  } else {
    sig_delta_smooth_alldata_reducedresponders[j]<-0
  }
} # end j

#make table of neuron responses
responses_final_alldata_reducedresponders<-as.data.frame(sig_delta_smooth_alldata_reducedresponders[,1])
responses_final_alldata4<-sig_delta_smooth_alldata_reducedresponders[,1]
table_reducedresponders<-tapply(responses_final_alldata4,list(f.IDstar3,f.taste3),mean)
df.reducedresponders<-data.frame(table_reducedresponders)
Q_reducedresponders<- cbind(f.cellIDstar3,df.reducedresponders)


Q_reduced <- Q_reducedresponders[,5:13]

m.best_reduced<-numeric(length(row.names(Q_reduced)))
cell_reduced<-length(m.best_reduced)


# find tastant with max response per cellID, all stimuli
for (j in 1:cell_reduced){
  best<-apply(Q_reduced,1,which.max)
  m.best_reduced[j]<-Q_reduced[j,best[j]]
}

#resp_cell_count <- nrow(tasteresp)

best.df_reduced <- as.data.frame(m.best_reduced)
best.df.info_reduced <-cbind(f.cellIDstar3,best.df_reduced)
best.df_reduced <- best.df.info_reduced


tastecols <- c(2,4,5,8)
tastecols_full <-c(2,4,5,6,8,9)

Q_reduced_all_taste <-as.data.frame(Q_reduced[,tastecols_full])
Q_reduced_basic_taste <-as.data.frame(Q_reduced[,tastecols])
Q_reduced2 <-as.data.frame(Q_reduced[,tastecols_full])
Q_reduced3 <-as.data.frame(Q_reduced[,tastecols])
colnames(Q_reduced)<-c("B", "C", "H", "N", "Q", "Sac", "Shu", "Suc", "U")
colnames2 <- c("C","N","Q","Sac","Suc","U")
#colnames(Q_reducedsort_all_taste) <- colnames2
colnames3 <- c("C","N","Q","Suc")
colnames(Q_reduced_basic_taste) <- colnames3



Q_reduced.info <-cbind(f.cellIDstar3,Q_reduced)
Q_reduced.allstim.df <-Q_reduced.info

Q_reduced2.info <-cbind(f.cellIDstar3,Q_reduced2)
Q_reduced.alltaste.df <-Q_reduced2.info
Q_reduced2.df <- as.data.frame(Q_reduced2.info)

Q_reduced3.info <-cbind(f.cellIDstar3,Q_reduced3)
Q_reduced.basictaste.df <-Q_reduced3.info
Q_reduced3.df <- as.data.frame(Q_reduced3.info)



#N/S calculation = Q_reducedbest/Q_reducedsecondbest

#norm Q_reduced to max of best responses, All stimuli

cell_reduced<-length(m.best_reduced)
Q_reducednorm<-numeric(cell_reduced*9)
dim(Q_reducednorm)<-c(cell_reduced,9)


for (j in 1:cell){
  Q_reducednorm[j,1]<-Q_reduced[j,1]/m.best_reduced[j]
  Q_reducednorm[j,2]<-Q_reduced[j,2]/m.best_reduced[j]
  Q_reducednorm[j,3]<-Q_reduced[j,3]/m.best_reduced[j]
  Q_reducednorm[j,4]<-Q_reduced[j,4]/m.best_reduced[j]
  Q_reducednorm[j,5]<-Q_reduced[j,5]/m.best_reduced[j]
  Q_reducednorm[j,6]<-Q_reduced[j,6]/m.best_reduced[j]
  Q_reducednorm[j,7]<-Q_reduced[j,7]/m.best_reduced[j]
  Q_reducednorm[j,8]<-Q_reduced[j,8]/m.best_reduced[j]
  Q_reducednorm[j,9]<-Q_reduced[j,9]/m.best_reduced[j]
}

colnames(Q_reducednorm)<-colnames(Q_reduced)
Q_reducednorm.info <-cbind(f.cellIDstar3,Q_reducednorm)
Q_reducednorm_allstim <-Q_reducednorm.info


################################################################################################
#Tracking Reduced Responders

#find cells present in taste days, find cells specific to each day for all stim
Q_reduced_tastedays_allstim_D1 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==1),]
Q_reduced_tastedays_allstim_D3 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==3),]
Q_reduced_tastedays_allstim_D5 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==5),]

cellmatch_tastedays_allstim_reduced <- match_df(Q_reduced_tastedays_allstim_D1, Q_reduced_tastedays_allstim_D3, on="f.cell")
cellmatch_tastedays_allstim_reducedfinal <- match_df(cellmatch_tastedays_allstim_reduced, Q_reduced_tastedays_allstim_D5, on="f.cell")

Q_reduced_tastedays_allstim_tracked <- rbind(cellmatch_tastedays_allstim_reducedfinal, Q_reduced_tastedays_allstim_D1[Q_reduced_tastedays_allstim_D1$f.cell %in% cellmatch_tastedays_allstim_reducedfinal$f.cell,],
                                     Q_reduced_tastedays_allstim_D3[Q_reduced_tastedays_allstim_D3$f.cell %in% cellmatch_tastedays_allstim_reducedfinal$f.cell,], Q_reduced_tastedays_allstim_D5[Q_reduced_tastedays_allstim_D5$f.cell %in% cellmatch_tastedays_allstim_reducedfinal$f.cell,])

Q_reduced_tastedays_allstim_D1_Novela <- rbind(Q_reduced_tastedays_allstim_tracked,Q_reduced_tastedays_allstim_D3,Q_reduced_tastedays_allstim_D5)
Q_reduced_tastedays_allstim_D3_Novela <- rbind(Q_reduced_tastedays_allstim_tracked,Q_reduced_tastedays_allstim_D1,Q_reduced_tastedays_allstim_D5)
Q_reduced_tastedays_allstim_D5_Novela <- rbind(Q_reduced_tastedays_allstim_tracked,Q_reduced_tastedays_allstim_D3,Q_reduced_tastedays_allstim_D1)

Q_reduced_tastedays_allstim_D1_novel <- Q_reduced_tastedays_allstim_D1[!Q_reduced_tastedays_allstim_D1$f.cell %in% Q_reduced_tastedays_allstim_D1_Novela$f.cell,]
Q_reduced_tastedays_allstim_D3_novel <- Q_reduced_tastedays_allstim_D3[!Q_reduced_tastedays_allstim_D3$f.cell %in% Q_reduced_tastedays_allstim_D3_Novela$f.cell,]
Q_reduced_tastedays_allstim_D5_novel <- Q_reduced_tastedays_allstim_D5[!Q_reduced_tastedays_allstim_D5$f.cell %in% Q_reduced_tastedays_allstim_D5_Novela$f.cell,]

Q_reduced_tastedays_allstim_novel <- rbind(Q_reduced_tastedays_allstim_D1_novel, 
                                   Q_reduced_tastedays_allstim_D3_novel,
                                   Q_reduced_tastedays_allstim_D5_novel)

Tracked_and_novel_tastedays_allstim_reduced <- rbind(Q_reduced_tastedays_allstim_tracked,Q_reduced_tastedays_allstim_novel)

Q_reduced_tastedays_allstim_D1_shared <- Q_reduced_tastedays_allstim_D1[!Q_reduced_tastedays_allstim_D1$f.cell %in% Tracked_and_novel_tastedays_allstim_reduced$f.cell,]
Q_reduced_tastedays_allstim_D3_shared <- Q_reduced_tastedays_allstim_D3[!Q_reduced_tastedays_allstim_D3$f.cell %in% Tracked_and_novel_tastedays_allstim_reduced$f.cell,]
Q_reduced_tastedays_allstim_D5_shared <- Q_reduced_tastedays_allstim_D5[!Q_reduced_tastedays_allstim_D5$f.cell %in% Tracked_and_novel_tastedays_allstim_reduced$f.cell,]

Q_reduced_tastedays_allstim_shared <- rbind(Q_reduced_tastedays_allstim_D1_shared, 
                                    Q_reduced_tastedays_allstim_D3_shared, 
                                    Q_reduced_tastedays_allstim_D5_shared)

best_tracked_tastedays_allstim_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_allstim_tracked$f.cell,]
best_new_tastedays_allstim_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_allstim_novel,]
best_shared_tastedays_allstim_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_allstim_shared]


#find cells present in taste days, find cells specific to each day for all taste
Q_reduced_tastedays_allstim_reduced_D1 <- Q_reduced.alltaste.df[(Q_reduced.alltaste.df$f.day==1),]
Q_reduced_tastedays_allstim_reduced_D3 <- Q_reduced.alltaste.df[(Q_reduced.alltaste.df$f.day==3),]
Q_reduced_tastedays_allstim_reduced_D5 <- Q_reduced.alltaste.df[(Q_reduced.alltaste.df$f.day==5),]

cellmatch_tastedays_allstim_reduced <- match_df(Q_reduced_tastedays_allstim_reduced_D1, Q_reduced_tastedays_allstim_reduced_D3, on="f.cell")
cellmatch_tastedays_allstim_reducedfinal <- match_df(cellmatch_tastedays_allstim_reduced, Q_reduced_tastedays_allstim_reduced_D5, on="f.cell")

Q_reduced_tastedays_allstim_reduced_tracked <- rbind(cellmatch_tastedays_allstim_reducedfinal, Q_reduced_tastedays_allstim_reduced_D1[Q_reduced_tastedays_allstim_reduced_D1$f.cell %in% cellmatch_tastedays_allstim_reducedfinal$f.cell,],
                                      Q_reduced_tastedays_allstim_reduced_D3[Q_reduced_tastedays_allstim_reduced_D3$f.cell %in% cellmatch_tastedays_allstim_reducedfinal$f.cell,], Q_reduced_tastedays_allstim_reduced_D5[Q_reduced_tastedays_allstim_reduced_D5$f.cell %in% cellmatch_tastedays_allstim_reducedfinal$f.cell,])

Q_reduced_tastedays_allstim_reduced_D1_Novela <- rbind(Q_reduced_tastedays_allstim_reduced_tracked,Q_reduced_tastedays_allstim_reduced_D3,Q_reduced_tastedays_allstim_reduced_D5)
Q_reduced_tastedays_allstim_reduced_D3_Novela <- rbind(Q_reduced_tastedays_allstim_reduced_tracked,Q_reduced_tastedays_allstim_reduced_D1,Q_reduced_tastedays_allstim_reduced_D5)
Q_reduced_tastedays_allstim_reduced_D5_Novela <- rbind(Q_reduced_tastedays_allstim_reduced_tracked,Q_reduced_tastedays_allstim_reduced_D3,Q_reduced_tastedays_allstim_reduced_D1)

Q_reduced_tastedays_allstim_reduced_D1_novel <- Q_reduced_tastedays_allstim_reduced_D1[!Q_reduced_tastedays_allstim_reduced_D1$f.cell %in% Q_reduced_tastedays_allstim_reduced_D1_Novela$f.cell,]
Q_reduced_tastedays_allstim_reduced_D3_novel <- Q_reduced_tastedays_allstim_reduced_D3[!Q_reduced_tastedays_allstim_reduced_D3$f.cell %in% Q_reduced_tastedays_allstim_reduced_D3_Novela$f.cell,]
Q_reduced_tastedays_allstim_reduced_D5_novel <- Q_reduced_tastedays_allstim_reduced_D5[!Q_reduced_tastedays_allstim_reduced_D5$f.cell %in% Q_reduced_tastedays_allstim_reduced_D5_Novela$f.cell,]

Q_reduced_tastedays_allstim_reduced_novel <- rbind(Q_reduced_tastedays_allstim_reduced_D1_novel, 
                                    Q_reduced_tastedays_allstim_reduced_D3_novel,
                                    Q_reduced_tastedays_allstim_reduced_D5_novel)

Tracked_and_novel_tastedays_allstim_reduced <- rbind(Q_reduced_tastedays_allstim_reduced_tracked,Q_reduced_tastedays_allstim_reduced_novel)

Q_reduced_tastedays_allstim_reduced_D1_shared <- Q_reduced_tastedays_allstim_reduced_D1[!Q_reduced_tastedays_allstim_reduced_D1$f.cell %in% Tracked_and_novel_tastedays_allstim_reduced$f.cell,]
Q_reduced_tastedays_allstim_reduced_D3_shared <- Q_reduced_tastedays_allstim_reduced_D3[!Q_reduced_tastedays_allstim_reduced_D3$f.cell %in% Tracked_and_novel_tastedays_allstim_reduced$f.cell,]
Q_reduced_tastedays_allstim_reduced_D5_shared <- Q_reduced_tastedays_allstim_reduced_D5[!Q_reduced_tastedays_allstim_reduced_D5$f.cell %in% Tracked_and_novel_tastedays_allstim_reduced$f.cell,]

Q_reduced_tastedays_allstim_reduced_shared <- rbind(Q_reduced_tastedays_allstim_reduced_D1_shared, 
                                     Q_reduced_tastedays_allstim_reduced_D3_shared, 
                                     Q_reduced_tastedays_allstim_reduced_D5_shared)

best_tracked_tastedays_allstim_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_allstim_reduced_tracked$f.cell,]
best_new_tastedays_allstim_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_allstim_reduced_novel,]
best_shared_tastedays_allstim_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_allstim_reduced_shared]

#find cells present in days taste days, find cells specific to each day for basic taste
Q_reduced_tastedays_basictaste_reduced_D1 <- Q_reduced.basictaste.df[(Q_reduced.basictaste.df$f.day==1),]
Q_reduced_tastedays_basictaste_reduced_D3 <- Q_reduced.basictaste.df[(Q_reduced.basictaste.df$f.day==3),]
Q_reduced_tastedays_basictaste_reduced_D5 <- Q_reduced.basictaste.df[(Q_reduced.basictaste.df$f.day==5),]

cellmatch_tastedays_basictaste_reduced <- match_df(Q_reduced_tastedays_basictaste_reduced_D1, Q_reduced_tastedays_basictaste_reduced_D3, on="f.cell")
cellmatch_tastedays_basictaste_reducedfinal <- match_df(cellmatch_tastedays_basictaste_reduced, Q_reduced_tastedays_basictaste_reduced_D5, on="f.cell")

Q_reduced_tastedays_basictaste_reduced_tracked <- rbind(cellmatch_tastedays_basictaste_reducedfinal, Q_reduced_tastedays_basictaste_reduced_D1[Q_reduced_tastedays_basictaste_reduced_D1$f.cell %in% cellmatch_tastedays_basictaste_reducedfinal$f.cell,],
                                        Q_reduced_tastedays_basictaste_reduced_D3[Q_reduced_tastedays_basictaste_reduced_D3$f.cell %in% cellmatch_tastedays_basictaste_reducedfinal$f.cell,], Q_reduced_tastedays_basictaste_reduced_D5[Q_reduced_tastedays_basictaste_reduced_D5$f.cell %in% cellmatch_tastedays_basictaste_reducedfinal$f.cell,])

Q_reduced_tastedays_basictaste_reduced_D1_Novela <- rbind(Q_reduced_tastedays_basictaste_reduced_tracked,Q_reduced_tastedays_basictaste_reduced_D3,Q_reduced_tastedays_basictaste_reduced_D5)
Q_reduced_tastedays_basictaste_reduced_D3_Novela <- rbind(Q_reduced_tastedays_basictaste_reduced_tracked,Q_reduced_tastedays_basictaste_reduced_D1,Q_reduced_tastedays_basictaste_reduced_D5)
Q_reduced_tastedays_basictaste_reduced_D5_Novela <- rbind(Q_reduced_tastedays_basictaste_reduced_tracked,Q_reduced_tastedays_basictaste_reduced_D3,Q_reduced_tastedays_basictaste_reduced_D1)

Q_reduced_tastedays_basictaste_reduced_D1_novel <- Q_reduced_tastedays_basictaste_reduced_D1[!Q_reduced_tastedays_basictaste_reduced_D1$f.cell %in% Q_reduced_tastedays_basictaste_reduced_D1_Novela$f.cell,]
Q_reduced_tastedays_basictaste_reduced_D3_novel <- Q_reduced_tastedays_basictaste_reduced_D3[!Q_reduced_tastedays_basictaste_reduced_D3$f.cell %in% Q_reduced_tastedays_basictaste_reduced_D3_Novela$f.cell,]
Q_reduced_tastedays_basictaste_reduced_D5_novel <- Q_reduced_tastedays_basictaste_reduced_D5[!Q_reduced_tastedays_basictaste_reduced_D5$f.cell %in% Q_reduced_tastedays_basictaste_reduced_D5_Novela$f.cell,]

Q_reduced_tastedays_basictaste_reduced_novel <- rbind(Q_reduced_tastedays_basictaste_reduced_D1_novel, 
                                      Q_reduced_tastedays_basictaste_reduced_D3_novel,
                                      Q_reduced_tastedays_basictaste_reduced_D5_novel)

Tracked_and_novel_tastedays_basictaste_reduced <- rbind(Q_reduced_tastedays_basictaste_reduced_tracked,Q_reduced_tastedays_basictaste_reduced_novel)

Q_reduced_tastedays_basictaste_reduced_D1_shared <- Q_reduced_tastedays_basictaste_reduced_D1[!Q_reduced_tastedays_basictaste_reduced_D1$f.cell %in% Tracked_and_novel_tastedays_basictaste_reduced$f.cell,]
Q_reduced_tastedays_basictaste_reduced_D3_shared <- Q_reduced_tastedays_basictaste_reduced_D3[!Q_reduced_tastedays_basictaste_reduced_D3$f.cell %in% Tracked_and_novel_tastedays_basictaste_reduced$f.cell,]
Q_reduced_tastedays_basictaste_reduced_D5_shared <- Q_reduced_tastedays_basictaste_reduced_D5[!Q_reduced_tastedays_basictaste_reduced_D5$f.cell %in% Tracked_and_novel_tastedays_basictaste_reduced$f.cell,]

Q_reduced_tastedays_basictaste_reduced_shared <- rbind(Q_reduced_tastedays_basictaste_reduced_D1_shared, 
                                       Q_reduced_tastedays_basictaste_reduced_D3_shared, 
                                       Q_reduced_tastedays_basictaste_reduced_D5_shared)

best_tracked_tastedays_basictaste_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_basictaste_reduced_tracked$f.cell,]
best_new_tastedays_basictaste_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_basictaste_reduced_novel,]
best_shared_tastedays_basictaste_reduced <- best.df[best.df$f.cell %in% Q_reduced_tastedays_basictaste_reduced_shared]

#find cells present in all days, find cells specific to each day for all stim
Q_reduced_D1 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==1),]
Q_reduced_D2 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==2),]
Q_reduced_D3 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==3),]
Q_reduced_D4 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==4),]
Q_reduced_D5 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==5),]
#Q_reduced_D6 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==6),]
#Q_reduced_D7 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==4),]
#Q_reduced_D8 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==8),]
#Q_reduced_D9 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==5),]
#Q_reduced_D10 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==10),]
#Q_reduced_D11 <- Q_reduced.allstim.df[(Q_reduced.allstim.df$f.day==11),]


cellmatch_reduced <- match_df(Q_reduced_D1, Q_reduced_D2, on="f.cell")
cellmatch_reduced2 <- match_df(cellmatch_reduced, Q_reduced_D3, on="f.cell")
cellmatch_reduced3 <- match_df(cellmatch_reduced2, Q_reduced_D4, on="f.cell")
cellmatch_reducedfinal <- match_df(cellmatch_reduced3, Q_reduced_D5, on="f.cell")
#cellmatch_reduced5 <- match_df(cellmatch_reduced4, Q_reduced_D6, on="f.cell")
#cellmatch_reduced6 <- match_df(cellmatch_reduced5, Q_reduced_D7, on="f.cell")
#cellmatch_reduced7 <- match_df(cellmatch_reduced6, Q_reduced_D8, on="f.cell")
#cellmatch_reduced8 <- match_df(cellmatch_reduced7, Q_reduced_D9, on="f.cell")
#cellmatch_reduced9 <- match_df(cellmatch_reduced8, Q_reduced_D10, on="f.cell")
#cellmatch_reducedfinal <- match_df(cellmatch_reduced9, Q_reduced_D11, on="f.cell")


Q_reduced_alldays_allstim_tracked <- rbind(Q_reduced_D1[Q_reduced_D1$f.cell %in% cellmatch_reducedfinal$f.cell,],
                                   Q_reduced_D2[Q_reduced_D2$f.cell %in% cellmatch_reducedfinal$f.cell,], Q_reduced_D3[Q_reduced_D3$f.cell %in% cellmatch_reducedfinal$f.cell,],
                                    Q_reduced_D4[Q_reduced_D4$f.cell %in% cellmatch_reducedfinal$f.cell,], Q_reduced_D5[Q_reduced_D5$f.cell %in% cellmatch_reducedfinal$f.cell,])

Q_reduced_alldays_D1_Novela <- rbind(Q_reduced_alldays_allstim_tracked,Q_reduced_D2,Q_reduced_D3,Q_reduced_D4, Q_reduced_D5)
Q_reduced_alldays_D2_Novela <- rbind(Q_reduced_alldays_allstim_tracked,Q_reduced_D1,Q_reduced_D3,Q_reduced_D4, Q_reduced_D5)
Q_reduced_alldays_D3_Novela <- rbind(Q_reduced_alldays_allstim_tracked,Q_reduced_D2,Q_reduced_D1,Q_reduced_D4, Q_reduced_D5)
Q_reduced_alldays_D4_Novela <- rbind(Q_reduced_alldays_allstim_tracked,Q_reduced_D2,Q_reduced_D3,Q_reduced_D1, Q_reduced_D5)
Q_reduced_alldays_D5_Novela <- rbind(Q_reduced_alldays_allstim_tracked,Q_reduced_D2,Q_reduced_D3,Q_reduced_D1, Q_reduced_D4)


Q_reduced_alldays_D1_novel <- Q_reduced_D1[!Q_reduced_D1$f.cell %in% Q_reduced_alldays_D1_Novela$f.cell,]
Q_reduced_alldays_D2_novel <- Q_reduced_D2[!Q_reduced_D2$f.cell %in% Q_reduced_alldays_D2_Novela$f.cell,]
Q_reduced_alldays_D3_novel <- Q_reduced_D3[!Q_reduced_D3$f.cell %in% Q_reduced_alldays_D3_Novela$f.cell,]
Q_reduced_alldays_D4_novel <- Q_reduced_D4[!Q_reduced_D4$f.cell %in% Q_reduced_alldays_D4_Novela$f.cell,]
Q_reduced_alldays_D5_novel <- Q_reduced_D5[!Q_reduced_D5$f.cell %in% Q_reduced_alldays_D5_Novela$f.cell,]




Q_reduced_alldays_allstim_novel <- rbind(Q_reduced_alldays_D1_novel,
                                 Q_reduced_alldays_D2_novel, 
                                 Q_reduced_alldays_D3_novel, 
                                 Q_reduced_alldays_D4_novel,
                                 Q_reduced_alldays_D5_novel)

Tracked_and_novel_reduced <- rbind(Q_reduced_alldays_allstim_tracked,Q_reduced_alldays_allstim_novel)

Q_reduced_alldays_D1_shared <- Q_reduced_D1[!Q_reduced_D1$f.cell %in% Tracked_and_novel_reduced$f.cell,]
Q_reduced_alldays_D2_shared <- Q_reduced_D2[!Q_reduced_D2$f.cell %in% Tracked_and_novel_reduced$f.cell,]
Q_reduced_alldays_D3_shared <- Q_reduced_D3[!Q_reduced_D3$f.cell %in% Tracked_and_novel_reduced$f.cell,]
Q_reduced_alldays_D4_shared <- Q_reduced_D4[!Q_reduced_D4$f.cell %in% Tracked_and_novel_reduced$f.cell,]
Q_reduced_alldays_D5_shared <- Q_reduced_D5[!Q_reduced_D5$f.cell %in% Tracked_and_novel_reduced$f.cell,]
#Q_reduced_alldays_D6_shared <- Q_reduced_D6[!Q_reduced_D6$f.cell %in% Tracked_and_novel_reduced$f.cell,]
#Q_reduced_alldays_D7_shared <- Q_reduced_D7[!Q_reduced_D7$f.cell %in% Tracked_and_novel_reduced$f.cell,]
#Q_reduced_alldays_D8_shared <- Q_reduced_D8[!Q_reduced_D8$f.cell %in% Tracked_and_novel_reduced$f.cell,]
#Q_reduced_alldays_D9_shared <- Q_reduced_D9[!Q_reduced_D9$f.cell %in% Tracked_and_novel_reduced$f.cell,]
#Q_reduced_alldays_D10_shared <- Q_reduced_D10[!Q_reduced_D10$f.cell %in% Tracked_and_novel_reduced$f.cell,]
#Q_reduced_alldays_D11_shared <- Q_reduced_D11[!Q_reduced_D11$f.cell %in% Tracked_and_novel_reduced$f.cell,]





Q_reduced_alldays_allstim_shared <- rbind(Q_reduced_alldays_D1_shared, 
                                  Q_reduced_alldays_D2_shared, 
                                  Q_reduced_alldays_D3_shared, 
                                  Q_reduced_alldays_D4_shared,
                                  Q_reduced_alldays_D5_shared)
                                  

best_tracked_alldays_reduced <- best.df[best.df$f.cell %in% Q_reduced_alldays_allstim_tracked$f.cell,]
best_new_alldays_reduced <- best.df[best.df$f.cell %in% Q_reduced_alldays_allstim_novel,]
best_shared_alldays_reduced <- best.df[best.df$f.cell %in% Q_reduced_alldays_allstim_shared]

##################################################################
####make a delta F dataframe for all cells (responders and non responders for alignment)
# define factor variables, etc
c.all2<-nrow(GUST)
f.anml2<-as.factor(GUST$Animal)
f.day2<-as.factor(GUST$Day_num)
f.cell2<-as.factor(GUST$Cell_num)
f.trial2<-as.factor(GUST$Trial_num)
f.taste2<-as.factor(GUST$Tastant)

sig_delta_smooth_alldata<-numeric(c.all2*num)
dim(sig_delta_smooth_alldata)<-c(c.all2,num)
f.taste2<-as.factor(GUST$Tastant)
f.IDstar2<-numeric(length=c.all2)
f.cellIDstar2<-unique(cbind(f.anml2, f.day2, f.trial2, f.cell2))
n.cellIDstar2<-length(f.cellIDstar2[,1])

# loop to find unique cell IDs ### f.trial should be removed to collapse?
for (j in 1:c.all2){
  ID<-c(f.anml2[j], f.day2[j], f.trial2[j], f.cell2[j])
  for (k in 1:n.cellIDstar2){
    if(f.cellIDstar2[k,1]==ID[1] &
       f.cellIDstar2[k,2]==ID[2] &
       f.cellIDstar2[k,3]==ID[3] &
       f.cellIDstar2[k,4]==ID[4]) f.IDstar2[j]<-k
  } # end k
} # end j

baseline_mean<-numeric(c.all2)
sigsmoothbaseline <- sig_smooth_all[,45:75]

#calculate delta F cell response on smoothed sig (Max response during taste +/- 1 second / mean of baseline)
for (j in 1:c.all2){
  baseline_mean[j]<-mean(sigsmoothbaseline[j,])
}

#making dataframe with resp-baseline if cell is present
for (j in 1:c.all2){
  if (tastemax[j]!=0) {
    sig_delta_smooth_alldata[j]<-((max_resp_taste[j]-baseline_mean[j]))
  } else {
    sig_delta_smooth_alldata[j]<-0
  }
} # end j

##################################################################
####make a delta F dataframe for all cells (responders and non responders for alignment)
# define factor variables, etc
c.all2<-nrow(GUST)
f.anml2<-as.factor(GUST$Animal)
f.day2<-as.factor(GUST$Day_num)
f.cell2<-as.factor(GUST$Cell_num)
f.trial2<-as.factor(GUST$Trial_num)
f.taste2<-as.factor(GUST$Tastant)

sig_delta_smooth_alldata<-numeric(c.all2*num)
dim(sig_delta_smooth_alldata)<-c(c.all2,num)
f.taste2<-as.factor(GUST$Tastant)
f.IDstar2<-numeric(length=c.all2)
f.cellIDstar2<-unique(cbind(f.anml2, f.day2, f.trial2, f.cell2))
n.cellIDstar2<-length(f.cellIDstar2[,1])

# loop to find unique cell IDs ### f.trial should be removed to collapse?
for (j in 1:c.all2){
  ID<-c(f.anml2[j], f.day2[j], f.trial2[j], f.cell2[j])
  for (k in 1:n.cellIDstar2){
    if(f.cellIDstar2[k,1]==ID[1] &
       f.cellIDstar2[k,2]==ID[2] &
       f.cellIDstar2[k,3]==ID[3] &
       f.cellIDstar2[k,4]==ID[4]) f.IDstar2[j]<-k
  } # end k
} # end j

baseline_mean<-numeric(c.all2)
sigsmoothbaseline <- sig_smooth_all[,45:75]

#calculate delta F cell response on smoothed sig (Max response during taste +/- 1 second / mean of baseline)
for (j in 1:c.all2){
  baseline_mean[j]<-mean(sigsmoothbaseline[j,])
}

#making dataframe with resp-baseline if cell is present
for (j in 1:c.all2){
  if (tastemax[j]!=0) {
    sig_delta_smooth_alldata[j]<-((max_resp_taste[j]-baseline_mean[j]))
  } else {
    sig_delta_smooth_alldata[j]<-0
  }
} # end j


#make table of neuron responses
responses_final_alldata<-as.data.frame(sig_delta_smooth_alldata[,1])
responses_final_alldata2<-sig_delta_smooth_alldata[,1]
table_alldata<-tapply(responses_final_alldata2,list(f.IDstar2,f.taste2),mean)

df.allresponses<-data.frame(table_alldata)

Q_alldata<- cbind(f.cellIDstar2,df.allresponses)

table_alldata_timetopeak <-tapply(t.max,list(f.IDstar2,f.taste2),mean)
df.alldata_timetopeak <- data.frame(table_alldata_timetopeak)
Q_alldata_timetopeak <- cbind(f.cellIDstar2,df.alldata_timetopeak)



#Find all significant responses 

t.all<-2.5 #set threshold
tk<-numeric(t.all)
#MAT=maximum above threshold
MAT5<-numeric(c.all3)
for (k in 1:t.all){
  tk[k]<-paste("thresh",k,sep="")
  TK<-eval(parse(text=tk[k]))
  for (j in 1:c.all3){
    #sig_resp[j,]<-sig[j,which(t.base>t)]
    #FAT[j]<-length(which(sig_resp[j,]>TK[j]))
    MAT5[j]<-ifelse(max_resp_taste[j]>TK[j],1,0)
    test4<-paste("MAT",k,sep="")
    assign(test4,MAT5)
  }
}

#making non threshold responses. responses 0 for time
t.max_responders = length(c.all3)
for (j in 1:c.all3){
  if (MAT5[j]>0) {
    t.max_responders[j]<-t.max[j]
  } else {
    t.max_responders[j]<-0
  }
} # end j

table_responders_time<-tapply(t.max_responders,list(f.IDstar3,f.taste3),mean)
df.responders_time<-data.frame(table_responders_time)
colnames(f.cellIDstar3) <- c("Animal", "f.day","f.trial","f.cell")
Q_responders_time<- cbind(f.cellIDstar3,df.responders_time)

#Save Results
sheets <- list("DeltaF_AllStim" = Q_alldata, "DeltaF_Responders" = Q_responders,
              "DeltaF_Reduced" = Q_reducedresponders,  "Tracked_responders_enhanced" = Q_alldays_allstim_tracked,
               "Novel_cells_enhanced" = Q_alldays_allstim_novel, "Shared_cells_enhanced" = Q_alldays_allstim_shared,
               "Tracked_responders_reduced" = Q_reduced_alldays_allstim_tracked,
               "Novel_cells_reduced" = Q_reduced_alldays_allstim_novel, "Shared_cells_reduced" = Q_reduced_alldays_allstim_shared)
write_xlsx(sheets, "AnimalNumber_Routput.xlsx")


beep(2)

Q_alldays_allstim_tracked_correct <- Q_alldays_allstim_tracked
Q_alldays_allstim_novel_correct <- Q_alldays_allstim_novel
Q_alldays_allstim_shared_correct <- Q_alldays_allstim_shared
Q_reduced_alldays_allstim_tracked_correct <- Q_alldays_allstim_tracked
Q_reduced_alldays_allstim_novel_correct <- Q_alldays_allstim_novel
Q_reduced_alldays_allstim_shared_correct <- Q_alldays_allstim_shared

save.image(file = "AnimalNumber.RData")

