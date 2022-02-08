library(tidyverse)
datafile <- read.csv("~/SkyDrive/2-kei-issues/type2_data.csv")

Stocks <-unique(datafile$Stock)

ccdata.stock<-list()
for(i in 1:length(unique(datafile$Stock))){
  ccdata.stock[[i]] <- datafile %>% filter(Stock==Stocks[i])
}
ABCs<-list()
n.catch<-5
for(i in 1:length(Stocks)){
  if(max(ccdata.stock[[i]]$Year)-min(ccdata.stock[[i]]$Year)<=4) next
  cpuetmp <-ccdata.stock[[i]]$CPUE
  cpuetmp <- cpuetmp[-na.omit(cpuetmp)]
  if(length(cpuetmp) <=4 ) next
  ccdata<-data.frame(year=ccdata.stock[[i]]$Year,cpue=ccdata.stock[[i]]$CPUE,catch=ccdata.stock[[i]]$Catch)
  filename<-paste0("~/Desktop/2kei-stocks/",Stocks[i],"_2ndbest_bt5year.png")
  resabc2 <-calc_abc2(ccdata,BTyear=(max(ccdata$year)-4),tune.par = c(0.4,0.4,0.8),summary_abc = F)
  #resabc2 <-calc_abc2(ccdata,summary_abc = F)
  graph_abc2 <-plot_abc2_fixHC_seqOut(resabc2)
  ABCs[[i]]<-graph_abc2[[1]]
  ABCs[[i]]$stock <- rep(Stocks[i],nrow(ABCs[[i]]))
  ABCs[[i]]$tunepar <- rep(str_c("0.4-0.4-0.8"),nrow(ABCs[[i]]))
  ABCs[[i]]$ABCdeviation <- (ABCs[[i]]$ABC-ABCs[[i]]$ABC[1])/ABCs[[i]]$ABC[1]
  ori.catch <- ccdata$catch
  l.catch <- length(ori.catch)
  Catch5yr <- mean(ori.catch[(l.catch-n.catch+1):l.catch],na.rm = TRUE)
  ABCs[[i]]$Catch5yr <- rep(Catch5yr,nrow(ABCs[[i]]))
  ABCs[[i]]$Catch5yrdeviation <- (ABCs[[i]]$ABC-Catch5yr)/Catch5yr
  #names(ABCs[[i]])<-Stocks[i]
  #ggsave(width=420,height=150,dpi=200,units="mm", graph_abc2[[3]],file=filename)
}

#ABCs1st<-ABCs
#ABCs2nd<-ABCs
#ABCsdefault<-ABCs
save(ABCs1st,file = "./tools/seqOutABCs_bt5best.rda")
save(ABCs2nd,file = "./tools/seqOutABCs_bt52ndbest.rda")
save(ABCsdefault,file = "./tools/seqOutABCs_default.rda")

load("./tools/seqOutABCs_bt5best.rda")
load("./tools/seqOutABCs_bt52ndbest.rda")
load("./tools/seqOutABCs_default.rda")


for(i in 1:length(Stocks)){
  if(is.null(ABCs1st[[i]])) next
  labels <-ABCs1st[[i]]$label
  defaultABCdev<-ABCsdefault[[i]]$ABCdeviation
  defaultCatchdev<-ABCsdefault[[i]]$Catch5yrdeviation
  bt51stABCdev<-ABCs1st[[i]]$ABCdeviation
  bt51stCatchdev<-ABCs1st[[i]]$Catch5yrdeviation
  bt52ndABCdev<-ABCs2nd[[i]]$ABCdeviation
  bt52ndCatchdev<-ABCs2nd[[i]]$Catch5yrdeviation
  ABCDevs<-data.frame(label=labels,baseABCdev=defaultABCdev,fix1ABCdev=bt51stABCdev,fix2ABCdev=bt52ndABCdev,baseCatchdev=defaultCatchdev,fix1Catchdev=bt51stCatchdev,fix2Catchdev=bt52ndCatchdev)
  filename<-paste0("~/Desktop/2kei-stocks/",Stocks[i],".csv")
  write.csv(ABCDevs,file = filename)

  gg.ABCdev <- ggplot(data=data.frame(X=c(-1,1))) +
                geom_point(x=ABCDevs$label,y=ABCDevs$baseABCdev)

}
