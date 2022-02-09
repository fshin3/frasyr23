library(tidyverse)
datafile <- read.csv("~/OneDrive/2-kei-issues/type2_data.csv")

Stocks <-unique(datafile$Stock)

ccdata.stock<-list()
for(i in 1:length(unique(datafile$Stock))){
  ccdata.stock[[i]] <- datafile %>% filter(Stock==Stocks[i])
}

tune.pars <- list()
tune.pars[[1]] <-c(0.5,0.4,0.4)
tune.pars[[2]] <-c(0.1,0.5,0.9)
tune.pars[[3]] <-c(0.4,0.4,0.8)
parameans <- c("default","best","2ndbest")

for(j in 1:length(tune.pars)){

ABCs<-list()
n.catch<-5
for(i in 1:length(Stocks)){
  if(max(ccdata.stock[[i]]$Year)-min(ccdata.stock[[i]]$Year)<=4) next
  cpuetmp <-ccdata.stock[[i]]$CPUE
  cpuetmp <- cpuetmp[-na.omit(cpuetmp)]
  if(length(cpuetmp) <=4 ) next
  ccdata<-data.frame(year=ccdata.stock[[i]]$Year,cpue=ccdata.stock[[i]]$CPUE,catch=ccdata.stock[[i]]$Catch)
  filename<-paste0("~/Desktop/2kei-stocks/",Stocks[i],"_",parameans[j],"_bt5year.png")
  resabc2 <-calc_abc2(ccdata,BTyear=(max(ccdata$year)-4),tune.par = tune.pars[[j]],summary_abc = F)
  #resabc2 <-calc_abc2(ccdata,summary_abc = F)
  graph_abc2 <-plot_abc2_fixHC_seqOut(resabc2)
  ABCs[[i]]<-graph_abc2[[1]]
  ABCs[[i]]$stock <- rep(Stocks[i],nrow(ABCs[[i]]))
  ABCs[[i]]$tunepar <- rep(str_c(paste0(tune.pars[[j]][1],"-",tune.pars[[j]][2],"-",tune.pars[[j]][3])),nrow(ABCs[[i]]))
  ABCs[[i]]$ABCdeviation <- (ABCs[[i]]$ABC-ABCs[[i]]$ABC[1])/ABCs[[i]]$ABC[1]
  ori.catch <- ccdata$catch
  l.catch <- length(ori.catch)
  Catch5yr <- mean(ori.catch[(l.catch-n.catch+1):l.catch],na.rm = TRUE)
  ABCs[[i]]$Catch5yr <- rep(Catch5yr,nrow(ABCs[[i]]))
  ABCs[[i]]$Catch5yrdeviation <- (ABCs[[i]]$ABC-Catch5yr)/Catch5yr
  #names(ABCs[[i]])<-Stocks[i]
  #ggsave(width=420,height=150,dpi=200,units="mm", graph_abc2[[3]],file=filename)
}

if(j==1) ABCsdefault<-ABCs
else if(j==2) ABCs1st<-ABCs
else ABCs2nd<-ABCs
}

save(ABCs1st,file = "./tools/seqOutABCs_bt5best.rda")
save(ABCs2nd,file = "./tools/seqOutABCs_bt52ndbest.rda")
save(ABCsdefault,file = "./tools/seqOutABCs_default.rda")

load("./tools/seqOutABCs_bt5best.rda")
load("./tools/seqOutABCs_bt52ndbest.rda")
load("./tools/seqOutABCs_default.rda")

# check diffs in indices between default and bt5yr opt
ABC1dif <- ABC2dif <- Catch1dif <- Catch2dif <-0
for(i in 1:length(Stocks)){
  if(is.null(ABCs1st[[i]])) next
  for(j in 1:nrow(ABCs1st[[i]])){
     if(ABCs1st[[i]]$ABCdeviation[j]-ABCsdefault[[i]]$ABCdeviation[j] < -0.01) {#print(paste("1st",Stocks[i],ABCs1st[[i]]$label[j]))
    ABC1dif <-ABC1dif+1}
     if((ABCs2nd[[i]]$ABCdeviation[j]-ABCsdefault[[i]]$ABCdeviation[j]) < -0.01) {#print(paste("ABC 2nd",Stocks[i]),ABCs2nd[[i]]$label[j])
    ABC2dif<-ABC2dif+1}
    if((ABCs1st[[i]]$Catch5yrdeviation[j]-ABCsdefault[[i]]$Catch5yrdeviation[j]) < -0.01) {#print(paste("1st",Stocks[i],ABCs1st[[i]]$label[j]))
      Catch1dif <-Catch1dif+1}
    if((ABCs2nd[[i]]$Catch5yrdeviation[j]-ABCsdefault[[i]]$Catch5yrdeviation[j]) < -0.01){ #print(paste("Catch 2nd",Stocks[i]),ABCs2nd[[i]]$label[j])
      Catch2dif <-Catch2dif+1}
  }
}

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
  #write.csv(ABCDevs,file = filename)

  gg.ABCdev <- ggplot(data=data.frame(X=c(-1,1))) +
                geom_point(y=ABCDevs$baseABCdev)

}
