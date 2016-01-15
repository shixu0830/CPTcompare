PData <-
function(cpt,dat,method="Elasticnet",alpha=0.5,testmethod="NBLRT"){
  if(method == "GLMM"){
    rrr = RRmix(dat)
    name="Generalized linear mixed model"
  }else if(method == "Elasticnet"){
    rrr = RRelasticnet(dat,alpha=0.5)
    name="Penalized Regression (elastic net)"
  }else if(method == "Lasso"){
    rrr = RRelasticnet(dat,alpha=1)
    name="Lasso Regression"
  }else if(method == "Ridge"){
    rrr = RRelasticnet(dat,alpha=0)
    name="Ridge Regression"
  }else{
    stop("Error in method")
  }
  
  ### compute p value use your choice of testmethod
  manhattan.data = all_count_Pval(cpt,testmethod = "NBLRT")
  # generate plot data: pdata
  pdata = data.frame(RR=exp(rrr)  , log2RR=rrr, 
                     block=as.numeric(as.character(dat$block[dat$site==1])),
                     CPT=as.numeric(as.character(dat$CPT[dat$site==1])),
                     truecode = dat$code[dat$site==1],
                     codegrp = dat$codegrp[dat$site==1])
  pdata = merge(pdata,manhattan.data[,c("code","p.trunc")],by.x="CPT",by.y="code")
  
  # significance level
  ## significance level
  pdata$level = cut(pdata$p.trunc,breaks=c(1.1,0.05,0.01,
                                           0.05/nrow(pdata),0),right=F)
  t = levels(pdata$level); t = strsplit(t, "[[*,*)]")
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[2]=="0"}))] = "sig. (Bonferroni)"
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="0.01"}))] = "0.01 - sig."
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="0.05"}))] = "0.05 - 0.01"
  levels(pdata$level)[unlist(lapply(t,FUN=function(x){x[3]=="1.1"}))] = "> 0.05"
  
  pdata$p = sprintf("%1.2e", pdata$p.trunc)
  names(pdata)=c("CPT","RR","log2RR","block","code","codegrp","p.trunc","level","P")
  
  
  # extra info to show: #code assignment / #pt(who had this code)
  pdata$rawdata1 = c(); pdata$rawdata2 = c();
  off0 = length(unique(cpt$id[cpt$site==0]))
  off1 = length(unique(cpt$id[cpt$site==1]))
  for(i in 1:nrow(pdata)){
    tmp = dat[dat$CPT==pdata$CPT[i],]
    pdata$rawdata1[i] = paste(tmp$count[which(tmp$site==0)],off0,sep="/")
    pdata$rawdata2[i] = paste(tmp$count[which(tmp$site==1)],off1,sep="/")
  }
  pdata$extra = paste(pdata$codegrp,", RR = ", formatC(pdata$RR,digits=3), ", "
                      ,"Raw data = ", pdata$rawdata1, "; ", pdata$rawdata2, sep="")
  pdata = pdata[order(pdata$block),]
  
  if(testmethod=="NBLRT"){
    testmethod="Neg-Bin LR Test"
  }else if(testmethod=="NBExact"){
    testmethod="Neg-Bin Exact Test"
  }else if(testmethod=="PoisLRT"){
    testmethod="Poisson LR Test"
  }else if(testmethod=="PoisExact"){
    testmethod="Poisson Exact Test"
  }else if(testmethod=="BinLRT"){
    testmethod="Binomial LR Test"
  }else if(testmethod=="FishersExact"){
    testmethod="Fisher's Exact Test"
  }else if(testmethod=="Ttest"){
    testmethod="T-test"
  }
  
  return(list(pdata,name=paste(name," and ",testmethod, sep="")))
}
