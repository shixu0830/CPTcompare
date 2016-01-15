SufficientData <-
function(cpt){
  cpt$visit = rep(1, nrow(cpt))
  cpt$code_grp = paste(cpt[,"code"],":",cpt[,"numericcode"]
                       ,"_",cpt[,"codegrp"],"#",cpt[,"codegrp.order"],sep="")
  cpt.dt = data.table(cpt)
  dat = data.frame(dcast.data.table(
    cpt.dt, code_grp+site~visit,
    fun=sum,value.var="visit",fill=0,drop=F))
  dat$code = substr(dat$code_grp,start=1,
                    stop=unlist(gregexpr(pattern ='[0-9,A-Z,a-z]:[0-9]',dat$code_grp)))
  dat$numericcode = substr(dat$code_grp,
                           start=unlist(gregexpr(pattern ='[0-9,A-Z,a-z]:[0-9]',dat$code_grp))+2,
                           stop=unlist(gregexpr(pattern ='[0-9]_[A-z,a-z]',dat$code_grp)))
  dat$codegrp = substr(dat$code_grp,
                       start=unlist(gregexpr(pattern ='[0-9]_[A-z,a-z]',dat$code_grp))+2,
                       stop=unlist(gregexpr(pattern ='#[0-9]',dat$code_grp))-1)
  dat$codegrp.order = substr(dat$code_grp,
                             start=unlist(gregexpr(pattern ='#[0-9]',dat$code_grp))+1,
                             stop=max(nchar(dat$code_grp))+1)
  dat$offset.pt = dat$site
  dat$offset.pt[dat$offset.pt==0] = length(unique(cpt$id[cpt$site==0]))
  dat$offset.pt[dat$offset.pt==1] = length(unique(cpt$id[cpt$site==1]))
  dat = data.frame(CPT=dat$numericcode,site=dat$site,offset=dat$offset.pt,
                   block=dat$codegrp.order,count=dat$X1,
                   code=dat$code,codegrp=dat$codegrp)
  dat$block = as.numeric(as.character(dat$block))
  dat = dat[order(dat$block),]
  dat$logN = log(dat$offset)
  
  return(dat)
}


#### old version ####
#   cpt$offset = cpt$site
#   cpt$offset[cpt$offset==0] = length(unique(cpt$id[cpt$site==0]))
#   cpt$offset[cpt$offset==1] = length(unique(cpt$id[cpt$site==1]))
#   
#   
#   dat = summaryBy(visit~numericcode+id  +site+offset+codegrp.order+code+codegrp,
#                   FUN=sum,data=cpt)
#   dat$pt = rep(1,nrow(dat))
#   dat = summaryBy(visit.sum+pt~numericcode  +site+offset+codegrp.order+code+codegrp,
#                   FUN=sum,data=dat)
#   names(dat) = c("numericcode", "site", "offset", "codegrp.order", 
#                  "code","codegrp","visit.sum","pt.sum")
#   dat$site=as.numeric(dat$site) 
#   # make place holders for codes not used in one of the sites
#   codes = unique(dat$numericcode); codes=codes[order(codes)]
#   maxoffset = max(unique(dat$offset)); minoffset = min(unique(dat$offset))
#   #   dat$code = as.character(dat$code)
#   for(i in 1:length(codes)){
#     tmp = dat[which(dat$numericcode==codes[i]),]
#     if(nrow(tmp)==1){
#       site.add=1-as.numeric(tmp[,2])
#       offset.add=minoffset*site.add+maxoffset*(1-site.add)
#       add=c(tmp[,1],site.add,offset.add,tmp[,4],tmp[,5],tmp[,6],0,0)
#       dat=rbind(dat,add)
#     }
#   }
#   dat$offset = as.numeric(dat$offset)
#   dat$visit.sum = as.numeric(dat$visit.sum)
#   dat$codegrp.order = as.numeric(dat$codegrp.order)
#   dat$pt.sum = as.numeric(dat$pt.sum)
#   dat$site = as.numeric(dat$site)
#   dat$numericcode = as.numeric(dat$numericcode)
#   dat=dat[order(dat$site),]
#   dat=dat[order(dat$numericcode),]
#   dat=dat[order(dat$codegrp.order),]
#   
#   
#   groups = unique(dat$codegrp.order)
#   names(dat)=c("CPT","site","offset","block","code","codegrp","count","pt.sum")
#   #   off1 = length(unique(cpt$id[cpt$site==1])) #the smaller site
#   #   off0 = length(unique(cpt$id[cpt$site==0]))
#   #   maxoffset = max(unique(cptglm$offset)); minoffset = min(unique(cptglm$offset))
#   #   # offset same for nonnum and num
#   #   dat$offset[dat$offset==minoffset]=off1; dat$offset[dat$offset==maxoffset]=off0
#   dat$logN = log(dat$offset)
#   #   dat$CPTblock = factor(factor(dat$block):factor(dat$CPT))