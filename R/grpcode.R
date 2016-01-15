grpcode <-
function(cptnonnum,cptnum){
  
  local({
  load("CCSS.rda")
    })
  groupbound.num = CCSS[[1]]
  groupbound.nonnum = CCSS[[2]]
  
  
  cptnonnum$numericcode = cptnonnum$code
  cptnum$numericcode = as.numeric(as.character(cptnum$code))
  code.nocategory = NULL
  
  ### 1. categorize non-numericc code "cptnonnum"
  if(!is.null(cptnonnum)){
    cptnonnum = merge(cptnonnum,groupbound.nonnum,by="code",all.x=T,all.y=F)
    if(length(  (ind = which(is.na(cptnonnum$codegrp.order)))  )>0){
      msg<-sprintf("Some (character+digits) CPT codes do not fall in any category")
      warning(msg,call.=TRUE)
      code.nocategory = c(code.nocategory, cptnonnum$code[ind])
      cptnonnum = cptnonnum[!is.na(cptnonnum$codegrp.order),]
    }
  }
  
  
  ### 2. categorize numericc code "cpt"
  if(!is.null(cptnum)){
    # NB: min in our data doesnt include "00***"
    cptnum = merge(cptnum,groupbound.num,by="code",all.x=T,all.y=F)
    if(length(  (ind = which(is.na(cptnum$codegrp.order)))  )>0){
      msg<-sprintf("Some (numeric) CPT codes do not fall in any category")
      warning(msg,call.=TRUE)
      code.nocategory = c(code.nocategory, cptnum$code[ind])
      cptnum = cptnum[!is.na(cptnum$codegrp.order),]
    }
  }
  
  if(length(  (code.nocategory=unique(code.nocategory))  )>0){
    code.nocategory=code.nocategory[order(code.nocategory)]
  }
  cpt = rbind(cptnonnum,cptnum)
  cpt$code = as.character(cpt$code)
  return(list(cpt=cpt, code.nocategory=code.nocategory,cptnum=cptnum,cptnonnum=cptnonnum))
  
}




#### old method of categorizing codes into blocks ####
#   cptnonnum$numericcode = cptnonnum$code
#   cptnum$numericcode = as.numeric(as.character(cptnum$code))
#   ### 1. categorize non-numericc code "cptnonnum"
#   cptnonnum$codegrp = rep("NA",nrow(cptnonnum))
#   cptnonnum$codegrp.order = rep(NA,nrow(cptnonnum))
#   groups = unique(groupbound.nonnum$group.order)
#   for (i in 1:length(unique(groups))){
#     ind.groupbound = which(groupbound.nonnum$group.order==groups[i])
#     tmp.ind = cptnonnum$numericcode %in% groupbound.nonnum$upper[ind.groupbound] # upper=lower
#     if(length(which(tmp.ind==T))>0){
#       cptnonnum$codegrp[tmp.ind] = groupbound.nonnum$groupname[ind.groupbound[1]]
#       cptnonnum$codegrp.order[tmp.ind] = groups[i]
#     }
#     #print(i)
#   }
#   cptnonnum = cptnonnum[-(which(cptnonnum$codegrp=="NA")),]
#   
#   ### 2. categorize numericc code "cpt"
#   cptnum$codegrp = rep("NA",nrow(cptnum))
#   cptnum$codegrp.order = rep(NA,nrow(cptnum))
#   # NB: min in our data doesnt include "00***"
#   groupind = list()
#   for (i in 1:length(groupbound.num$upper)){
#     groupind[[i]] = which(cptnum$numericcode>=groupbound.num$lower[i]&
#                             cptnum$numericcode<=groupbound.num$upper[i])
#     cptnum$codegrp[groupind[[i]]] = groupbound.num$groupname[i]
#     cptnum$codegrp.order[groupind[[i]]] = groupbound.num$group.order[i]
#     if(i %% 20 == 1){
#       print(paste0("Categorizing CPT codes, ", round(100*i/length(groupbound.num$upper)),
#                    "% done"))
#     } 
#   }
