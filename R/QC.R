QC <-
function(id,code,site){
  # check same length of the input variables
  if(length(id)!=length(code)){
    stop("IDs and CPT codes have different lengths")
  } else if(length(code)!=length(site)){
    stop("Sites and CPT codes have different lengths")
  } else if(length(site)!=length(id)){
    stop("Sites and IDs have different lengths")
  }
  
  # check NA in data
  cpt = data.frame(id,code,site)
  cpt$code = as.character(code)
  if(length( (msgind=which(is.na(id)))==T )>0){
    msg<-sprintf("Observations with NA's in ID deleted")
    warning(msg,call.=TRUE)
    cpt = cpt[-msgind,]
  } else if(length( (msgind=which(is.na(code)))==T )>0){
    msg<-sprintf("Observations with NA's in code deleted")
    warning(msg,call.=TRUE)
    cpt = cpt[-msgind,]
  } else if(length( (msgind=which(is.na(site)))==T )>0){
    msg<-sprintf("Observations with NA's in site deleted")
    warning(msg,call.=TRUE)
    cpt = cpt[-msgind,]
  }
  
  # check format of site variable
  u.site = unique(site)
  if(length(u.site)==1){
    stop("Only one value in site")
  }else if(length(u.site)>2){
    stop("More than two values in site")
  }else if( (which(u.site==1)+which(u.site==0)) != 3){
    stop("Site is a vector of 0 and 1. Please check values of site")
  }
  
  # check code digits length
  if(length(  (msgind = which(nchar(as.character(cpt$code))>5))   )>0){
    msg<-sprintf("CPT code longer than 5 digits dropped")
    warning(msg,call.=TRUE)
    cpt = cpt[-msgind,]
  } else if(length(   (msgind = which(nchar(as.character(cpt$code))<5))   )>0){
    msg<-sprintf("CPT code shorter than 5 digits dropped")
    warning(msg,call.=TRUE)
    cpt = cpt[-msgind,]
  }
  
  # check numeric code range
  nonnumericind = grep("[A-Z,a-z]",as.character(cpt$code))
  tmp = cpt[nonnumericind,]
  if(length(nonnumericind)>0 & length(nonnumericind)!=nrow(cpt)){
    cptnonnum = cpt[nonnumericind,]
    cptnonnum$numericcode = as.character(cptnonnum$code) # keep char in cptnonnum
    cptnum = cpt[-nonnumericind,]
    cptnum$numericcode = as.numeric(as.character(cptnum$code)) # char->num in cptnum
    if(length(which(cptnum$numericcode>99608))>0 | 
         length(which(cptnum$numericcode<as.numeric(00100)))>0){
      msg<-sprintf("CPT codes category I out of range (00100, 99607)")
      warning(msg,call.=TRUE)
      cptnum = cptnum[-which(cptnum$numericcode>99607 | cptnum$numericcode<as.numeric(00100)),] # out of range of cpt codes
    }
  } else if(length(nonnumericind)==0){
    cptnum = cpt
    cptnum$numericcode = as.numeric(as.character(cptnum$code)) # char->num in cptnum
    cptnonnum = NULL
    if(length(which(cptnum$numericcode>99608))>0 | 
         length(which(cptnum$numericcode<as.numeric(00100)))>0){
      msg<-sprintf("CPT codes category I out of range (00100, 99607)")
      warning(msg,call.=TRUE)
      cptnum = cptnum[-which(cptnum$numericcode>99607 | cptnum$numericcode<as.numeric(00100)),] # out of range of cpt codes
    }
  } else if(length(nonnumericind)==nrow(cpt)){
    cptnonnum = cpt
    cptnonnum$numericcode = as.character(cptnonnum$code) # keep char in cptnonnum
    cptnum = NULL
  }
  
  
  # return cpt data
  cpt = rbind(cptnum, cptnonnum)  
  return(list(cpt=cpt,cptnum=cptnum,cptnonnum=cptnonnum))
}
