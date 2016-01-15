renameNonnumeric <-
function(cpt){
  code.all = cpt$code
  cpt$numericcode = as.character(cpt$code)
  nonnumericind = grep("[A-Z,a-z]",as.character(code.all))
  uni.nonnumcode = unique(code.all[nonnumericind])
  n = length(uni.nonnumcode)
  if(n>0){
    for (i in 1:n){
      cpt$numericcode[which(code.all==uni.nonnumcode[i])]=99607+i
    }
  }
  cpt$numericcode = as.numeric(cpt$numericcode)
  cpt$id = as.numeric(cpt$id)
  
  # check non-integer codes
  if(length(which((cpt$numericcode*100)%%100 != 0))>0){
    msg<-sprintf("CPT code is not an integer: deleted")
    warning(msg,call.=TRUE)
    cpt = cpt[-which((cpt$numericcode*100)%%100 != 0),]
  }
  cpt = cpt[order(cpt$codegrp.order),]
  
  return(cpt)
}
