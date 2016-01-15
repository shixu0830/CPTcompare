RRelasticnet <-
function(dat, alpha){
  x = model.matrix( ~ factor(dat$CPT)*dat$site + factor
                    (dat$block)*dat$site )
  # the structure of the x matrix:
  # intercept; code2-2378; site; blk2-191; code2-2378*site; blk2-191*site
  # new: intercept; code2-2450; site; blk2-194; code2-2450*site; blk2-194*site
  n.code = length(unique(dat$CPT))
  n.blk = length(unique(dat$block))
  
  ## the codes chosen to be ref
  ## excludes the blk whose code is alr ref in x 
  # first find the code that is alr ref (commonly the min code in data)
  digit.ind.list = gregexpr(pattern ='[0-9]',colnames(x))
  x.code = as.numeric(
    substr(colnames(x),
           start=lapply(digit.ind.list,FUN=function(x){x[1]}),
           stop=lapply(digit.ind.list,FUN=function(x){n=length(x);x[n]}))
    )
  ind.block = which(unlist(gregexpr(pattern ='block',colnames(x)))>0)
  x.code[ind.block] = NA
  
  x.alr_refcode = as.numeric(setdiff(unique(dat$CPT), x.code))
  # second find which blk the code blong to
  x.alr_refblk = cpt$codegrp.order[which(cpt$numericcode == x.alr_refcode)[1]]
  
  refcodes = dat$CPT[sapply(setdiff(unique(dat$block),x.alr_refblk),
                            FUN = function(y){which(dat$block==y)[1]})]
  # delete ref codes
  refcodes = as.numeric(as.character(refcodes))
  code.del = which(x.code %in% refcodes)
  x = x[,-c(code.del)] # check: dim(x)[1] should = dim(x)[2]
  y = dat$count
  penalty = rep(1,ncol(x))
  # no penalty on site
  penalty[c(which(colnames(x)=="site"))]=0
  
  ## exclude the intercept 
  ## less penalty on anything that has block in it
  x.hp = x[,-1]
  blah = grep("block",colnames(x))
  penalty[blah]=0.5
  fitL = glmnet( x.hp, y, family="poisson", offset = dat$logN, alpha=alpha
                 ,penalty.factor=penalty
  )
  bbb = fitL$beta[,95] #hp
  lp = as.vector( x.hp%*%c(bbb)) + fitL$a[95]
  rrr_big = (lp[dat$site==1]) - (lp[dat$site==0])
  
  return(rrr_big)
}
