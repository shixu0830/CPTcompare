all_count_Pval = function(cpt,testmethod = "NBLRT"){
  cpt$code_grp = paste(cpt[,"numericcode"],"_",cpt[,"codegrp.order"],sep="")
  ## gen sufficient stat (count or pt) based data
  ## pt level offset: count of pt with assignment per site
  cpt$offset.pt = cpt$site
  cpt$offset.pt[cpt$offset.pt==0] = length(unique(cpt$id[cpt$site==0]))
  cpt$offset.pt[cpt$offset.pt==1] = length(unique(cpt$id[cpt$site==1]))
  ## (3.1) sum visit by id*code (site covered by id) 
  cpt$visit = rep(1, nrow(cpt))
  cpt.dt = data.table(cpt)
  ## sum of visits per pt per code over time
  cpt.dt_ptcode = cpt.dt[,list(visit.sum = sum(visit)), 
                         by = c("id","numericcode","code","site",
                                "offset.pt","codegrp.order","codegrp","code_grp")] 
  ptcode = cpt.dt_ptcode[,c("site","offset.pt","id", 
                            "code_grp","visit.sum"),with=F]
  ## reshape to pt level code counts
  ptcode = dcast.data.table(ptcode,id+site+offset.pt~code_grp, 
                            value.var="visit.sum",drop=T,fill=0)
  site=ptcode$site
  y=as.matrix(ptcode)[,c(4:ncol(ptcode))]
  numericcode = substr(names(ptcode),start=1,
                       stop=unlist(gregexpr(pattern ='_',names(ptcode)))-1)[c(4:ncol(ptcode))]
  
  n1 = length(which(site==1)); n2 = length(which(site==0))
  n.lambda = ncol(y)
  y1 = y[site==1,]; y2 = y[site==0,]
  s.y1 = apply(y1,2,sum)
  s.y2 = apply(y2,2,sum)
  bin.y1 = apply( y1,2,FUN=function(x){ as.numeric(x>0) } ) # binary data, dim = (n1)*length(lambda)
  bin.y2 = apply( y2,2,FUN=function(x){ as.numeric(x>0) } ) # binary data, dim = (n2)*length(lambda)
  bin.y = apply( y,2,FUN=function(x){ as.numeric(x>0) } ) #rbind(bin.y1,bin.y2)
  s.bin.y1=apply(bin.y1,2,sum);s.bin.y2=apply(bin.y2,2,sum)
  
  if(method=="NBLRT"){
    Neg.log.like_alte = function (x,param,site){ 
      intercept = param[1]
      slope = param[2]
      size = param[3]
      mu = exp(intercept + slope*site)
      out = -sum(log(dnbinom(x = x, size = size, mu = mu)))
      if(!is.finite(out)) {out = 1e+200}
      return(out)
    }
    Neg.log.like_null = function (x,param){ 
      intercept = param[1]
      size = param[2]
      mu = exp(intercept)
      out = -sum(log(dnbinom(x = x, size = size, mu = mu)))
      if(!is.finite(out)) {out = 1e+200}
      return(out)
    }
    NB_byhand = apply(y, 2, FUN=function(x){
      m <- try(glm.nb(x~factor(site),data=data.frame(x,site),
                      control=list(maxit = 100,epsilon = 1e-8,trace=F)), silent=TRUE)
      if(inherits(m, "try-error")){
        Negloglik_alte = try(optim(par=c(0.5,0.5,0.5),fn=Neg.log.like_alte,
                                   method="L-BFGS-B",x=x,site=site,
                                   lower=c(-Inf,-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        Negloglik_null = try(optim(par=c(0.5,0.5),fn=Neg.log.like_null,
                                   method="L-BFGS-B",x=x,
                                   lower=c(-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        if(inherits(Negloglik_alte, "try-error") | inherits(Negloglik_null, "try-error")){
          list(dev=0,error=2)
        }else{
          list(dev=2*(Negloglik_null$value-Negloglik_alte$value),error=1)
        }
      }else {
        list(dev=m$null.deviance-m$deviance,error=0)
      }
    })
    T_NB_byhand = as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(1,2*n.lambda,by=2)]
    #error = as.numeric(as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(2,2*n.lambda,by=2)])
    p = pchisq(unlist(T_NB_byhand),df=1,lower.tail = F)
  }else if(method=="PoisLRT"){
    model_alte_pois = apply(y, 2, FUN=function(x){
      glm(x~factor(site),data=data.frame(x,site),family="poisson",
          maxit = 50,epsilon = 1e-8,trace=F)
    })
    deviance_alte_pois = unlist(lapply(model_alte_pois,FUN=function(x){x$deviance}))
    deviance_alte.null_pois = unlist(lapply(model_alte_pois,FUN=function(x){x$null.deviance}))
    T_NB_byhand2_pois = deviance_alte.null_pois-deviance_alte_pois # same as LRbinom below
    p = pchisq(T_NB_byhand2_pois,df=1,lower.tail = F)
  }else if(method=="BinLRT"){
    model_alte_bin = apply(bin.y, 2, FUN=function(x){
      glm(x~factor(site),data=data.frame(x,site),family="binomial",
          maxit = 50,epsilon = 1e-8,trace=F)
    })
    deviance_alte_bin = unlist(lapply(model_alte_bin,FUN=function(x){x$deviance}))
    deviance_alte.null_bin = unlist(lapply(model_alte_bin,FUN=function(x){x$null.deviance}))
    T_NB_byhand_bin = deviance_alte.null_bin-deviance_alte_bin # same as LRbinom below
    p = pchisq(T_NB_byhand_bin,df=1,lower.tail = F)
  }else if(method=="NBExact"){
    # ## NB exact test
    myexactTestDoubleTail_New = function(s.y1,s.y2,dispersion=0,n1,n2)
    {
      s.y1 = round(s.y1)
      s.y2 = round(s.y2)
      ncodes = length(s.y1)
      if(length(dispersion)==1) dispersion = rep(dispersion,ncodes)
      
      #  Null fitted values
      s = s.y1+s.y2
      r = size = 1/dispersion # same dispersion param for two sites
      mu = (s.y1+s.y2)/(n1+n2)
      # "mu" is lambda under the null; dont use (s.y1/n1+s.y2/n2)/2
      size1 = n1/dispersion; size2=n2/dispersion; sizes = (n1+n2)/dispersion
      mu1 = n1*mu; mu2=n2*mu; mus = (n1+n2)*mu
      
      p.bot = p.top = pvals = rep(10,ncodes)
      
      left = s.y1<=mu1
      if(any(left)) {
        p.bot[left] <- dnbinom(s[left],size=sizes[left],mu=mus[left])
        for (g in which(left)) {
          x = 0:s.y1[g]
          tmp.p.top = dnbinom(x,size=size1[g],mu=mu1[g]) * 
            dnbinom(s[g]-x,size=size2[g],mu=mu2[g])
          p.top[g] = 2*sum(tmp.p.top)
        }
        pvals[left] = p.top[left]/p.bot[left]
      }
      right = s.y1>=mu1
      if(any(right)) {
        p.bot[right] = dnbinom(s[right],size=sizes[right],mu=mus[right])
        for (g in which(right)) {
          x = s.y1[g]:s[g]
          tmp.p.top = dnbinom(x,size=size1[g],mu=mu1[g]) * 
            dnbinom(s[g]-x,size=size2[g],mu=mu2[g])
          p.top[g] = 2*sum(tmp.p.top)
        }
        pvals[right] = p.top[right]/p.bot[right]
      }
      pmin(pvals,1)
    }
    ## estimate code-wise phi
    NB_byhand = apply(y, 2, FUN=function(x){
      m <- try(glm.nb(x~factor(site),data=data.frame(x,site),
                      control=list(maxit = 100,epsilon = 1e-8,trace=F)), silent=TRUE)
      if(inherits(m, "try-error")){
        Negloglik_alte = try(optim(par=c(0.5,0.5,0.5),fn=Neg.log.like_alte,
                                   method="L-BFGS-B",x=x,site=site,
                                   lower=c(-Inf,-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        Negloglik_null = try(optim(par=c(0.5,0.5),fn=Neg.log.like_null,
                                   method="L-BFGS-B",x=x,
                                   lower=c(-Inf,0),
                                   control=list(maxit=1000)), silent=TRUE)
        if(inherits(Negloglik_alte, "try-error") | inherits(Negloglik_null, "try-error")){
          #moment est of theta under the null..
          #have to assume heterogeneity bc under alternative cant converge
          size.m = try(theta.mm(x, mu=mean(x),dfr=length(site)-1), silent=T)
          if(inherits(size.m, "try-error")){return(list(dev=0,error=T,size=1e200))}else{
            return(list(dev=0,error=T,size=size.m))}
        }else{
          list(dev=2*(Negloglik_null$value-Negloglik_alte$value),error=T,
               size=Negloglik_alte$par[3])}
      }else{
        list(dev=m$null.deviance-m$deviance,error=F,size=m$theta)
      }
    })
    size_alte = as.numeric(as.data.frame(NB_byhand[1:length(NB_byhand)])[,seq(3,3*n.lambda,by=3)])
    Negbin.y1=s.y1; Negbin.y2=s.y2
    p = myexactTestDoubleTail_New(Negbin.y1,Negbin.y2, dispersion=1/(size_alte),n1,n2)
    #note: size_alte instead of size_null*norm cuz this time I used pt-level data to est 
    #code-wise dispersion parameter (captures variability within elements of sumation for s.y1)
  }else if(method=="PoisExact"){
    p=apply(cbind(s.y2,s.y1),1,FUN=function(x){
      poisson.test(x,c(n2,n1),r=1,alternative="two.sided")$p.value
    })
  }else if(method=="FishersExact"){
    table = data.frame(s.bin.y1,m1=n1-s.bin.y1,s.bin.y2,m2=n2-s.bin.y2)
    p = apply(table,1,FUN=function(x){
      fisher.test(matrix(x,nrow=2,ncol=2),or=1,alternative = "two.sided")$p.value
    })
  }else if(method=="PredSiteLRT"){
    Pred_site = apply(y, 2, FUN=function(x){
      m <- try(glm(site~x,data=data.frame(x,site),
                   control=list(maxit = 50,epsilon = 1e-8,trace=F),
                   family="binomial"), silent=TRUE)
      list(dev=m$null.deviance-m$deviance,error=F)
    })
    T_Pred_site = as.data.frame(Pred_site[1:length(Pred_site)])[,seq(1,2*n.lambda,by=2)]
    p = pchisq(unlist(T_Pred_site),df=1,lower.tail = F)
  }else if(method=="Ttest"){
    p = apply(y, 2, FUN=function(x){
      myttest = t.test(x~site)
      if(is.nan(myttest$p.value)){
        1
      }else{
        myttest$p.value
      }
    })
  }
  
  manhattan.data = data.frame(code=numericcode,
                              p,method=method)
  manhattan.data$p.trunc = manhattan.data$p
  if(length(manhattan.data$p.trunc[which(manhattan.data$p.trunc<1e-17)])>0){
    manhattan.data$p.trunc[which(manhattan.data$p.trunc<1e-17)] = 1e-17
  }
  if(length(manhattan.data$p.trunc[which(manhattan.data$p.trunc>1)])>0){
    manhattan.data$p.trunc[which(manhattan.data$p.trunc>1)] = 1
  }
  ## return in manhattan.data form for merging
  return(manhattan.data)
}