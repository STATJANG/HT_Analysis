link_sp = function(x,c_sp=1){
  max(0,x)+c_sp*log(1+exp(-abs(x/c_sp)))
}
##used for likelihood calculation.
compute_nu = function(dat,model = list(past_mean=1,past_obs=1),d,a=NULL,b,xbeta=NULL,xreg=NULL){
  n = length(dat)
  nu = rep(0,n)
  max.q = max(model[["past_obs"]])
  if( is.null(xreg) ){ xbeta = 0; xreg = as.matrix(rep(0,n)) }
  
  nu[1:max.q] = mean(log(dat+1)) #d + xreg[1:max.q,,drop=F]%*%xbeta
  if(is.null(model[["past_mean"]])){#ARCH
    for(tt in (max.q+1):n){
      nu[tt] = d + sum(b*log(dat[tt-model[["past_obs"]]]+1)) + sum(xbeta*xreg[tt,]) }
  }else{#GARCH
    for(tt in (max.q+1):n){
      nu[tt] = d + sum(a*nu[tt-model[["past_mean"]]]) + sum(b*log(dat[tt-model[["past_obs"]]]+1)) + sum(xbeta*xreg[tt,]) }  
  }
  return(nu)
}
##used for likelihood calculation.
compute_lam_sp = function(dat,model = list(past_mean=1,past_obs=1),d,a=NULL,b,xbeta=NULL,xreg=NULL, c_sp){
  n = length(dat)
  lam = rep(0,n)
  max.q = max(model[["past_obs"]])
  if( is.null(xreg) ){ xbeta = 0; xreg = as.matrix(rep(0,n)) }
  
  if(is.null(model[["past_mean"]])){ #ARCH
    
    lam[1:max.q] = link_sp(d + xreg[1:max.q,,drop=F]%*%xbeta, c_sp) #link_sp(mean(dat),c_sp)   
    for(tt in (max.q+1):n){
      lam[tt] =link_sp( d + sum(b*dat[tt-model[["past_obs"]]]) + sum(xbeta*xreg[tt,]) ,c_sp ) }
  }else{  #GARCH
    lam[1:max.q] = link_sp(mean(dat),c_sp) #link_sp(d + sum(xbeta*xreg[1,]),c_sp) 
    for(tt in (max.q+1):n){
      lam[tt] =link_sp( d + sum(a*lam[tt-model[["past_mean"]]]) + sum(b*dat[tt-model[["past_obs"]]]) + sum(xbeta*xreg[tt,]) ,c_sp ) }
  }
  return(lam)
}



###DATA GENERATION
###Data generation mainly for parametric bootstrapping.
gen_dat_sp = function(n,model = list(past_mean=1,past_obs=1),disp,d,a=0,b,starting_lam,c_sp=1,xbeta=NULL,xreg=NULL){
  if(!is.null(xreg)&!is.matrix(xreg)){stop("xreg should be a matrix.")}
  if( is.null(xreg) ){ xbeta = 0; xreg = as.matrix(rep(0,n)) }
  max.q = max( model[["past_obs"]] )
  lam = dat = rep(NA,n)
  
  lam[1:max.q] = starting_lam #link_sp(d + xreg[1:max.q,,drop=F]%*%xbeta, c_sp)  
  dat[1:max.q] = rnbinom(max.q,mu=lam[1:max.q],size=disp)  #first q observations.
  if(is.null(model[["past_mean"]])){    
    for(tt in (max.q+1):n){  ###ARCH
      lam[tt] = link_sp(x = d + sum(b*dat[tt-model[["past_obs"]]]) + sum(xbeta*xreg[tt,]) ,c_sp)[1]
      dat[tt] = rnbinom(1,mu=lam[tt],size=disp) #size=disp.param  
    }
  }else{  
    for(tt in (max.q+1):n){  ##GARCH
      lam[tt] = link_sp(x = d + sum(a*lam[tt-model[["past_mean"]]]) + sum(b*dat[tt-model[["past_obs"]]]) + sum(xbeta*xreg[tt,]) ,c_sp)[1]
      dat[tt] = rnbinom(1,mu=lam[tt],size=disp) #size=disp.param  
    }
  }
  return(dat)
}


###Data generation mainly for parametric bootstrapping.
gen_dat_log = function(n,model = list(past_mean=1,past_obs=1),disp,d,a=0,b,starting_nu,xbeta=NULL,xreg=NULL){
  if(!is.null(xreg)&!is.matrix(xreg)){stop("xreg should be a matrix.")}
  if( is.null(xreg) ){ xbeta = 0; xreg = as.matrix(rep(0,n)) }
  max.q = max( model[["past_obs"]] )
  nu = dat = rep(NA,n)
  
  nu[1:max.q] = starting_nu 
  dat[1:max.q] = rnbinom( n = max.q, mu= exp(starting_nu),size=disp ) #first q observations.
  if(is.null(model[["past_mean"]])){    
    for(tt in (max.q+1):n){  ###ARCH
      nu[tt] = d + sum(b*log(dat[tt-model[["past_obs"]]]+1)) + sum(xbeta*xreg[tt,])
      dat[tt] = rnbinom(1,mu = exp(nu[tt]), size=disp) #size=disp.param  
    }
  }else{  
    for(tt in (max.q+1):n){  ##GARCH
      nu[tt] = d + sum(a*nu[tt-model[["past_mean"]]]) + sum(b* log(dat[tt-model[["past_obs"]]]+1 )) + sum(xbeta*xreg[tt,])
      dat[tt] = rnbinom(1,mu = exp(nu[tt]),size=disp) #size=disp.param  
    }
  }
  return(dat)
}














NB_lik = function(dat,model,theta,p = 0, q, xreg, link, c_sp=1){ 
  if( is.null(xreg) ){ xbeta = NULL }else{xbeta = theta[-(1:(2+p+q))]}
  if( is.null(model[["past_mean"]]) ){ a = NULL }else{a = theta[(1:p)+2+q] } 
  
  if(link=="sp" ){lam = compute_lam_sp(dat=dat,model = model, d = theta[2], b = theta[(1:q)+2], a = a, xbeta=xbeta, xreg=xreg , c_sp = c_sp)}
  if(link=="log"){lam = exp(compute_nu(dat=dat,model = model, d = theta[2], b = theta[(1:q)+2], a = a, xbeta=xbeta, xreg=xreg))}
  
  result = dnbinom(x=dat, mu = lam, size=theta[1],log=T)
  return(-sum(result[-(1:q)]) ) #n_size - q
}


NB_ml = function(dat,init=NULL, model = list(past_mean=1,past_obs=1),link ,xreg=NULL, c_sp=1,op_method="BFGS"){
  n = length(dat)
  p = length(model[["past_mean"]])
  q = length(model[["past_obs"]])
  max.q = max(model[["past_obs"]])
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = init[-(1:(2+p+q))]}
  
  if(is.null(init)){
    init = tscount::tsglm(dat,model=model,xreg=xreg, link="log", distr="nbinom")
    init = c(1/init$sigmasq,init$coef)
  }
  
  tmp = optim(par = init, fn = NB_lik, 
              dat = dat, xreg=xreg, c_sp = c_sp, link=link, 
              model = model,p = p, q = max.q ,method=op_method,hessian = T)
  cur.par = tmp$par
  #print(cur.par)
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = cur.par[-(1:(2+p+q))]}
  
  if(link=="sp"){
    cur.lam = compute_lam_sp(dat=dat,model=model, d = cur.par[2], b = cur.par[(1:q)+2], a = cur.par[(1:p)+2+q],xbeta=xbeta, xreg=xreg, c_sp = c_sp ) }
  else{
    cur.lam = exp(compute_nu(dat=dat,model=model, d = cur.par[2], b = cur.par[(1:q)+2], a = cur.par[(1:p)+2+q],xbeta=xbeta, xreg=xreg) )
  }
  
  lik.vec = -tmp$value
  
  pearson = ( dat - cur.lam )/sqrt(cur.lam)
  pearson = pearson[-(1:max.q)]
  
  return(list(estimates = cur.par,log.lik= lik.vec, data = dat, disp=cur.par[1],
              d = cur.par[2],b = cur.par[(1:q)+2],a = cur.par[(1:p)+2+q],xbeta=xbeta,xreg=xreg,
              fitted_value =  cur.lam[-(1:max.q)] ,pearson=pearson,aic = -2*lik.vec + 2*length(init), bic = -2*lik.vec + log(n)*length(init),
              model=model,sp_par = c_sp, hessian=tmp$hessian, link=link))
}


NB_ml_ARCH = function(dat,init=NULL,model = list(past_mean=NULL,past_obs=1),link ,xreg=NULL, c_sp=1,op_method="BFGS"){
  n = length(dat)
  p = length(model[["past_mean"]])
  q = length(model[["past_obs"]])
  max.q = max(model[["past_obs"]])
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = init[-(1:(2+p+q))]}
  
  if(is.null(init)){
    init = tscount::tsglm(dat,model=model,xreg=xreg, link="log", distr="nbinom")
    init = c(1/init$sigmasq,init$coef)
  }
  
  tmp = optim(par = init, fn = NB_lik, 
              dat = dat, xreg=xreg, c_sp = c_sp, link=link, 
              model = model,p = p, q = max.q ,method=op_method,hessian = T)
  cur.par = tmp$par
  #print(cur.par)
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = cur.par[-(1:(2+p+q))]}
  
  if(link=="sp"){
    cur.lam = compute_lam_sp(dat=dat, model=model, d = cur.par[2], b = cur.par[(1:q)+2], xbeta=xbeta, xreg=xreg, c_sp = c_sp ) }
  else{
    cur.lam = exp(compute_nu(dat=dat, model=model, d = cur.par[2], b = cur.par[(1:q)+2], xbeta=xbeta, xreg=xreg) )
  }
  
  lik.vec = -tmp$value
  pearson = ( dat - cur.lam )/sqrt(cur.lam)
  pearson = pearson[-(1:max.q)]
  
  return(list(estimates = cur.par,log.lik= lik.vec, data = dat, disp=cur.par[1],
              d = cur.par[2],a=NULL,b = cur.par[(1:q)+2],xbeta=xbeta,xreg=xreg,
              fitted_value =  cur.lam[-(1:max.q)] ,pearson=pearson,aic = -2*lik.vec + 2*length(init), bic = -2*lik.vec + log(n)*length(init),
              model=model,sp_par = c_sp, hessian=tmp$hessian, link=link))
}



NB_u = function(dat,ingarch_ml){ #length(dat1)=n-max.b, length(lam)=n-max.b
  
  max.ab = max(ingarch_ml$model[["past_obs"]])
  p = length(ingarch_ml$model[["past_mean"]])
  q = length(ingarch_ml$model[["past_obs"]])
  dat1 = dat#[-(1:max.ab)]
  lam1 = ingarch_ml$fitted_value#[-(1:max.ab)]
  
  #P_t (y_t)
  Py_t = apply(cbind(dat1,lam1),1,function(x){ 
    exp( pnbinom(q=x[1] , mu=x[2], size = ingarch_ml$estimates[1],log.p =T)) })
  Py_past = Py_t
  Py_past[dat1==0] = 0
  Py_past[dat1>0] = apply(cbind(dat1[dat1>0],lam1[dat1>0]),1,function(x){
    exp(pnbinom(q=x[1]-1 , mu=x[2], size = ingarch_ml$estimates[1],log.p = T))  } )
  
  return(list(Py_t=Py_t,Py_past=Py_past))
}

pit_u = function(u,pit_ingarch){  #pit_ingarch = list(Py_t,Py_past)
  Py_t = pit_ingarch[["Py_t"]]
  Py_past = pit_ingarch[["Py_past"]]
  F_t = rep(0,length(Py_t))
  F_t[Py_past>=u] = 0
  F_t[which(Py_t<=u)] = 1
  F_t[Py_t>u & Py_past<u] = ( (u-Py_past)/(Py_t-Py_past) )[Py_t>u & Py_past<u]
  return(mean(F_t))
}
pit_mar = Vectorize(pit_u ,vectorize.args = "u")






#########################  Poisson function#########################
#########################  Poisson function#########################
#########################  Poisson function#########################
#########################  Poisson function#########################


Poi_lik = function(dat,model,theta,p = 0,q,xreg, c_sp,link){ 
  if( is.null(xreg) ){ xbeta = NULL }else{xbeta = theta[-(1:(1+p+q))]}
  if( is.null(model[["past_mean"]]) ){ a = NULL }else{a = theta[(1:p)+1+q] } 
  
  if(link=="sp"){lam = compute_lam_sp(dat=dat,model=model, d = theta[1], b = theta[(1:q)+1], a = a, xbeta=xbeta, xreg=xreg , c_sp = c_sp)
  }else{
    lam = exp(compute_nu(dat=dat,model=model, d = theta[1], b = theta[(1:q)+1], a = a, xbeta=xbeta, xreg=xreg))
  }
  result = dpois(x=dat, lambda = lam,log=T)
  return(-sum(result[-(1:q)]) ) #n_size - q
}


Poi_ml = function(dat,init=NULL,model = list(past_mean=1,past_obs=1),xreg=NULL, link, c_sp=1,op_method="BFGS"){
  n = length(dat)
  p = length(model[["past_mean"]])
  q = length(model[["past_obs"]])
  max.q = max(model[["past_obs"]])
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = init[-(1:(1+p+q))]}
  
  
  if(is.null(init)){
    init = tscount::tsglm(dat,model=model,xreg=xreg, link="log", distr="nbinom")
    init = c(init$coef)
  }
  
  tmp = optim(par = init, fn = Poi_lik, 
              dat = dat, xreg=xreg, c_sp = c_sp,
              model = model,p = p, q = max.q ,method=op_method,link=link)
  cur.par = tmp$par
  #print(cur.par)
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = cur.par[-(1:(1+p+q))]}
  if(link=="sp"){
    cur.lam = compute_lam_sp(dat=dat,model=model, d = cur.par[1], b = cur.par[(1:q)+1], a = cur.par[(1:p)+1+q],xbeta=xbeta, xreg=xreg, c_sp = c_sp ) }
  else{
    cur.lam = exp(compute_nu(dat=dat,model=model, d = cur.par[1], b = cur.par[(1:q)+1], a = cur.par[(1:p)+1+q],xbeta=xbeta, xreg=xreg) )
  }
  
  lik.vec = -tmp$value
  pearson = ( dat - cur.lam )/sqrt(cur.lam)
  pearson = pearson[-(1:max.q)]
  
  return(list(estimates = cur.par,log.lik= lik.vec, data = dat,
              d = cur.par[1],b = cur.par[(1:q)+1],a = cur.par[(1:p)+1+q],xbeta=xbeta,xreg=xreg,
              fitted_value =  cur.lam[-(1:max.q)] ,pearson=pearson,aic = -2*lik.vec + 2*length(init), bic = -2*lik.vec + log(n)*length(init),
              model=model,sp_par = c_sp, link=link))
}



Poi_ml_ARCH = function(dat,init=NULL,model = list(past_mean=NULL,past_obs=1),xreg=NULL, link, c_sp=1,op_method="BFGS"){
  n = length(dat)
  p = length(model[["past_mean"]])
  q = length(model[["past_obs"]])
  max.q = max(model[["past_obs"]])
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = init[-(1:(1+p+q))]}
  
  
  if(is.null(init)){
    init = tscount::tsglm(dat,model=model,xreg=xreg, link="log", distr="nbinom")
    init = c(init$coef)
  }
  
  tmp = optim(par = init, fn = Poi_lik, 
              dat = dat, xreg=xreg, c_sp = c_sp,link=link,
              model = model,p = p, q = max.q ,method=op_method)
  cur.par = tmp$par
  #print(cur.par)
  if( is.null(xreg) ){xbeta = NULL}else{xbeta = cur.par[-(1:(1+p+q))]}
  if(link=="sp"){
    cur.lam = compute_lam_sp(dat=dat,model=model, d = cur.par[1], b = cur.par[(1:q)+1], xbeta=xbeta, xreg=xreg, c_sp = c_sp ) }
  else{
    cur.lam = exp(compute_nu(dat=dat,model=model, d = cur.par[1], b = cur.par[(1:q)+1], xbeta=xbeta, xreg=xreg) )
  }
  
  lik.vec = -tmp$value
  pearson = ( dat - cur.lam )/sqrt(cur.lam)
  pearson = pearson[-(1:max.q)]
  
  return(list(estimates = cur.par,log.lik= lik.vec, data = dat,
              d = cur.par[1],a=NULL,b = cur.par[(1:q)+1], xbeta=xbeta, xreg=xreg,
              fitted_value =  cur.lam[-(1:max.q)] ,pearson=pearson,aic = -2*lik.vec + 2*length(init), bic = -2*lik.vec + log(n)*length(init),
              model=model,sp_par = c_sp, link=link))
}




Poi_u = function(dat,ingarch_ml){ #length(dat1)=n-max.b, length(lam)=n-max.b
  
  max.ab = max(ingarch_ml$model[["past_obs"]])
  p = length(ingarch_ml$model[["past_mean"]])
  q = length(ingarch_ml$model[["past_obs"]])
  dat1 = dat#[-(1:max.ab)]
  lam1 = ingarch_ml$fitted_value#[-(1:max.ab)]
  
  #P_t (y_t)
  Py_t = apply(cbind(dat1,lam1),1,function(x){ 
    exp( ppois(q=x[1] , lambda =x[2], log.p =T)) })
  Py_past = Py_t
  Py_past[dat1==0] = 0
  Py_past[dat1>0] = apply(cbind(dat1[dat1>0],lam1[dat1>0]),1,function(x){
    exp(ppois(q=x[1]-1 , lambda=x[2], log.p = T))  } )
  return(list(Py_t=Py_t,Py_past=Py_past))
}








###Parametric bootstrap

NB_ml_pb = function(ingarch, pb_size=100, op_method="BFGS"){  ###Must have any covariate.
  if(is.null(ingarch$xreg)){stop("This function can be used only in the presence of any covariate.")}
  p = length(ingarch$model[["past_mean"]]) #if null, length=0
  q = length(ingarch$model[["past_obs"]])
  n_par = p+q + 2 + dim(ingarch$xreg)[2] 
  
  max.q = max( ingarch$model[["past_obs"]] )
  store.est = matrix(0,ncol=n_par,nrow=pb_size)
  #store.mspe = 0
  
  pb=1
  while(pb<=pb_size){ 
    
    if(ingarch$link=="sp"){
    tmp.start = link_sp(ingarch$d + ingarch$xreg[1:max.q,,drop=F]%*%ingarch$xbeta,c_sp = ingarch$sp_par) #dat[1:max.q]
    sim.dat = gen_dat_sp(length(ingarch$data),model = ingarch$model,disp = ingarch$disp, d=ingarch$d,a = ingarch$a, b = ingarch$b,
                         xbeta = ingarch$xbeta, xreg = ingarch$xreg, starting_lam = tmp.start )
    }
    if(ingarch$link=="log"){
    tmp.start = ingarch$d + ingarch$xreg[1:max.q,,drop=F]%*%ingarch$xbeta  
    sim.dat = gen_dat_log(length(ingarch$data),model = ingarch$model,disp = ingarch$disp, d=ingarch$d,a = ingarch$a, b = ingarch$b,
                            xbeta = ingarch$xbeta, xreg = ingarch$xreg, starting_nu = tmp.start ) #dat[1:max.q]
    }
    
    init_val = ingarch$estimates
    tryCatch(
      {
        if( is.null(ingarch$model[["past_mean"]]) ){
          tmp.em.sp = NB_ml_ARCH(sim.dat, init = init_val, model = ingarch$model, link=ingarch$link ,
                                    xreg = ingarch$xreg, c_sp = ingarch$sp_par, op_method=op_method)
        }else{
          tmp.em.sp = NB_ml(sim.dat, init = init_val, model = ingarch$model, link=ingarch$link ,
                               xreg = ingarch$xreg, c_sp = ingarch$sp_par, op_method=op_method)  
        }
        
        store.est[pb,] = tmp.em.sp$estimates
        #store.mspe[pb] = mean(tmp.em.sp$pearson^2)
        if(pb%%10==0)cat("pb=",pb, round(as.vector(store.est[pb,]),3),"\n" )
        pb=pb+1
      },
      error=function(e){
        message('An Error Occurred')
        print(e)
      }
    )
    
  }
  #return(list(store.est,store.mspe))
  return(store.est)
}




ingarch.pred = function(ingarch, link, xreg = NULL){
  dat_n = length(ingarch$data)
  p = length(ingarch$a)
  q = length(ingarch$b)
  
  if(link == "sp"){
    return(link_sp(
      ingarch$d + 
        sum(ingarch$b*ingarch$data[dat_n:(dat_n-q+1)]) + 
        sum(ingarch$a*ingarch$fitted_value[c(dat_n:(dat_n-p+1))-q ]) + 
        sum(ingarch$xbeta*xreg)))  }
  if(link == "log"){
    return(
      exp(
        ingarch$d + 
          sum(ingarch$b*log(ingarch$data[dat_n:(dat_n-q+1)]+1)) + 
          sum(ingarch$a*log(ingarch$fitted_value[c(dat_n:(dat_n-p+1))-q ])) + 
          sum(ingarch$xbeta*xreg))  
    )}
}




