
#intensity matrix
Qint <- function(x,start_value1){
  
  par_ori <- start_value1[-c(1,2,11,12,21,22,31,32)]
  
  q12=exp(par_ori[1]+par_ori[2]*x)
  q13=exp(par_ori[3]+par_ori[4]*x)
  q14=exp(par_ori[5]+par_ori[6]*x)
  
  q21=exp(par_ori[7]+par_ori[8]*x)
  q23=exp(par_ori[9]+par_ori[10]*x)
  q24=exp(par_ori[11]+par_ori[12]*x)
  
  q31=exp(par_ori[13]+par_ori[14]*x)
  q32=exp(par_ori[15]+par_ori[16]*x)
  q34=exp(par_ori[17]+par_ori[18]*x)
  
  q41=exp(par_ori[19]+par_ori[20]*x)
  q42=exp(par_ori[21]+par_ori[22]*x)
  q43=exp(par_ori[23]+par_ori[24]*x)
  
  
  
  Qinten_temp=matrix(c(0,q12,q13,q14,
                       q21,0,q23,q24,
                       q31,q32,0,q34,
                       q41,q42,q43,0),4,4,byrow = T)
  
  diag(Qinten_temp)=-rowSums(Qinten_temp)
  
  return(Qinten_temp)
}

pinten <- function(x,start_value1){
  
  par_ori2 <- start_value1[c(1,2,11,12,21,22,31,32)]
  
  l11=exp(par_ori2[1]+par_ori2[2]*x)
  l22=exp(par_ori2[3]+par_ori2[4]*x)
  l33=exp(par_ori2[5]+par_ori2[6]*x)
  l44=exp(par_ori2[7]+par_ori2[8]*x)
  
  return(c(l11,l22,l33,l44))
  
}

get_single <- function(x,yy){
  
  genderr=sample(c(0,1),1,prob = c(0.5,0.5))
  
  testexp=Qint(genderr,yy)
  lambda_p=pinten(genderr,yy)
  
  jump_p=testexp/abs(diag(testexp))
  diag(jump_p)=0
  
  timeline=time1=0
  stateline=1
  indicator=0
  state_temp=1
  jj=1
  
  while (jj<=4) {
    
    time_temp <- rexp(1,rate=abs(diag(testexp))[state_temp])
    
    k_time=rpois(1,(lambda_p[state_temp])*time_temp)
    p_times=sort(runif(k_time,time1,time_temp+time1))
    
    timeline=c(timeline,p_times)
    stateline=c(stateline,rep(state_temp,k_time))
    indicator=c(indicator,rep(0,k_time))
    
    time1=time_temp+time1
    state_temp=sample(c(1,2,3,4),1,prob = jump_p[state_temp,])
    
    timeline=c(timeline,time1)
    stateline=c(stateline,state_temp)
    indicator=c(indicator,1)
    
    jj=jj+1
  }
  
  idx=rep(x,length(timeline))
  
  gender=rep(genderr,length(idx))
  
  single_df=as.data.frame(cbind(idx,timeline,stateline,indicator,gender,location=seq(1,length(stateline)),need=0))
  
  loc=NULL
  for (iter in 1:(length(indicator)-2)) {
    if (single_df$indicator[iter]==1 & single_df$indicator[iter+1]==1 & single_df$indicator[iter+2]==0){
      loc=c(loc,(iter+1))
    }
  }
  
  single_df$need[loc]=1
  
  single_df=single_df[single_df$need!=1,-c(6,7)]
  
  return(single_df)
}

impute_f1 <- function(dt,par){
  
  q11=exp(par[1]+par[2]*dt$Gender)
  q12=exp(par[3]+par[4]*dt$Gender)
  q13=exp(par[5]+par[6]*dt$Gender)
  q14=exp(par[7]+par[8]*dt$Gender)
  
  q1=q11+q12+q13+q14
  
  q21=exp(par[9]+par[10]*dt$Gender)
  q22=exp(par[11]+par[12]*dt$Gender)
  q23=exp(par[13]+par[14]*dt$Gender)
  q24=exp(par[15]+par[16]*dt$Gender)
  
  q2=q21+q22+q23+q24
  
  q31=exp(par[17]+par[18]*dt$Gender)
  q32=exp(par[19]+par[20]*dt$Gender)
  q33=exp(par[21]+par[22]*dt$Gender)
  q34=exp(par[23]+par[24]*dt$Gender)
  
  q3=q31+q32+q33+q34
  
  q41=exp(par[25]+par[26]*dt$Gender)
  q42=exp(par[27]+par[28]*dt$Gender)
  q43=exp(par[29]+par[30]*dt$Gender)
  q44=exp(par[31]+par[32]*dt$Gender)
  
  
  q4=q41+q42+q43+q44
  
  #qt=cbind(rep(q1,length(dt$from)),rep(q2,length(dt$from)),rep(q3,length(dt$from)),rep(q4,length(dt$from)))
  qt=cbind(q1,q2,q3,q4)
  
  #hot one encoding
  Atran=rowSums(qt*as.matrix( one_hot(as.data.table(data.frame(idd=1:(length(dt$from)+3),from=as.factor(c(dt$from,c(2,3,4))))))[1:length(dt$from),-1]))
  
  Btran=rowSums(qt* as.matrix( one_hot(as.data.table(data.frame(idd=1:(length(dt$to)+1),from=as.factor(c(dt$to,1)))))[1:length(dt$from),-1]))
  
  dt$newtime=(1/(Atran-Btran))+((dt$time)*exp(-Atran*(dt$time)))/(exp(-Atran*(dt$time))-exp(-Btran*(dt$time)))
  
  return(dt)
}

getinten <- function(x){
  qtt=x[1:4]
  return(as.numeric(qtt[x[5]]-qtt[x[6]]))
}


impute_f <- function(dt,par){
  
  q11=exp(par[1]+par[2]*dt$Gender)
  q12=exp(par[3]+par[4]*dt$Gender)
  q13=exp(par[5]+par[6]*dt$Gender)
  q14=exp(par[7]+par[8]*dt$Gender)
  
  q1=q11+q12+q13+q14
  
  q21=exp(par[9]+par[10]*dt$Gender)
  q22=exp(par[11]+par[12]*dt$Gender)
  q23=exp(par[13]+par[14]*dt$Gender)
  q24=exp(par[15]+par[16]*dt$Gender)
  
  q2=q21+q22+q23+q24
  
  q31=exp(par[17]+par[18]*dt$Gender)
  q32=exp(par[19]+par[20]*dt$Gender)
  q33=exp(par[21]+par[22]*dt$Gender)
  q34=exp(par[23]+par[24]*dt$Gender)
  
  q3=q31+q32+q33+q34
  
  q41=exp(par[25]+par[26]*dt$Gender)
  q42=exp(par[27]+par[28]*dt$Gender)
  q43=exp(par[29]+par[30]*dt$Gender)
  q44=exp(par[31]+par[32]*dt$Gender)
  
  
  q4=q41+q42+q43+q44
  
  qt=cbind(q1,q2,q3,q4)
  
  qtbind=cbind(qt,dt$from,dt$to)
  
  A_Btran= apply(qtbind, 1, getinten)
  
  dt$newtime=(1/(A_Btran))+((dt$time))/(1-exp(A_Btran*(dt$time)))
  
  return(dt)
}

cre_ind <- function(x){
  
  x$i1=0;x$i2=0;x$i3=0;x$i4=0
  x$i5=0;x$i6=0;x$i7=0;x$i8=0
  x$i9=0;x$i10=0;x$i11=0;x$i12=0
  x$i13=0;x$i14=0;x$i15=0;x$i16=0
  x$ii1=0;x$ii2=0;x$ii3=0;x$ii4=0
  
  x$i1[x$trans==1]=1; x$i2[x$trans==2]=1;x$i3[x$trans==3]=1;x$i4[x$trans==4]=1
  x$i5[x$trans==5]=1; x$i6[x$trans==6]=1;x$i7[x$trans==7]=1;x$i8[x$trans==8]=1
  x$i9[x$trans==9]=1; x$i10[x$trans==10]=1;x$i11[x$trans==11]=1;x$i12[x$trans==12]=1
  x$i13[x$trans==13]=1; x$i14[x$trans==14]=1;x$i15[x$trans==15]=1;x$i16[x$trans==16]=1
  x$ii1[x$trans %in% c(1,2,3,4)]=1; x$ii2[x$trans%in% c(5,6,7,8)]=1;x$ii3[x$trans%in% c(9,10,11,12)]=1;x$ii4[x$trans%in% c(13,14,15,16)]=1
  
  return(x)
  
}


full_log_likeli <- function(par,dt){
  
  q11=exp(par[1]+par[2]*dt$Gender)
  q12=exp(par[3]+par[4]*dt$Gender)
  q13=exp(par[5]+par[6]*dt$Gender)
  q14=exp(par[7]+par[8]*dt$Gender)
  
  q1=q11+q12+q13+q14
  
  q21=exp(par[9]+par[10]*dt$Gender)
  q22=exp(par[11]+par[12]*dt$Gender)
  q23=exp(par[13]+par[14]*dt$Gender)
  q24=exp(par[15]+par[16]*dt$Gender)
  
  q2=q21+q22+q23+q24
  
  q31=exp(par[17]+par[18]*dt$Gender)
  q32=exp(par[19]+par[20]*dt$Gender)
  q33=exp(par[21]+par[22]*dt$Gender)
  q34=exp(par[23]+par[24]*dt$Gender)
  
  q3=q31+q32+q33+q34
  
  q41=exp(par[25]+par[26]*dt$Gender)
  q42=exp(par[27]+par[28]*dt$Gender)
  q43=exp(par[29]+par[30]*dt$Gender)
  q44=exp(par[31]+par[32]*dt$Gender)
  
  q4=q41+q42+q43+q44
  
  fulllog=sum(dt$i1*log(q11)+dt$i2*log(q12)+dt$i3*log(q13)+dt$i4*log(q14)+
                dt$i5*log(q21)+dt$i6*log(q22)+dt$i7*log(q23)+dt$i8*log(q24)+
                dt$i9*log(q31)+dt$i10*log(q32)+dt$i11*log(q33)+dt$i12*log(q34)+
                dt$i13*log(q41)+dt$i14*log(q42)+dt$i15*log(q43)+dt$i16*log(q44)-dt$ii1*q1*dt$time-dt$ii2*q2*dt$time-dt$ii3*q3*dt$time-dt$ii4*q4*dt$time)
  
  return(-fulllog)
  
}

mis_log_likeli <- function(par,dt){
  
  q11=exp(par[1]+par[2]*dt$Gender)
  q12=exp(par[3]+par[4]*dt$Gender)
  q13=exp(par[5]+par[6]*dt$Gender)
  q14=exp(par[7]+par[8]*dt$Gender)
  
  q1=q11+q12+q13+q14
  
  q21=exp(par[9]+par[10]*dt$Gender)
  q22=exp(par[11]+par[12]*dt$Gender)
  q23=exp(par[13]+par[14]*dt$Gender)
  q24=exp(par[15]+par[16]*dt$Gender)
  
  q2=q21+q22+q23+q24
  
  q31=exp(par[17]+par[18]*dt$Gender)
  q32=exp(par[19]+par[20]*dt$Gender)
  q33=exp(par[21]+par[22]*dt$Gender)
  q34=exp(par[23]+par[24]*dt$Gender)
  
  q3=q31+q32+q33+q34
  
  q41=exp(par[25]+par[26]*dt$Gender)
  q42=exp(par[27]+par[28]*dt$Gender)
  q43=exp(par[29]+par[30]*dt$Gender)
  q44=exp(par[31]+par[32]*dt$Gender)
  
  q4=q41+q42+q43+q44
  
  qt=cbind(q1,q2,q3,q4)
  
  qtbind=cbind(qt,dt$from,dt$to)
  
  A_Btran= apply(qtbind, 1, getinten)
  
  dt$newtime=(1/(A_Btran))+((dt$time))/(1-exp(A_Btran*(dt$time)))
  
  mis_log=sum(log(-A_Btran/(exp(-A_Btran*dt$time)-1))-A_Btran*dt$newtime)
  
  return(-mis_log)
}


likelihood_equations1 <- function(parv,trans_n,dt){
  
  dt1=subset(dt,dt$trans == trans_n)
  
  dt2=subset(dt,dt$trans %in% matrix(1:16,4,4,byrow = T)[((trans_n-1)%/%4+1),])
  
  f1=dim(dt1)[1]-sum(exp(parv[1]+parv[2]*dt2$Gender)*dt2$time)
  
  f2=sum(dt1$Gender)-sum(dt2$Gender*dt2$time*exp(parv[1]+parv[2]*dt2$Gender))
  
  c(f1=f1,f2=f2)     
  
}



wide_form <- function(x){
  
  lenx <- dim(x)[1]
  dtx1 <- x[1:(lenx-1),2:3]
  colnames(dtx1) <- c("start","from")
  dtx2 <- x[2:(lenx),c(2,3,5,4)]
  colnames(dtx2) <- c("stop","to","Gender","disease")
  dt_wide=cbind(dtx1,dtx2)
  
  return(dt_wide)
}

long_form1 <- function(x){
  
  lenx <- dim(x)[1]
  dtx1 <- x[1:(lenx-1), c(1,2,3,6)]
  colnames(dtx1) <- c("idx","time","state","Gender")
  dtx2 <- x[lenx, c(1,4,5,6)]
  colnames(dtx2) <-  c("idx","time","state","Gender")
  dt_long=rbind(dtx1,dtx2)
  
  return(dt_long)
  
}

long_form <- function(x){
  
  dtx1 <- x[1, c(11,2,3,6)]
  colnames(dtx1) <- c("idx","time","state","Gender")
  dtx2 <- x[1, c(11,4,5,6)]
  colnames(dtx2) <-  c("idx","time","state","Gender")
  dt_long=rbind(dtx1,dtx2)
  
  return(dt_long)
  
}

find_loc <- function(x){
  
  idxloc <- c(0,x$stateline[2:(dim(x)[1])]- x$stateline[1:(dim(x)[1]-1)])
  x$type=2
  x$type[idxloc != 0]=1
  
  return(x)
}

#### exact model
getmsmes1= function(tt,simdt){
  
  system(paste0("echo '",tt,"'"))
  
  msm1=rep(0,48)
  
  dt_final=simdt[[tt]]
  dt_final=ddply(dt_final,.(idx),find_loc)
  
  four.msm1 <- msm( stateline ~ timeline, subject=idx, 
                    data = dt_final, qmatrix = Q.crude, 
                    covariates = ~ gender,obstype=2)
  
  if (length(sqrt(diag(four.msm1$covmat)))==0) {
    msm1[1:24]=c(four.msm1$estimates)
  } else {
    msm1=c(four.msm1$estimates,sqrt(diag(four.msm1$covmat)))
  }
  
  return(msm1)
}

#### panel model
getmsmes2= function(tt,simdt){
  
  system(paste0("echo '",tt,"'"))
  
  msm2=rep(0,48)
  
  dt_final=simdt[[tt]]
  dt_final=ddply(dt_final,.(idx),find_loc)
  
  four.msm2 <- msm( stateline ~ timeline, subject=idx, data = dt_final, 
                    qmatrix = Q.crude,method="BFGS",
                    covariates = ~ gender,obstype=dt_final$type,
                    control = list(fnscale=50000,reltol = 1e-16))
  
  if (length(sqrt(diag(four.msm2$covmat)))==0) {
    msm2[1:24]=c(four.msm2$estimates)
  } else {
    msm2=c(four.msm2$estimates,sqrt(diag(four.msm2$covmat)))
  }
  
  
  return(msm2)
}

#### joint model

getmsmes3= function(tt,simdt,start_value){
  msm3=rep(0,64)
  system(paste0("echo '",tt,"'"))
  dt_final=simdt[[tt]]
  dtf3.1 <- ddply(dt_final, .(idx),wide_form)
  
  dtf3 <- dtf3.1 
  
  dtf3$time <- dtf3$stop-dtf3$start
  
  dtf3$trans=0
  dtf3$trans[dtf3$from==1 & dtf3$to==1]=1
  dtf3$trans[dtf3$from==1 & dtf3$to==2]=2
  dtf3$trans[dtf3$from==1 & dtf3$to==3]=3
  dtf3$trans[dtf3$from==1 & dtf3$to==4]=4
  
  dtf3$trans[dtf3$from==2 & dtf3$to==1]=5
  dtf3$trans[dtf3$from==2 & dtf3$to==2]=6
  dtf3$trans[dtf3$from==2 & dtf3$to==3]=7
  dtf3$trans[dtf3$from==2 & dtf3$to==4]=8
  
  dtf3$trans[dtf3$from==3 & dtf3$to==1]=9
  dtf3$trans[dtf3$from==3 & dtf3$to==2]=10
  dtf3$trans[dtf3$from==3 & dtf3$to==3]=11
  dtf3$trans[dtf3$from==3 & dtf3$to==4]=12
  
  dtf3$trans[dtf3$from==4 & dtf3$to==1]=13
  dtf3$trans[dtf3$from==4 & dtf3$to==2]=14
  dtf3$trans[dtf3$from==4 & dtf3$to==3]=15
  dtf3$trans[dtf3$from==4 & dtf3$to==4]=16
  
  dtf3$newtime=dtf3$time
  dtf6=dtf3
  
  dtf6.1=subset(dtf6,(dtf6$trans %in%c(1,6,11,16))|dtf6$disease==1)
  
  dtf6.2=subset(dtf6,!((dtf6$trans %in%c(1,6,11,16))|dtf6$disease==1))
  
  
  dtf7=impute_f(dtf6.2,par = start_value)
  
  
  dtf7=rbind(dtf6.1,dtf7)
  dtf7.1=subset(dtf7,dtf7$trans %in% c(1,6,11,16)|dtf7$disease==1)
  dtf7.2=subset(dtf7,!(dtf7$trans %in% c(1,6,11,16)|dtf7$disease==1))
  dtf7.3=dtf7.2
  dtf7.2$stop=dtf7.2$start+dtf7.2$newtime
  dtf7.2$time=dtf7.2$newtime
  dtf7.3$from=dtf7.3$to
  dtf7.3$start=dtf7.3$start+dtf7.3$newtime
  dtf7.3$time=dtf7.3$time-dtf7.3$newtime
  dtf7.3$trans=0
  dtf7.3$trans[dtf7.3$from==1 & dtf7.3$to==1]=1
  dtf7.3$trans[dtf7.3$from==2 & dtf7.3$to==2]=6
  dtf7.3$trans[dtf7.3$from==3 & dtf7.3$to==3]=11
  dtf7.3$trans[dtf7.3$from==4 & dtf7.3$to==4]=16
  dtf8=rbind(dtf7.1,dtf7.2,dtf7.3)
  
  dtf8=cre_ind(dtf8)

  par1=NULL
  for (i in 1:16) {
    #i=1
    parxx=start_value[(2*i-1):(2*i)]
    ss <- multiroot(f=likelihood_equations1,start = parxx,trans_n=i,dt=dtf8 )
    par1 <- c(par1,ss$root)
    #print(i)
  }
  
  par2=par1+0.0001
  
  while (sqrt(sum((par2-par1)^2))>0.0001) {
    
    par2=par1
    dtf7=impute_f(dtf6.2,par = par1)
    dtf7_imp=dtf7
    dtf7=rbind(dtf6.1,dtf7)
    dtf7.1=subset(dtf7,dtf7$trans %in% c(1,6,11,16)|dtf7$disease==1)
    dtf7.2=subset(dtf7,!(dtf7$trans %in% c(1,6,11,16)|dtf7$disease==1))
    dtf7.3=dtf7.2
    dtf7.2$stop=dtf7.2$start+dtf7.2$newtime
    dtf7.2$time=dtf7.2$newtime
    dtf7.3$from=dtf7.3$to
    dtf7.3$start=dtf7.3$start+dtf7.3$newtime
    dtf7.3$time=dtf7.3$time-dtf7.3$newtime
    dtf7.3$trans=0
    dtf7.3$trans[dtf7.3$from==1 & dtf7.3$to==1]=1
    dtf7.3$trans[dtf7.3$from==2 & dtf7.3$to==2]=6
    dtf7.3$trans[dtf7.3$from==3 & dtf7.3$to==3]=11
    dtf7.3$trans[dtf7.3$from==4 & dtf7.3$to==4]=16
    dtf8=rbind(dtf7.1,dtf7.2,dtf7.3)
    
    dtf8=cre_ind(dtf8)
    
    par1=NULL
    for (jj in 1:16) {
      
      parxx=par2[(2*jj-1):(2*jj)]
      ss <- multiroot(f=likelihood_equations1,start = parxx,trans_n=jj,dt=dtf8 )
      par1 <- c(par1,ss$root)
      
    }
    
  }
  
  getinf=hessian(full_log_likeli,par2,dt=dtf8)-hessian(mis_log_likeli,par2,dt=dtf7_imp)
  
  msm3[1:32] =par2
  msm3[33:64]=sqrt(diag(solve(getinf)))
  
  return(msm3)
}
