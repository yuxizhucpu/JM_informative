library(parallel)
library(palmerpenguins)
library(rlist)
#detect available cores
mc.cores<-parallel::detectCores()

mc.cores

# Group 1
Group1=c( 0.1,0.5,-5,1,-6,2,-6,1,
          -4,1,0.12,0.1,-5,1,-6,1,
          -6,-1,-5,-1,0.12,1,-5,1,
          -5,1,-5,1,-5,1,0.13,1)

# Group 2
Group2=c( -2,0.5,-5,1,-6,2,-6,1,
          -4,1,0.12,0.1,-5,1,-6,1,
          -6,-1,-5,-1,0.12,1,-5,1,
          -5,1,-5,1,-5,1,0.13,1)

# Group 3
Group3=c( -4,0.5,-5,1,-6,2,-6,1,
          -4,1,0.12,0.1,-5,1,-6,1,
          -6,-1,-5,-1,0.12,1,-5,1,
          -5,1,-5,1,-5,1,0.13,1)

start_value= Group1

Q <- rbind(c(0, 1, 1, 1),
           c(1, 0, 1, 1),
           c(1, 1, 0, 1),
           c(1, 1, 1, 0)) 

Q.crude=matrix(exp(c( 0,-5,-6,-6,
                      -4,0,-5,-6,
                      -6,-5,0,-5,
                      -5,-5,-5,0)),4,4,byrow = 4)

#high
sample_all.1 <- vector(mode='list', length=100)
for (i in 1:100) {
  sample_all.1[[i]]=as.data.frame(ldply(1:50, get_single,yy=Group1))
}

#medium
sample_all1.1 <- vector(mode='list', length=100)
for (i in 1:100) {
  sample_all1.1[[i]]=as.data.frame(ldply(1:50, get_single,yy=Group2))
}

#low
sample_all2.1 <- vector(mode='list', length=100)
for (i in 1:100) {
  sample_all2.1[[i]]=as.data.frame(ldply(1:50, get_single,yy=Group3))
}

msm_model1.1=mclapply(1:100, getmsmes1,simdt=sample_all.1, mc.cores = (mc.cores-2))
res_matrix_msm1_senario1 =list.rbind(msm_model1.1)

msm_model1.2=mclapply(1:100, getmsmes1,simdt=sample_all1.1, mc.cores = (mc.cores-2))
res_matrix_msm1_senario2 =list.rbind(msm_model1.2)

msm_model1.3=mclapply(1:100, getmsmes1,simdt=sample_all2.1, mc.cores = (mc.cores-2))
res_matrix_msm1_senario3 =list.rbind(msm_model1.3)

msm_model2.1=mclapply(1:100, getmsmes2,simdt=sample_all.1, mc.cores = (mc.cores-2))
res_matrix_msm2_senario1 =list.rbind(msm_model2.1)

msm_model2.2=mclapply(1:100, getmsmes2,simdt=sample_all1.1, mc.cores = (mc.cores-2))
res_matrix_msm2_senario2 =list.rbind(msm_model2.2)

msm_model2.3=mclapply(1:100, getmsmes2,simdt=sample_all2.1, mc.cores = (mc.cores-2))
res_matrix_msm2_senario3 =list.rbind(msm_model2.3)

msm_model3.1=mclapply(1:100, getmsmes3,simdt=sample_all.1,start_value=Group1, mc.cores = (mc.cores-2))
res_matrix1 =list.rbind(msm_model3.1)

msm_model3.2==mclapply(1:100, getmsmes3,simdt=sample_all1.1,start_value=Group2, mc.cores = (mc.cores-2))
res_matrix2 =list.rbind(msm_model3.2)

msm_model3.3==mclapply(1:100, getmsmes3,simdt=sample_all2.1,start_value=Group3, mc.cores = (mc.cores-2))
res_matrix3 =list.rbind(msm_model3.3)



####get plot
#joint model
#high frequency
start_value3= c( 0.1,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

truematrix=matrix(rep(start_value3,100),100, 32,byrow = T)
#estimation
res_matrix1_es=colMeans(res_matrix1[,1:32])
#se
res_matrix1_se=colMeans(res_matrix1[,33:64])
#relative bias
res_matrix1_rs=colMeans((res_matrix1[,1:32]-truematrix)/truematrix)
#mse
colMeans((res_matrix1[,1:32]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix1[,1:32]- 1.96*res_matrix1[,33:64]
cp_matrix_upper <- res_matrix1[,1:32]+ 1.96*res_matrix1[,33:64]
res_matrix1_cp=colSums((cp_matrix_lower-truematrix)<=0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)

#medium frequency
start_value3= c( -2,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

truematrix=matrix(rep(start_value3,100),100, 32,byrow = T)
#estimation
res_matrix2_es=colMeans(res_matrix2[,1:32])
#se
res_matrix2_se=colMeans(res_matrix2[,33:64])
#relative bias
res_matrix2_rs=colMeans((res_matrix2[,1:32]-truematrix)/truematrix)
#mse
colMeans((res_matrix2[,1:32]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix2[,1:32]- 1.96*res_matrix2[,33:64]
cp_matrix_upper <- res_matrix2[,1:32]+ 1.96*res_matrix2[,33:64]
res_matrix2_cp=colSums((cp_matrix_lower-truematrix)<0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)

#low frequency
start_value3= c( -4,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

truematrix=matrix(rep(start_value3,100),100, 32,byrow = T)
#estimation
res_matrix3_es=colMeans(res_matrix3[,1:32])
#se
res_matrix3_se=colMeans(res_matrix3[,33:64])
#relative bias
res_matrix3_rs=colMeans((res_matrix3[,1:32]-truematrix)/truematrix)
#mse
colMeans((res_matrix3[,1:32]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix3[,1:32]- 1.96*res_matrix3[,33:64]
cp_matrix_upper <- res_matrix3[,1:32]+ 1.96*res_matrix3[,33:64]
res_matrix3_cp=colSums((cp_matrix_lower-truematrix)<0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)

#msm1
#high frequency
start_value3= c( 0.1,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

start_value_msm=start_value3[c(3,5,7,9,13,15,17,19,23,25,27,29,4,6,8,10,14,16,18,20,24,26,28,30)]
truematrix=matrix(rep(start_value_msm,100),100, 24,byrow = T)

#estimation
res_matrix_msm1_senario1_es=colMeans(res_matrix_msm1_senario1[,1:24])
#se
res_matrix_msm1_senario1_se=colMeans(res_matrix_msm1_senario1[,25:48])
#relative bias
res_matrix_msm1_senario1_rs=colMeans((res_matrix_msm1_senario1[,1:24]-truematrix)/truematrix)
#mse
colMeans((res_matrix_msm1_senario1[,1:24]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix_msm1_senario1[,1:24]- 1.96*res_matrix_msm1_senario1[,25:48]
cp_matrix_upper <- res_matrix_msm1_senario1[,1:24]+ 1.96*res_matrix_msm1_senario1[,25:48]
res_matrix_msm1_senario1_cp=colSums((cp_matrix_lower-truematrix)<0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)

#medium frequency
start_value3= c( -2,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

start_value_msm=start_value3[c(3,5,7,9,13,15,17,19,23,25,27,29,4,6,8,10,14,16,18,20,24,26,28,30)]
truematrix=matrix(rep(start_value_msm,100),100, 24,byrow = T)
#estimation
res_matrix_msm1_senario2_es=colMeans(res_matrix_msm1_senario2[,1:24])
#se
res_matrix_msm1_senario2_se=colMeans(res_matrix_msm1_senario2[,25:48])
#relative bias
res_matrix_msm1_senario2_rs=colMeans((res_matrix_msm1_senario2[,1:24]-truematrix)/truematrix)
#mse
colMeans((res_matrix_msm1_senario2[,1:24]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix_msm1_senario2[,1:24]- 1.96*res_matrix_msm1_senario2[,25:48]
cp_matrix_upper <- res_matrix_msm1_senario2[,1:24]+ 1.96*res_matrix_msm1_senario2[,25:48]
res_matrix_msm1_senario2_cp=colSums((cp_matrix_lower-truematrix)<=0 & (cp_matrix_upper-truematrix) >=0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <=0)

#low frequency
start_value3= c( -4,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

start_value_msm=start_value3[c(3,5,7,9,13,15,17,19,23,25,27,29,4,6,8,10,14,16,18,20,24,26,28,30)]
truematrix=matrix(rep(start_value_msm,100),100, 24,byrow = T)
#estimation
res_matrix_msm1_senario3_es=colMeans(res_matrix_msm1_senario3[,1:24])
#se
res_matrix_msm1_senario3_se=colMeans(res_matrix_msm1_senario3[,25:48])
#relative bias
res_matrix_msm1_senario3_rs=colMeans((res_matrix_msm1_senario3[,1:24]-truematrix)/truematrix)
#mse
colMeans((res_matrix_msm1_senario3[,1:24]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix_msm1_senario3[,1:24]- 1.96*res_matrix_msm1_senario3[,25:48]
cp_matrix_upper <- res_matrix_msm1_senario3[,1:24]+ 1.96*res_matrix_msm1_senario3[,25:48]
res_matrix_msm1_senario3_cp=colSums((cp_matrix_lower-truematrix)<0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)


#msm2
#high frequency
start_value3= c( 0.1,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

start_value_msm=start_value3[c(3,5,7,9,13,15,17,19,23,25,27,29,4,6,8,10,14,16,18,20,24,26,28,30)]
truematrix=matrix(rep(start_value_msm,100),100, 24,byrow = T)

#estimation
res_matrix_msm2_senario1_es=colMeans(res_matrix_msm2_senario1[,1:24])
#se
res_matrix_msm2_senario1_se=colMeans(res_matrix_msm2_senario1[,25:48])
#relative bias
res_matrix_msm2_senario1_rs=colMeans((res_matrix_msm2_senario1[,1:24]-truematrix)/truematrix)
#mse
colMeans((res_matrix_msm2_senario1[,1:24]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix_msm2_senario1[,1:24]- 1.96*res_matrix_msm2_senario1[,25:48]
cp_matrix_upper <- res_matrix_msm2_senario1[,1:24]+ 1.96*res_matrix_msm2_senario1[,25:48]
res_matrix_msm2_senario1_cp=colSums((cp_matrix_lower-truematrix)<=0 & (cp_matrix_upper-truematrix) >=0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <=0)

#medium frequency
start_value3= c( -2,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

start_value_msm=start_value3[c(3,5,7,9,13,15,17,19,23,25,27,29,4,6,8,10,14,16,18,20,24,26,28,30)]
truematrix=matrix(rep(start_value_msm,100),100, 24,byrow = T)
#estimation
res_matrix_msm2_senario2_es=colMeans(res_matrix_msm2_senario2[,1:24])
#se
res_matrix_msm2_senario2_se=colMeans(res_matrix_msm2_senario2[,25:48])
#relative bias
res_matrix_msm2_senario2_rs=colMeans((res_matrix_msm2_senario2[,1:24]-truematrix)/truematrix)
#mse
colMeans((res_matrix_msm2_senario2[,1:24]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix_msm2_senario2[,1:24]- 1.96*res_matrix_msm2_senario2[,25:48]
cp_matrix_upper <- res_matrix_msm2_senario2[,1:24]+ 1.96*res_matrix_msm2_senario2[,25:48]
res_matrix_msm2_senario2_cp=colSums((cp_matrix_lower-truematrix)<0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)

#low frequency

start_value3= c( -4,0.5,-5,1,-6,2,-6,1,
                -4,1,0.12,0.1,-5,1,-6,1,
                -6,-1,-5,-1,0.12,1,-5,1,
                -5,1,-5,1,-5,1,0.13,1)

start_value_msm=start_value3[c(3,5,7,9,13,15,17,19,23,25,27,29,4,6,8,10,14,16,18,20,24,26,28,30)]
truematrix=matrix(rep(start_value_msm,100),100, 24,byrow = T)
#estimation
res_matrix_msm2_senario3_es=colMeans(res_matrix_msm2_senario3[,1:24])
#se
res_matrix_msm2_senario3_se=colMeans(res_matrix_msm2_senario3[,25:48])
#relative bias
res_matrix_msm2_senario3_rs=colMeans((res_matrix_msm2_senario3[,1:24]-truematrix)/truematrix)
#mse
colMeans((res_matrix_msm2_senario3[,1:24]-truematrix)^2)
#cp
cp_matrix_lower <- res_matrix_msm2_senario3[,1:24]- 1.96*res_matrix_msm2_senario3[,25:48]
cp_matrix_upper <- res_matrix_msm2_senario3[,1:24]+ 1.96*res_matrix_msm2_senario3[,25:48]
res_matrix_msm2_senario3_cp=colSums((cp_matrix_lower-truematrix)<0 & (cp_matrix_upper-truematrix) >0 & ((cp_matrix_lower-truematrix) * (cp_matrix_upper-truematrix)) <0)

tran1_res=round(cbind(c(res_matrix3_es[c(3,4,5,6,7,8,1,2)],res_matrix2_es[c(3,4,5,6,7,8,1,2)],res_matrix1_es[c(3,4,5,6,7,8,1,2)]),
                      c(res_matrix_msm1_senario3_es[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario2_es[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario1_es[c(1,13,2,14,3,15)],0,0),
                      c(res_matrix_msm2_senario3_es[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario2_es[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario1_es[c(1,13,2,14,3,15)],0,0),
                      c(res_matrix3_se[c(3,4,5,6,7,8,1,2)],res_matrix2_se[c(3,4,5,6,7,8,1,2)],res_matrix1_se[c(3,4,5,6,7,8,1,2)]),
                      c(res_matrix_msm1_senario3_se[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario2_se[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario1_se[c(1,13,2,14,3,15)],0,0),
                      c(res_matrix_msm2_senario3_se[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario2_se[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario1_se[c(1,13,2,14,3,15)],0,0),
                      c(res_matrix3_rs[c(3,4,5,6,7,8,1,2)],res_matrix2_rs[c(3,4,5,6,7,8,1,2)],res_matrix1_rs[c(3,4,5,6,7,8,1,2)])*100,
                      c(res_matrix_msm1_senario3_rs[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario2_rs[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario1_rs[c(1,13,2,14,3,15)],0,0)*100,
                      c(res_matrix_msm2_senario3_rs[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario2_rs[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario1_rs[c(1,13,2,14,3,15)],0,0)*100,
                      c(res_matrix3_cp[c(3,4,5,6,7,8,1,2)],res_matrix2_cp[c(3,4,5,6,7,8,1,2)],res_matrix1_cp[c(3,4,5,6,7,8,1,2)]),
                      c(res_matrix_msm1_senario3_cp[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario2_cp[c(1,13,2,14,3,15)],0,0,res_matrix_msm1_senario1_cp[c(1,13,2,14,3,15)],0,0),
                      c(res_matrix_msm2_senario3_cp[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario2_cp[c(1,13,2,14,3,15)],0,0,res_matrix_msm2_senario1_cp[c(1,13,2,14,3,15)],0,0)),5)

tran2_res=round(cbind(c(res_matrix3_es[c(9,10,13,14,15,16,11,12)],res_matrix2_es[c(9,10,13,14,15,16,11,12)],res_matrix1_es[c(9,10,13,14,15,16,11,12)]),
                      c(res_matrix_msm1_senario3_es[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario2_es[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario1_es[c(4,16,5,17,6,18)],0,0),
                      c(res_matrix_msm2_senario3_es[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario2_es[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario1_es[c(4,16,5,17,6,18)],0,0),
                      c(res_matrix3_se[c(9,10,13,14,15,16,11,12)],res_matrix2_se[c(9,10,13,14,15,16,11,12)],res_matrix1_se[c(9,10,13,14,15,16,11,12)]),
                      c(res_matrix_msm1_senario3_se[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario2_se[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario1_se[c(4,16,5,17,6,18)],0,0),
                      c(res_matrix_msm2_senario3_se[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario2_se[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario1_se[c(4,16,5,17,6,18)],0,0),
                      c(res_matrix3_rs[c(9,10,13,14,15,16,11,12)],res_matrix2_rs[c(9,10,13,14,15,16,11,12)],res_matrix1_rs[c(9,10,13,14,15,16,11,12)])*100,
                      c(res_matrix_msm1_senario3_rs[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario2_rs[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario1_rs[c(4,16,5,17,6,18)],0,0)*100,
                      c(res_matrix_msm2_senario3_rs[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario2_rs[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario1_rs[c(4,16,5,17,6,18)],0,0)*100,
                      c(res_matrix3_cp[c(9,10,13,14,15,16,11,12)],res_matrix2_cp[c(9,10,13,14,15,16,11,12)],res_matrix1_cp[c(9,10,13,14,15,16,11,12)]),
                      c(res_matrix_msm1_senario3_cp[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario2_cp[c(4,16,5,17,6,18)],0,0,res_matrix_msm1_senario1_cp[c(4,16,5,17,6,18)],0,0),
                      c(res_matrix_msm2_senario3_cp[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario2_cp[c(4,16,5,17,6,18)],0,0,res_matrix_msm2_senario1_cp[c(4,16,5,17,6,18)],0,0)),5)


tran3_res=round(cbind(c(res_matrix3_es[c(17,18,19,20,23,24,21,22)],res_matrix2_es[c(17,18,19,20,23,24,21,22)],res_matrix1_es[c(17,18,19,20,23,24,21,22)]),
                      c(res_matrix_msm1_senario3_es[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario2_es[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario1_es[c(7,19,8,20,9,21)],0,0),
                      c(res_matrix_msm2_senario3_es[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario2_es[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario1_es[c(7,19,8,20,9,21)],0,0),
                      c(res_matrix3_se[c(17,18,19,20,23,24,21,22)],res_matrix2_se[c(17,18,19,20,23,24,21,22)],res_matrix1_se[c(17,18,19,20,23,24,21,22)]),
                      c(res_matrix_msm1_senario3_se[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario2_se[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario1_se[c(7,19,8,20,9,21)],0,0),
                      c(res_matrix_msm2_senario3_se[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario2_se[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario1_se[c(7,19,8,20,9,21)],0,0),
                      c(res_matrix3_rs[c(17,18,19,20,23,24,21,22)],res_matrix2_rs[c(17,18,19,20,23,24,21,22)],res_matrix1_rs[c(17,18,19,20,23,24,21,22)])*100,
                      c(res_matrix_msm1_senario3_rs[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario2_rs[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario1_rs[c(7,19,8,20,9,21)],0,0)*100,
                      c(res_matrix_msm2_senario3_rs[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario2_rs[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario1_rs[c(7,19,8,20,9,21)],0,0)*100,
                      c(res_matrix3_cp[c(17,18,19,20,23,24,21,22)],res_matrix2_cp[c(17,18,19,20,23,24,21,22)],res_matrix1_cp[c(17,18,19,20,23,24,21,22)]),
                      c(res_matrix_msm1_senario3_cp[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario2_cp[c(7,19,8,20,9,21)],0,0,res_matrix_msm1_senario1_cp[c(7,19,8,20,9,21)],0,0),
                      c(res_matrix_msm2_senario3_cp[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario2_cp[c(7,19,8,20,9,21)],0,0,res_matrix_msm2_senario1_cp[c(7,19,8,20,9,21)],0,0)),5)


tran4_res=round(cbind(c(res_matrix3_es[c(25,26,27,28,29,30,31,32)],res_matrix2_es[c(25,26,27,28,29,30,31,32)],res_matrix1_es[c(25,26,27,28,29,30,31,32)]),
                      c(res_matrix_msm1_senario3_es[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario2_es[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario1_es[c(10,22,11,23,12,24)],0,0),
                      c(res_matrix_msm2_senario3_es[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario2_es[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario1_es[c(10,22,11,23,12,24)],0,0),
                      c(res_matrix3_se[c(25,26,27,28,29,30,31,32)],res_matrix2_se[c(25,26,27,28,29,30,31,32)],res_matrix1_se[c(25,26,27,28,29,30,31,32)]),
                      c(res_matrix_msm1_senario3_se[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario2_se[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario1_se[c(10,22,11,23,12,24)],0,0),
                      c(res_matrix_msm2_senario3_se[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario2_se[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario1_se[c(10,22,11,23,12,24)],0,0),
                      c(res_matrix3_rs[c(25,26,27,28,29,30,31,32)],res_matrix2_rs[c(25,26,27,28,29,30,31,32)],res_matrix1_rs[c(25,26,27,28,29,30,31,32)])*100,
                      c(res_matrix_msm1_senario3_rs[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario2_rs[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario1_rs[c(10,22,11,23,12,24)],0,0)*100,
                      c(res_matrix_msm2_senario3_rs[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario2_rs[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario1_rs[c(10,22,11,23,12,24)],0,0)*100,
                      c(res_matrix3_cp[c(25,26,27,28,29,30,31,32)],res_matrix2_cp[c(25,26,27,28,29,30,31,32)],res_matrix1_cp[c(25,26,27,28,29,30,31,32)]),
                      c(res_matrix_msm1_senario3_cp[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario2_cp[c(10,22,11,23,12,24)],0,0,res_matrix_msm1_senario1_cp[c(10,22,11,23,12,24)],0,0),
                      c(res_matrix_msm2_senario3_cp[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario2_cp[c(10,22,11,23,12,24)],0,0,res_matrix_msm2_senario1_cp[c(10,22,11,23,12,24)],0,0)),5)

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2))

boxplot(cbind(res_matrix1[,9],res_matrix2[,9],res_matrix3[,9],
              res_matrix_msm1_senario1[,4],res_matrix_msm1_senario2[,4],res_matrix_msm1_senario3[,4],
              res_matrix_msm2_senario1[,4],res_matrix_msm2_senario2[,4],res_matrix_msm2_senario3[,4]),
        lim = c(0, 35))
abline(h=-4,col="red")
boxplot(cbind(res_matrix1[,10],res_matrix2[,10],res_matrix3[,10],
              res_matrix_msm1_senario1[,16],res_matrix_msm1_senario2[,16],res_matrix_msm1_senario3[,16],
              res_matrix_msm2_senario1[,16],res_matrix_msm2_senario2[,16],res_matrix_msm2_senario3[,16]))
abline(h=1,col="red")

boxplot(cbind(res_matrix1[,13],res_matrix2[,13],res_matrix3[,13],
              res_matrix_msm1_senario1[,5],res_matrix_msm1_senario2[,5],res_matrix_msm1_senario3[,5],
              res_matrix_msm2_senario1[,5],res_matrix_msm2_senario2[,5],res_matrix_msm2_senario3[,5]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,14],res_matrix2[,14],res_matrix3[,14],
              res_matrix_msm1_senario1[,17],res_matrix_msm1_senario2[,17],res_matrix_msm1_senario3[,17],
              res_matrix_msm2_senario1[,17],res_matrix_msm2_senario2[,17],res_matrix_msm2_senario3[,17]))
abline(h=1,col="red")


boxplot(cbind(res_matrix1[,15],res_matrix2[,15],res_matrix3[,15],
              res_matrix_msm1_senario1[,6],res_matrix_msm1_senario2[,6],res_matrix_msm1_senario3[,6],
              res_matrix_msm2_senario1[,6],res_matrix_msm2_senario2[,6],res_matrix_msm2_senario3[,6]))
abline(h=-6,col="red")
boxplot(cbind(res_matrix1[,16],res_matrix2[,16],res_matrix3[,16],
              res_matrix_msm1_senario1[,18],res_matrix_msm1_senario2[,18],res_matrix_msm1_senario3[,18],
              res_matrix_msm2_senario1[,18],res_matrix_msm2_senario2[,18],res_matrix_msm2_senario3[,18]))
abline(h=1,col="red")

par(mfrow = c(1, 1))

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2))

boxplot(cbind(res_matrix1[,3],res_matrix2[,3],res_matrix3[,3],
              res_matrix_msm1_senario1[,1],res_matrix_msm1_senario2[,1],res_matrix_msm1_senario3[,1],
              res_matrix_msm2_senario1[,1],res_matrix_msm2_senario2[,1],res_matrix_msm2_senario3[,1]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,4],res_matrix2[,4],res_matrix3[,4],
              res_matrix_msm1_senario1[,13],res_matrix_msm1_senario2[,13],res_matrix_msm1_senario3[,13],
              res_matrix_msm2_senario1[,13],res_matrix_msm2_senario2[,13],res_matrix_msm2_senario3[,13]))
abline(h=1,col="red")

boxplot(cbind(res_matrix1[,5],res_matrix2[,5],res_matrix3[,5],
              res_matrix_msm1_senario1[,2],res_matrix_msm1_senario2[,2],res_matrix_msm1_senario3[,2],
              res_matrix_msm2_senario1[,2],res_matrix_msm2_senario2[,2],res_matrix_msm2_senario3[,2]))
abline(h=-6,col="red")
boxplot(cbind(res_matrix1[,6],res_matrix2[,6],res_matrix3[,6],
              res_matrix_msm1_senario1[,14],res_matrix_msm1_senario2[,14],res_matrix_msm1_senario3[,14],
              res_matrix_msm2_senario1[,14],res_matrix_msm2_senario2[,14],res_matrix_msm2_senario3[,14]))
abline(h=2,col="red")


boxplot(cbind(res_matrix1[,7],res_matrix2[,7],res_matrix3[,7],
              res_matrix_msm1_senario1[,3],res_matrix_msm1_senario2[,3],res_matrix_msm1_senario3[,3],
              res_matrix_msm2_senario1[,3],res_matrix_msm2_senario2[,3],res_matrix_msm2_senario3[,3]))
abline(h=-6,col="red")
boxplot(cbind(res_matrix1[,8],res_matrix2[,8],res_matrix3[,8],
              res_matrix_msm1_senario1[,15],res_matrix_msm1_senario2[,15],res_matrix_msm1_senario3[,15],
              res_matrix_msm2_senario1[,15],res_matrix_msm2_senario2[,15],res_matrix_msm2_senario3[,15]))
abline(h=1,col="red")

par(mfrow = c(1, 1))

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2))

boxplot(cbind(res_matrix1[,9],res_matrix2[,9],res_matrix3[,9],res_matrix_msm1_senario1[,4],res_matrix_msm1_senario2[,4],res_matrix_msm1_senario3[,4],res_matrix_msm2_senario1_1[,4],res_matrix_msm2_senario2_1[,4],res_matrix_msm2_senario3_1[,4]))
abline(h=-4,col="red")
boxplot(cbind(res_matrix1[,10],res_matrix2[,10],res_matrix3[,10],res_matrix_msm1_senario1[,16],res_matrix_msm1_senario2[,16],res_matrix_msm1_senario3[,16],res_matrix_msm2_senario1_1[,16],res_matrix_msm2_senario2_1[,16],res_matrix_msm2_senario3_1[,16]))
abline(h=1,col="red")

boxplot(cbind(res_matrix1[,13],res_matrix2[,13],res_matrix3[,13],res_matrix_msm1_senario1[,5],res_matrix_msm1_senario2[,5],res_matrix_msm1_senario3[,5],res_matrix_msm2_senario1_1[,5],res_matrix_msm2_senario2_1[,5],res_matrix_msm2_senario3_1[,5]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,14],res_matrix2[,14],res_matrix3[,14],res_matrix_msm1_senario1[,17],res_matrix_msm1_senario2[,17],res_matrix_msm1_senario3[,17],res_matrix_msm2_senario1_1[,17],res_matrix_msm2_senario2_1[,17],res_matrix_msm2_senario3_1[,17]))
abline(h=1,col="red")


boxplot(cbind(res_matrix1[,15],res_matrix2[,15],res_matrix3[,15],res_matrix_msm1_senario1[,6],res_matrix_msm1_senario2[,6],res_matrix_msm1_senario3[,6],res_matrix_msm2_senario1_1[,6],res_matrix_msm2_senario2_1[,6],res_matrix_msm2_senario3_1[,6]))
abline(h=-6,col="red")
boxplot(cbind(res_matrix1[,16],res_matrix2[,16],res_matrix3[,16],res_matrix_msm1_senario1[,18],res_matrix_msm1_senario2[,18],res_matrix_msm1_senario3[,18],res_matrix_msm2_senario1_1[,18],res_matrix_msm2_senario2_1[,18],res_matrix_msm2_senario3_1[,18]))
abline(h=1,col="red")

par(mfrow = c(1, 1))

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2))

boxplot(cbind(res_matrix1[,17],res_matrix2[,17],res_matrix3[,17],res_matrix_msm1_senario1[,7],res_matrix_msm1_senario2[,7],res_matrix_msm1_senario3[,7],res_matrix_msm2_senario1_1[,7],res_matrix_msm2_senario2_1[,7],res_matrix_msm2_senario3_1[,7]),ylim=c(-9,-4))
abline(h=-6,col="red")
boxplot(cbind(res_matrix1[,18],res_matrix2[,18],res_matrix3[,18],res_matrix_msm1_senario1[,19],res_matrix_msm1_senario2[,19],res_matrix_msm1_senario3[,19],res_matrix_msm2_senario1_1[,19],res_matrix_msm2_senario2_1[,19],res_matrix_msm2_senario3_1[,19]),ylim=c(-6,1))
abline(h=-1,col="red")

boxplot(cbind(res_matrix1[,19],res_matrix2[,19],res_matrix3[,19],res_matrix_msm1_senario1[,8],res_matrix_msm1_senario2[,8],res_matrix_msm1_senario3[,8],res_matrix_msm2_senario1_1[,8],res_matrix_msm2_senario2_1[,8],res_matrix_msm2_senario3_1[,8]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,20],res_matrix2[,20],res_matrix3[,20],res_matrix_msm1_senario1[,20],res_matrix_msm1_senario2[,20],res_matrix_msm1_senario3[,20],res_matrix_msm2_senario1_1[,20],res_matrix_msm2_senario2_1[,20],res_matrix_msm2_senario3_1[,20]))
abline(h=-1,col="red")


boxplot(cbind(res_matrix1[,23],res_matrix2[,23],res_matrix3[,23],res_matrix_msm1_senario1[,9],res_matrix_msm1_senario2[,9],res_matrix_msm1_senario3[,9],res_matrix_msm2_senario1_1[,9],res_matrix_msm2_senario2_1[,9],res_matrix_msm2_senario3_1[,9]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,24],res_matrix2[,24],res_matrix3[,24],res_matrix_msm1_senario1[,21],res_matrix_msm1_senario2[,21],res_matrix_msm1_senario3[,21],res_matrix_msm2_senario1_1[,21],res_matrix_msm2_senario2_1[,21],res_matrix_msm2_senario3_1[,21]))
abline(h=1,col="red")

par(mfrow = c(1, 1))

par(mfrow = c(3, 2), mar = c(2, 2, 2, 2))

boxplot(cbind(res_matrix1[,25],res_matrix2[,25],res_matrix3[,25],res_matrix_msm1_senario1[,10],res_matrix_msm1_senario2[,10],res_matrix_msm1_senario3[,10],res_matrix_msm2_senario1_1[,10],res_matrix_msm2_senario2_1[,10],res_matrix_msm2_senario3_1[,10]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,26],res_matrix2[,26],res_matrix3[,26],res_matrix_msm1_senario1[,22],res_matrix_msm1_senario2[,22],res_matrix_msm1_senario3[,22],res_matrix_msm2_senario1_1[,22],res_matrix_msm2_senario2_1[,22],res_matrix_msm2_senario3_1[,22]))
abline(h=1,col="red")

boxplot(cbind(res_matrix1[,27],res_matrix2[,27],res_matrix3[,27],res_matrix_msm1_senario1[,11],res_matrix_msm1_senario2[,11],res_matrix_msm1_senario3[,11],res_matrix_msm2_senario1_1[,11],res_matrix_msm2_senario2_1[,11],res_matrix_msm2_senario3_1[,11]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,28],res_matrix2[,28],res_matrix3[,28],res_matrix_msm1_senario1[,23],res_matrix_msm1_senario2[,23],res_matrix_msm1_senario3[,23],res_matrix_msm2_senario1_1[,23],res_matrix_msm2_senario2_1[,23],res_matrix_msm2_senario3_1[,23]))
abline(h=1,col="red")

boxplot(cbind(res_matrix1[,29],res_matrix2[,29],res_matrix3[,29],res_matrix_msm1_senario1[,12],res_matrix_msm1_senario2[,12],res_matrix_msm1_senario3[,12],res_matrix_msm2_senario1_1[,12],res_matrix_msm2_senario2_1[,12],res_matrix_msm2_senario3_1[,12]))
abline(h=-5,col="red")
boxplot(cbind(res_matrix1[,30],res_matrix2[,30],res_matrix3[,30],res_matrix_msm1_senario1[,24],res_matrix_msm1_senario2[,24],res_matrix_msm1_senario3[,24],res_matrix_msm2_senario1_1[,24],res_matrix_msm2_senario2_1[,24],res_matrix_msm2_senario3_1[,24]))
abline(h=1,col="red")

par(mfrow = c(1, 1))









