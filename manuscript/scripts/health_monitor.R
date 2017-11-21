library(sinib)
library(foreach)
library(cowplot)
library(data.table)

# Sinib:
size=as.integer(c(12, 14, 4, 2, 20, 17, 11, 1, 8, 11))
prob=c(0.074, 0.039, 0.095, 0.039, 0.053, 0.043, 0.067, 0.018, 0.099, 0.045)
approx=dsinib(0:sum(size),size,prob)
approx=data.frame(s=0:sum(size),pdf=approx,type='saddlepoint')

# Simulation:
data=foreach(n_sim=10^c(3:6,8),.combine='rbind')%do%{
	ptm=proc.time()
	n_binom=length(prob)
	set.seed(42)
	mat=matrix(rbinom(n_sim*n_binom,size,prob),nrow=n_binom,ncol=n_sim)
	
	S=colSums(mat)
	sim=sapply(X = 0:sum(size), FUN = function(x) {sum(S==x)/length(S)})
	
	print(proc.time()-ptm)
	
	data.table(s=0:sum(size),pdf=sim,type=n_sim)
	
}


	

data=rbind(data,approx)
truth=data[type=='1e+08',]

merged=merge(truth[,list(s,pdf)],data,by='s',suffixes=c('_truth','_approx'))
merged=merged[type!='1e+08',]

p=ggplot(merged,aes(pdf_truth,pdf_approx))+geom_point()+facet_grid(~type)+geom_abline(intercept=0,slope=1)+theme_bw()+xlab('Truth')+ylab('Approx')
save_plot('/Users/boshliu/Documents/tools/sinib/manuscript/figures/health_monitor_truth_vs_approx_pdf.pdf',p,base_height=3,base_width=8)

merged[,diff:=pdf_truth-pdf_approx]

p2=ggplot(merged,aes(s,diff))+geom_point()+facet_grid(~type)+theme_bw()+xlab('Quantile')+ylab('Truth-Approx')
save_plot('/Users/boshliu/Documents/tools/sinib/manuscript/figures/health_monitor_truth_minus_approx_pdf.pdf',p2,base_height=3,base_width=8)



# time: 
ptm=proc.time()
n_binom=length(prob)
mat=matrix(rbinom(n_sim*n_binom,size,prob),nrow=n_binom,ncol=n_sim)
S=colSums(mat)
sim=sapply(X = 0:sum(size), FUN = function(x) {sum(S==x)/length(S)})
proc.time()-ptm

ptm=proc.time()
approx=dsinib(0:sum(size),size,prob)
proc.time()-ptm
