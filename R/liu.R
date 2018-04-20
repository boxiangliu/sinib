library(foreach)
library(data.table)
library(cowplot)
library(sinib)

#-----------#
# Functions #
#-----------#
p_norm_app = function(q,size,prob){
	mu = sum(size*prob)
	sigma = sqrt(sum(size*prob*(1-prob)))
	pnorm(q, mean = mu, sd = sigma)
}

d_norm_app = function(x,size,prob){
	mu = sum(size*prob)
	sigma = sqrt(sum(size*prob*(1-prob)))
	dnorm(x, mean = mu, sd = sigma)
}

#----------#
# Figure 1 #
#----------#
# Comparison of CDF between truth and approximation:
data=foreach(m=c(10,100,1000),.combine='rbind')%do%{
	foreach(n=c(10,100,1000),.combine='rbind')%do%{
		foreach(p=c(0.1, 0.5, 0.9),.combine='rbind')%do%{
			a=pbinom(q=0:(m+n),size=(m+n),prob = p)
			b=psinib(q=0:(m+n),size=c(m,n),prob=c(p,p))
			c=p_norm_app(q=0:(m+n),size=c(m,n),prob = c(p,p))
			data.table(s=seq_along(a),truth=a,saddle=b,norm=c,m=m,n=n,p=p)
		}
	}
}
data[,m:=paste0('m = ',m)]
data[,p:=paste0('p = ',p)]
data = melt(data,measure.vars = c('saddle','norm'))

for (i in c(10,100,1000)){
	p=ggplot(data[n==i],aes(x=truth,y=value,color=p,linetype=variable))+
		geom_line()+
		facet_grid(p~m)+
		theme_bw()+
		scale_color_discrete(name='prob',guide = 'none')+
		xlab('Truth')+
		ylab('Approximation') + 
		scale_linetype_discrete(name = '', breaks = c('saddle','norm'), labels = c('Saddlepoint','Gassian')) + 
		theme(legend.position = 'top')
	save_plot(sprintf('~/Documents/tools/sinib/manuscript/figures/truth_vs_approximation_n%s.pdf',i),p,base_width=6,base_height=6)
}


#----------#
# Figure 2 #
#----------#
data=foreach(m=c(100),.combine='rbind')%do%{
	foreach(n=c(100),.combine='rbind')%do%{
		foreach(p=c(0.1, 0.5, 0.9),.combine='rbind')%do%{
			a=pbinom(q=0:(m+n),size=(m+n),prob = p)
			b=psinib(q=0:(m+n),size=as.integer(c(m,n)),prob=c(p,p))
			c=p_norm_app(q=0:(m+n),size=c(m,n),prob = c(p,p))
			data.table(s=seq_along(a),truth=a,saddle=b,norm=c,m=m,n=n,p=p)
		}
	}
}
data = melt(data,measure.vars = c('saddle','norm'),value.name = 'approx', variable.name = 'Method')
data[,`Relative error` := (truth-approx)/truth]
data[,Error := (truth-approx)]
data[,p:=paste0('p = ',p)]
data = melt(data,measure.vars = c('Relative error','Error'),value.name = 'error', variable.name = 'type')

p2=ggplot(data[Method == 'saddle'],aes(x=s,y=error,color=p))+
	geom_point(alpha=0.5)+theme_bw()+
	facet_wrap(type~p,scales='free')+
	xlab('Quantile')+
	ylab(expression('Truth - Approximation'~~~~~~~~~~~~~~~frac(Truth - Approximation,Truth)))+
	scale_color_discrete(guide='none') + 
	geom_vline(xintercept=200*0.5,color='green',linetype='longdash')+
	geom_vline(xintercept=200*0.1,color='red',linetype='longdash')+
	geom_vline(xintercept=200*0.9,color='blue',linetype='longdash')
	
save_plot('~/Documents/tools/sinib/manuscript/figures/truth_minus_saddlepoint_approximation.pdf',p2,base_width=6,base_height=4.5)


p2=ggplot(data[Method == 'norm'],aes(x=s,y=error,color=p))+
	geom_point(alpha=0.5)+theme_bw()+
	facet_wrap(type~p,scales='free')+
	xlab('Quantile')+
	ylab(expression('Truth - Approximation'~~~~~~~~~~~~~~~frac(Truth - Approximation,Truth)))+
	scale_color_discrete(guide='none') + 
	geom_vline(xintercept=200*0.5,color='green',linetype='longdash')+
	geom_vline(xintercept=200*0.1,color='red',linetype='longdash')+
	geom_vline(xintercept=200*0.9,color='blue',linetype='longdash')

save_plot('~/Documents/tools/sinib/manuscript/figures/truth_minus_gaussian_approximation.pdf',p2,base_width=6,base_height=4.5)

#----------#
# Figure 3 #
#----------#
# Comparison of PDF between truth and approximation:
data=foreach(m=c(10,100,1000),.combine='rbind')%do%{
	foreach(n=c(10,100,1000),.combine='rbind')%do%{
		foreach(p=c(0.1, 0.5, 0.9),.combine='rbind')%do%{
			a=dbinom(x=0:(m+n),size=(m+n),prob = p)
			b=dsinib(x=0:(m+n),size=as.integer(c(m,n)),prob=c(p,p))
			c=d_norm_app(x=0:(m+n),size=c(m,n),prob=c(p,p))
			data.table(s=seq_along(a),truth=a,saddle=b,norm=c,m=m,n=n,p=p)
		}
	}
}

data[,m:=paste0('m = ',m)]
data[,p:=paste0('p = ',p)]
data = melt(data,measure.vars = c('saddle','norm'))

for (i in c(10,100,1000)){
	p3=ggplot(data[n==i],aes(x=truth,y=value,color=p,linetype=variable))+
		geom_line()+
		facet_wrap(m~p,scales='free')+
		theme_bw()+
		scale_color_discrete(guide = 'none')+
		xlab('Truth')+
		ylab('Approximation') + 
		scale_linetype_discrete(name = '', breaks = c('saddle','norm'), labels = c('Saddlepoint','Gassian')) + 
		theme(legend.position = 'top')
	save_plot(sprintf('~/Documents/tools/sinib/manuscript/figures/truth_vs_approximation_pdf_n%s.pdf',i),p3,base_width=6,base_height=6)
}


#----------# 
# Figure 4 #
#----------#
data=foreach(m=c(100),.combine='rbind')%do%{
	foreach(n=c(100),.combine='rbind')%do%{
		foreach(p=c(0.1, 0.5, 0.9),.combine='rbind')%do%{
			a=dbinom(x=0:(m+n),size=(m+n),prob = p)
			b=dsinib(x=0:(m+n),size=as.integer(c(m,n)),prob=c(p,p))
			c=d_norm_app(x=0:(m+n),size=c(m,n),prob = c(p,p))
			data.table(s=seq_along(a),truth=a,saddle=b,norm=c,m=m,n=n,p=p)
		}
	}
}
data = melt(data,measure.vars = c('saddle','norm'),value.name = 'approx', variable.name = 'Method')
data[,`Relative error` := (truth-approx)/truth]
data[,Error := (truth-approx)]
data[,p:=paste0('p = ',p)]
data = melt(data,measure.vars = c('Relative error','Error'),value.name = 'error', variable.name = 'type')

p4=ggplot(data[Method == 'saddle'],aes(x=s,y=error,color=p))+
	geom_point(alpha=0.5)+theme_bw()+
	facet_wrap(type~p,scales='free')+
	xlab('Quantile')+
	ylab(expression('Truth - Approximation'~~~~~~~~~~~~~~~frac(Truth - Approximation,Truth)))+
	scale_color_discrete(guide='none') + 
	geom_vline(xintercept=200*0.5,color='green',linetype='longdash')+
	geom_vline(xintercept=200*0.1,color='red',linetype='longdash')+
	geom_vline(xintercept=200*0.9,color='blue',linetype='longdash')

save_plot('~/Documents/tools/sinib/manuscript/figures/truth_minus_saddlepoint_approximation_pdf.pdf',p4,base_width=6,base_height=4.5)


#-----------------------#
# Healthcare monitoring #
#-----------------------#
library(sinib)
library(foreach)
library(cowplot)
library(data.table)

size=as.integer(c(12, 14, 4, 2, 20, 17, 11, 1, 8, 11))
prob=c(0.074, 0.039, 0.095, 0.039, 0.053, 0.043, 0.067, 0.018, 0.099, 0.045)

# Sinib:
approx=dsinib(0:sum(size),size,prob)
approx=data.frame(s=0:sum(size),pdf=approx,type='saddlepoint')

# Gauss: 
gauss_approx = d_norm_app(0:sum(size),size,prob)
gauss_approx = data.frame(s=0:sum(size),pdf=gauss_approx,type='gauss')

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


data=rbind(data,gauss_approx,approx)
truth=data[type=='1e+08',]

merged=merge(truth[,list(s,pdf)],data,by='s',suffixes=c('_truth','_approx'))
merged=merged[type!='1e+08',]


p=ggplot(merged,aes(pdf_truth,pdf_approx))+
	geom_point()+
	facet_grid(~type)+
	geom_abline(intercept=0,slope=1)+
	theme_bw()+
	xlab('Truth')+
	ylab('Approximation')
save_plot('/Users/boshliu/Documents/tools/sinib/manuscript/figures/health_monitor_truth_vs_approx_pdf.pdf',p,base_height=2,base_width=8)

merged[,Error:=pdf_truth-pdf_approx]
merged[,`Relative Error`:=(pdf_truth-pdf_approx)/pdf_truth]
merged = melt(merged,measure.vars = c('Error','Relative Error'),variable.name = 'error_type',value.name = 'error')

p2=ggplot(merged,aes(s,error))+
	geom_point()+
	facet_grid(error_type~type,scales = 'free_y')+
	theme_bw()+
	xlab('Outcome')+
	ylab('Truth-Approx')+
	xlim(0,20) + 
	ylab(expression(frac(Truth - Approximation,Truth)~~~~~~~'Truth - Approximation'))
save_plot('/Users/boshliu/Documents/tools/sinib/manuscript/figures/health_monitor_truth_minus_approx_pdf.pdf',p2,base_height=4,base_width=8)



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
