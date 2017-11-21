library(foreach)
library(data.table)
library(cowplot)
library(sinib)

# Comparison of CDF between truth and approximation:
data=foreach(m=c(10,100,1000),.combine='rbind')%do%{
	foreach(n=c(10,100,1000),.combine='rbind')%do%{
		foreach(p=c(0.1, 0.5, 0.9),.combine='rbind')%do%{
			a=pbinom(q=0:(m+n),size=(m+n),prob = p)
			b=psinib(q=0:(m+n),size=as.integer(c(m,n)),prob=c(p,p))
			data.table(s=seq_along(a),truth=a,approx=b,m=m,n=n,p=p)
		}
	}
}

p=ggplot(data,aes(x=truth,y=approx,color=as.character(p)))+geom_point(alpha=0.5)+facet_grid(m~n)+theme_bw()+scale_color_discrete(name='prob')+xlab('Truth')+ylab('Approximation')

save_plot('~/Documents/tools/sinib/manuscript/figures/truth_vs_approximation.pdf',p,base_width=6,base_height=6)


p2=ggplot(data[m==100&n==100],aes(x=s,y=truth-approx,color=as.character(p)))+geom_point(alpha=0.5)+theme_bw()+scale_color_discrete(name='prob')+xlab('Quantile')+ylab('Truth-Approximation')+geom_vline(xintercept=200*0.5,color='green',linetype='longdash')+geom_vline(xintercept=200*0.1,color='red',linetype='longdash')+geom_vline(xintercept=200*0.9,color='blue',linetype='longdash')

save_plot('~/Documents/tools/sinib/manuscript/figures/truth_minus_approximation.pdf',p2,base_width=6,base_height=4)


# Comparison of PDF between truth and approximation:
data=foreach(m=c(10,100,1000),.combine='rbind')%do%{
	foreach(n=c(10,100,1000),.combine='rbind')%do%{
		foreach(p=c(0.1, 0.5, 0.9),.combine='rbind')%do%{
			a=dbinom(x=0:(m+n),size=(m+n),prob = p)
			b=dsinib(x=0:(m+n),size=as.integer(c(m,n)),prob=c(p,p))
			data.table(s=seq_along(a),truth=a,approx=b,m=m,n=n,p=p)
		}
	}
}

p3=ggplot(data,aes(x=truth,y=approx,color=as.character(p)))+geom_point(alpha=0.5)+facet_grid(m~n)+theme_bw()+scale_color_discrete(name='prob')+xlab('Truth')+ylab('Approximation')

save_plot('~/Documents/tools/sinib/manuscript/figures/truth_vs_approximation_pdf.pdf',p3,base_width=6,base_height=6)


p4=ggplot(data[m==100&n==100],aes(x=s,y=truth-approx,color=as.character(p)))+geom_point(alpha=0.5)+theme_bw()+scale_color_discrete(name='prob')+xlab('Quantile')+ylab('Truth-Approximation')+geom_vline(xintercept=200*0.5,color='green',linetype='longdash')+geom_vline(xintercept=200*0.1,color='red',linetype='longdash')+geom_vline(xintercept=200*0.9,color='blue',linetype='longdash')

save_plot('~/Documents/tools/sinib/manuscript/figures/truth_minus_approximation_pdf.pdf',p4,base_width=6,base_height=4)
