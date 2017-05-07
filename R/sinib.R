q=function(u,p){
	return(p*exp(u)/(1-p+p*exp(u)))
}
K=function(u,n,p){
	return(sum(n*log(1-p+p*exp(u))))
}
Kp=function(u,n,p){
	return(sum(n*q(u,p)))
}
Kpp=function(u,n,p){
	q_=q(u,p)
	return(sum(n*q_*(1-q_)))
}
Kppp=function(u,n,p){
	q_=q(u,p)
	return(sum(n*q_*(1-q_)*(1-2*q_)))
}
Kpppp=function(u,n,p){
	q_=q(u,p)
	return(sum(n*q_*(1-q_)*(1-6*q_*(1-q_))))
}

saddlepoint=function(u,n,p,s){
	return(Kp(u,n,p)-s)
}

psinib=function(q,size,prob,lower.tail=TRUE,log.p=FALSE,method=c('Daniels','Butler')){
	
	method=match.arg(method)
	
	n=size
	p=prob
	s=q
	
	w=function(u,n,p){
		K_=K(u,n,p)
		Kp_=Kp(u,n,p)
		return(sign(u)*sqrt(2*u*Kp_-2*K_))
	}
	u1=function(u,n,p){
		Kpp_=Kpp(u,n,p)
		return((1-exp(-u))*sqrt(Kpp_))
	}
	p3=function(u,n,p){
		w_=w(u,n,p)
		u1_=u1(u,n,p)
		if (u1_==0) {
			Kpp0=sum(n*p*(1-p))
			Kppp0=sum(n*p*(1-p)*(1-2*p))
			return(1/2-(2*pi)^(-1/2)*((1/6)*Kppp0*Kpp0^(-3/2)-(1/2)*Kpp0^(-1/2)))
		} else {
			return(1-pnorm(w_)-dnorm(w_)*(1/w_-1/u1_))
		}
	}
	
	
	u2=function(u,n,p){
		return(u*sqrt(Kpp(u,n,p)))
	}
	k3=function(u,n,p){
		return(Kppp(u,n,p)*Kpp(u,n,p)^(-3/2))
	}
	k4=function(u,n,p){
		return(Kpppp(u,n,p)*Kpp(u,n,p)^(-2))
	}
	p4=function(u,n,p){
		u2_=u2(u,n,p)
		k3_=k3(u,n,p)
		k4_=k4(u,n,p)
		w_=w(u,n,p)
		p3_=p3(u,n,p)
		return(p3_-dnorm(w_)*((1/u2_)*((1/8)*k4_-(5/24)*k3_^2)-1/(u2_^3)-k3_/(2*u2_^2)+1/w_^3))
	}
	calc_p4=function(s){
		if (s == sum(n)){
			p4_=prod(p^n)
		} else {
			u_hat=tryCatch({uniroot(saddlepoint,lower=-100,upper=100,tol = 0.0001,n=n,p=p,s=s)$root},error=function(e){return(NA)})
			if (is.na(u_hat)){
				n_trial=100000
				n_binom=length(p)
				set.seed(42)
				mat=matrix(rbinom(n_trial*n_binom,n,p),nrow=n_binom,ncol=n_trial)
				S=colSums(mat)
				p4_=sum(S>=s)/length(S)
			} else {
				p4_=p4(u_hat,n,p)
			}
		}
		return(p4_)
	}
	
	if (method=='Daniels'){
		psinib_=sapply(s,calc_p4)
		print(psinib_)
	} else {
		dsinib_=dsinib(x=0:sum(size),size,prob)
		psinib_=rep(0,length(dsinib_))
		for (i in 1:length(dsinib_)){
			if (i==1){
				psinib_[i]=dsinib_[i]
			} else {
				psinib_[i]=dsinib_[i]+psinib_[i-1]
			}
		}
	}  
	
	if (lower.tail){
		return(psinib_[s+1])
	} else {
		return(1-psinib_[s+1])
	}
}

dsinib=function(x,size,prob,log=FALSE){
	
	n=size
	p=prob
	s=x
	
	p1=function(u,n,p,s){
		return((2*pi*Kpp(u,n,p))^(-1/2)*exp(K(u,n,p)-u*s))
	}
	p2=function(u,n,p,s){
		return(p1(u,n,p,s)*(1+(1/8)*Kpppp(u,n,p)/Kpp(u,n,p)^2-(5/24)*Kppp(u,n,p)^2/Kpp(u,n,p)^3))
	}
	
	calc_p2=function(s){
		u_hat=tryCatch({uniroot(saddlepoint,lower=-100,upper=100,tol = 0.0001,n=n,p=p,s=s)$root},error=function(e){return(NA)})
		p2_=p2(u_hat,n,p,s)
		return(p2_)
	}
	p2_=sapply(1:(sum(n)-1),calc_p2)
	
	p2b=function(n,p,p2_){
		p0=prod((1-p)^n)
		pmx=prod(p^n)
		p2b_=rep(NA,sum(n)+1)
		p2b_[1]=p0
		p2b_[length(p2b_)]=pmx
		p2b_[2:sum(n)]=(1-p0-pmx)*p2_/sum(p2_)
		return(p2b_)
	}
	p2b_=p2b(n,p,p2_)
	
	return(p2b_[s+1])	
}


size=n
prob=p
rsinib=function(n,size,prob){
	psinib(q = 0,size = size,prob = prob)
}

