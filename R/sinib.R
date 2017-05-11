source('R/utils.R')
#' @importFrom stats uniroot
#' @importFrom stats dnorm
#' @importFrom stats pnorm
#' @importFrom stats rbinom
#' @importFrom stats runif
NULL


#' Distribution of Sum of Independent Non-Identical Binomial Random Variables
#' 
#' Density, distribution function, quantile function, and random number generation for the sum of independent non-identical binomial random variables 
#' 
#' Suppose S is a random variable formed by summing R independent non-identical random variables \eqn{X_r}{Xr}, \eqn{r = 1,...,R}. \deqn{S = \sum_{r=1}^R X_r}{S = X1+X2+...XR}
#' \code{size} and \code{prob} should both be vectors of length R. The first elements of \code{size} and \code{prob} specifies \eqn{X_1}{X1}, the second elements specifies \eqn{X_2}{X2}, so on and so forth. The probability \eqn{F(S)} is calculated using Daniels' second-order continuity-corrected saddlepoint approximation. The density \eqn{p(S)} is calculated using second-order saddlepoint mass approximation with Butler's normalization. 
#' 
#' @param x,q integer vector of quantiles.
#' @param p numeric vector of probabilities. 
#' @param n numeric scalar to indicate number of observations.
#' @param size integer vector of number of trials (see detail).
#' @param prob numeric vector of success probabilities (see detail).
#' @param lower.tail logical; if TRUE, probabilities are \eqn{P[S<=s]}, otherwise, \eqn{P[S>s]}.
#' @param log,log.p logical; if TRUE, probabilities p are given as log(p).
#' @return qsinib gives the cumulative distribution of sum of independent non-identical random variables. 
#' @source See Eisinga et al (2012) Saddlepoint approximations for the sum of independent non-identically distributed binomial random variables. Available from \url{http://onlinelibrary.wiley.com/doi/10.1111/stan.12002/full}
#' 
#' @examples
#' # Calculating the density and probability:
#' size <- as.integer(c(12, 14, 4, 2, 20, 17, 11, 1, 8, 11))
#' prob <- c(0.074, 0.039, 0.095, 0.039, 0.053, 0.043, 0.067, 0.018, 0.099, 0.045)
#' q <- x <- as.integer(seq(1, 19, 2))
#' dsinib(x, size, prob)
#' psinib(q, size, prob)
#' 
#' # Generating random samples:
#' rsinib(100, size, prob)
#' 
#' # Calculating quantiles:
#' p <- psinib(q, size, prob) 
#' qsinib(p, size, prob)
#' 
#' @export
psinib=function(q,size,prob,lower.tail=TRUE,log.p=FALSE){
	
	n=size
	p=prob
	s=q
	stopifnot(is.integer(n))
	stopifnot(is.integer(s))
	
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
	

	p4_=sapply(s+1,calc_p4)

	if (lower.tail){
		if (log.p){
			return(log(1-p4_))
		} else {
			return(1-p4_)	
		}
	} else {
		if (log.p){
			return(log(p4_))
		} else {
			return(p4_)
		}
			
	}
}

#' @rdname psinib
#' @export
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
	
	if (log){
		return(log(p2b_[s+1]))
	} else {
		return(p2b_[s+1])
	}
		
}

#' @rdname psinib
#' @export
rsinib=function(n,size,prob){
	cdf=psinib(q=0:sum(size),size = size,prob = prob)
	x=runif(n,0,1)
	y=sapply(x,function(x) {min(which(cdf>=x))-1})
	return(y)
}

#' @rdname psinib
#' @export
qsinib=function(p,size,prob){
	cdf=psinib(q=0:sum(size),size = size,prob = prob)
	y=sapply(p,function(x) {min(which(cdf>=x))-1})
	return(y)
}