
# function to calculate the hard assignments
MultinomialEM = function(H,K,tau){
	
	nrow_H = nrow(H)
	ncol_H = ncol(H)
	
	T_rowindex = sample(1:nrow_H,size=K)
	T = H[T_rowindex,]
	
	A_old = matrix(0,nrow_H,K)
	
	while(TRUE){
		
		C = rep(1/K,K)
		Phi = exp(H %*% t(log(T)))
		aboveA = Phi %*% diag(C)
		belowA = rowSums(aboveA)
		A = aboveA * (rep(1/belowA,K))
		
		C = colSums(A)/nrow_H
		B = t(A) %*% H
		belowB = rowSums(B)
		T = B * (rep(1/belowB,ncol_H))
		
		delta = norm(A-A_old,type="O")
		
		A_old = A
		
		if(delta < tau){
			break;
		}
	}
	
	m = apply(A,1,which.max)
	
	return(m)
}


# the following code will run the algorithm for K = 3,4,5
# and a plot will be generated for each K
H = matrix(readBin("histograms.bin", "double", 640000), 40000, 16)
H = H + 0.01
tau = 0.001

for(K in 3:5){
	m = MultinomialEM(H,K,tau)
	image_m = t(apply(matrix(m,200), 1, rev))
	title = paste("K =",toString(K))
	image(1:200,1:200,image_m,axes=FALSE,col=grey(seq(0,1,length=256)),xlab="",ylab="",main=title)
}









