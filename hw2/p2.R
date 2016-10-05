# fakedata.R should be loaded first in order for this script to run.
# Due to randomness, in some situation it may take very long
# time to get the result.

#the classify function is for problem 1
classify = function(S,z){
	v = sign(S %*% z)
	v = v + (v==0)
	return(v)
}

#the perceptrain function is for problem 2
perceptrain = function(S,y){
	d = dim(S)[2]
	n = dim(S)[1]
	z=sample(1:10,d)
	z_history = z
	k = 1
	while(1){
		cp = 0
		for(i in 1:n){
			dotp = S[i,] %*% z
			if(sign(dotp) != y[i]){
				cp = cp + abs(dotp)
			}
		}
		
		if(cp == 0){
			break
		}
		
		grad = matrix(0,d,1)
		for(i in 1:n){
			dotp = S[i,] %*% z
			if(sign(dotp) != y[i]){
				grad = grad + sign(dotp)*S[i,]
			}
		}
		z = z - 1/k*grad
		z=z[,1]
		z_history = rbind(z_history,z)
		k = k + 1
	}
	
	return(list(z=z,Z_history=z_history))
}

#the convert2D function targets at the requirement
#for problem 4 (convert to 2D), however, I don't
#really use it for plotting
convert2D = function(S,z,z_history){
	return(list(S=S[,-3],z=z[-3],z_history=z_history[,-3],c=z[3],c_history=z_history[,3]))
}


#the plotperceptron function is a helper function for
#ploting a dataset as well as a hyperplane in 2D
plotperceptron = function(S,y,z){
	n = length(y)
	plot(S[y==1,1],S[y==1,2],col="red",xlim=4*range(S[,1]),ylim=4*range(S[,2]),pch="+",xlab='x1',ylab='x2')
	points(S[y==-1,1],S[y==-1,2],col="blue",pch="-")
	if(is.na(-z[3]/z[2]) || is.na(-z[1]/z[2]) || is.null(-z[3]/z[2]) || is.null(-z[1]/z[2]) || is.infinite(-z[3]/z[2]) || is.infinite(-z[1]/z[2])){
		abline(v=z[1])
	}else{
		abline(a=-z[3]/z[2],b=-z[1]/z[2])
	}
}

#the plot_history function is a helper function for
#ploting a dataset as well as a history of hyperplane in 2D
plot_history = function(S,y,z){
	if(is.null(dim(z))){
		z=t(z)
	}
	
	n = length(y)
	plot(S[y==1,1],S[y==1,2],col="red",xlim=4*range(S[,1]),ylim=4*range(S[,2]),pch="+",xlab='x1',ylab='x2')
	points(S[y==-1,1],S[y==-1,2],col="blue",pch="-")
	
	step = floor(length(z[,1])/3)
	if(step == 0){
		step = 1
	}
	
	i = 0
	while(i <= length(z[,1])){
		i = i+ step
		if(i > length(z[,1])){
			break
		}
		if(is.na(-z[i,3]/z[i,2]) || is.na(-z[i,1]/z[i,2]) || is.null(-z[i,3]/z[i,2]) || is.null(-z[i,1]/z[i,2]) || is.infinite(-z[i,3]/z[i,2]) || is.infinite(-z[i,1]/z[i,2])){
			abline(v=z[i,1])
		}else{
			abline(a=-z[i,3]/z[i,2],b=-z[i,1]/z[i,2])
		}
		if(abs(-z[i,1]/z[i,2])<1){
			text_x = runif(1, 4*range(S[,1]))
			text(x=text_x, y=text_x*(-z[i,1]/z[i,2])-z[i,3]/z[i,2],label=i)
		}else{
			text_x = runif(1, 4*range(S[,1]))/abs(-z[i,1]/z[i,2])^3
			text(x=text_x, y=text_x*(-z[i,1]/z[i,2])-z[i,3]/z[i,2],label=i)
		}
	}
	
	i = length(z[,1])
	if(is.na(-z[i,3]/z[i,2]) || is.na(-z[i,1]/z[i,2]) || is.null(-z[i,3]/z[i,2]) || is.null(-z[i,1]/z[i,2]) || is.infinite(-z[i,3]/z[i,2]) || is.infinite(-z[i,1]/z[i,2])){
		abline(v=z[i,1])
	}else{
		abline(a=-z[i,3]/z[i,2],b=-z[i,1]/z[i,2])
	}

	if(abs(-z[i,1]/z[i,2])<1){
		text_x = runif(1, 4*range(S[,1]))
		text(x=text_x, y=text_x*(-z[i,1]/z[i,2])-z[i,3]/z[i,2],label="final z")
	}else{
		text_x = runif(1, 4*range(S[,1]))/abs(-z[i,1]/z[i,2])^3
		text(x=text_x, y=text_x*(-z[i,1]/z[i,2])-z[i,3]/z[i,2],label="final z")
	}
	
	
}

#code to generate the training history, as well as the test data,
#as well as the error rate

z0 = sample(1:10,3)
n = 100
T1 = fakedata(z0,n)
S_train = T1$S
y_train = T1$y

train = perceptrain(S_train,y_train)
z = train$z
z_history = train$Z_history

T2 = fakedata(z0,n)
S_test = T2$S
y_test = T2$y

y_predict = classify(S_test,z)

test_error = sum(y_predict != y_test)/n

print("test error is:")
print(test_error)

plotperceptron(S_test,y_test,z)
plot_history(S_train,y_train,z_history)


