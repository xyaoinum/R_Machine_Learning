# if you cannot run, please uncomment the next line to install the package
# install.packages('e1071', dependencies=TRUE)

# the file uspsdata.txt and uspscl.txt should be loacted in the present
# working directory
# this script will generate a two plot, one for the linear case
# and another for the radial case, as well as the test error
# and the best parameters for the two cases


library(e1071)

get_error_svm = function(train_x, train_y, test_x, test_y, cost, bandwidth, ifkernel){
	train_n = length(train_y)
	test_n = length(test_y)
	gamma = bandwidth
	if(ifkernel == 1){
		svm_train_result = svm(train_x,train_y,type='C',kernel='radial',cost=cost,gamma=gamma)
		predict_label = predict(svm_train_result, test_x)
		error = sum(test_y != predict_label)/test_n
		return(error)
	}else{
		svm_train_result = svm(train_x,train_y,type='C',kernel='linear',cost=cost)
		predict_label = predict(svm_train_result, test_x)
		error = sum(test_y != predict_label)/test_n
		return(error)
	}

}

get_param_crossval = function(train_x,train_y,cost_params,bandwidth_params,ifkernel,fold_k){
	val_size = floor(length(train_y)/fold_k)
	
	the_error = 1
	the_cost = 0
	the_bandwidth = 0
	
	plot.new
	
	bandwidth_i = 0
	
	for(bandwidth in bandwidth_params){
		bandwidth_i = bandwidth_i+1
		
		cost_error = c(1:length(cost_params))
		cost_i = 0
		
		for(cost in cost_params){
			cost_i = cost_i + 1
			
			for(i in 1:fold_k){
				start = 1
				start = start + (i-1)*val_size
				end = start + val_size - 1
				if(i == fold_k){
					end = length(train_y)
				}
				val_index = start:end
				error = get_error_svm(train_x[-val_index,],train_y[-val_index],train_x[val_index,],train_y[val_index],cost,bandwidth,ifkernel)
				
				cost_error[cost_i] = error
				
				
				if(error < the_error){
					the_error = error
					the_cost = cost
					the_bandwidth = bandwidth
				}
			}
		}
		
		if(bandwidth_i == 1){
			if(ifkernel == 1){
				plot(cost_params,cost_error,type="l",lwd=2,xlab="cost",ylim=range(c(0,1.8)),ylab="error",lty=min(bandwidth_i,6))
			}else{
				plot(cost_params,cost_error,type="l",lwd=2,xlab="cost",ylab="error",lty=min(bandwidth_i,6))
				
			}
		}else{
			lines(cost_params,cost_error,type="l",lwd=2,xlab="cost",ylab="error",lty=min(bandwidth_i,6))
		}
	}
	if(length(bandwidth_params) <= 6 && ifkernel == 1){
		legend(x=0.5,y=1.8,bandwidth_params,lty=c(1:length(bandwidth_params)),title="bandwidth")
	}
	return(list(error=the_error,cost=the_cost,bandwidth=the_bandwidth))
}

x_original = read.table("uspsdata.txt")
y_original = read.table("uspscl.txt")

y_original = y_original[,1]

cost_params = c(0.001,0.01,0.1,0.2,0.5,0.8)
bandwidth_params =c(0)

test_index = sample(1:length(y_original), 1/5*length(y_original), replace=FALSE)

train_x = x_original[-test_index,]
train_y = y_original[-test_index]
test_x = x_original[test_index,]
test_y = y_original[test_index]

no_kernel_params = get_param_crossval(x_original,y_original,cost_params,bandwidth_params,0,5)

bandwidth_params =c(0.0001,0.001,0.01,0.1)
kernel_params = get_param_crossval(x_original,y_original,cost_params,bandwidth_params,1,5)

print("linear: test error, cost")
print(get_error_svm(train_x, train_y, test_x, test_y, no_kernel_params$cost, 0, 0))
print(no_kernel_params$cost)
print("radial: test error, cost, bandwidth:")
print(get_error_svm(train_x, train_y, test_x, test_y, kernel_params$cost, kernel_params$bandwidth, 1))
print(kernel_params$cost)
print(kernel_params$bandwidth)



