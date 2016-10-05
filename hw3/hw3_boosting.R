
train = function(X,w,y){
  n = length(y)
  d = dim(X)[2]

  optimal_thetas = c()
  optimal_sums = c()
  optimal_ms = c()


  initial_class = rep(1,n)
  initial_class[1] = -1

  for(j in 1:d){
    sorted_index = order(X[,j])
    wj = w[sorted_index]
    xj = X[,j][sorted_index]
    yj = y[sorted_index]
    
    wjsum = sum(wj)

    theta = xj[1]
    incorrectness = (initial_class!=yj)
    cur_sum = incorrectness %*% wj
    tmp_sum = cur_sum
    min_sum = cur_sum
    m = 1
    
    rep_pos = rep(0,n)
    
    for(i in 1:(n-1)){
    	if(xj[i]==xj[i+1]){
    		rep_pos[i]=1
    	}
    }
    
    for(i in 2:n){
      cur_sum = cur_sum + wj[i]*yj[i]

    	if(rep_pos[i]==1){
    		next
    	}

      if(cur_sum < min_sum){
        theta = xj[i]
        min_sum = cur_sum
      }
    }

    cur_sum = wjsum - tmp_sum
    
    if(cur_sum < min_sum){
    	m = -1
    	theta = xj[1]
    	min_sum = cur_sum
    }

    for(i in 2:n){
      cur_sum = cur_sum - wj[i]*yj[i]

    	if(rep_pos[i]==1){
    		next
    	}

      if(cur_sum < min_sum){
        m = -1
        theta = xj[i]
        min_sum = cur_sum
      }
    }

    optimal_thetas = c(optimal_thetas,c(theta))
    optimal_sums = c(optimal_sums,c(min_sum))
    optimal_ms = c(optimal_ms,c(m))
  }

  optimal_sum = optimal_sums[1]
  optimal_j = 1

  for(j in 1:d){
    if(optimal_sums[j] < optimal_sum){
      optimal_sum = optimal_sums[j]
      optimal_j = j
    }
  }

  return(list(j = optimal_j, theta = optimal_thetas[optimal_j], m = optimal_ms[optimal_j]))
}


classify = function(X,pars){
  j = pars$j
  theta = pars$theta
  m = pars$m

  ret = (X[,j]<=theta)
  ret = -sign(ret-1/2)*m
  return(ret)
}


adaBoost = function(X,y,B){
  n = length(y)
  w = rep(1/n,n)
  alpha = rep(1,B)
  allPars = list()

  for(b in 1:B){
    pars = train(X,w,y)
    allPars[[b]] = pars

    incorrectness = (y != classify(X,pars))
    sum_above = w %*% incorrectness
    e = sum_above/sum(w)

    alpha[b] = log((1-e)/e)
    w = w*exp(incorrectness*alpha[b])
  }
  return(list(allPars = allPars, alpha = alpha))

}


agg_class = function(X,alpha,allPars){

  n = dim(X)[1]
  B = length(alpha)

  ret = rep(0,n)

  for(b in 1:B){
    c = classify(X,allPars[[b]])
    ret = ret + alpha[b]*c
  }

  ret = sign(ret)
  ret = sign(ret + 1/2) # change 0 to 1

  return(ret)

}

# answers for question 3 and 4

X = read.table("uspsdata.txt")
y = read.table("uspscl.txt")[,1]
max_train_error= 0
max_test_error = 0

folds = 5
B = 50
val_size = floor(length(y)/folds)

trainX_list = list()
testX_list = list()
trainy_list = list()
testy_list = list()

train_error_list = list()
test_error_list = list()


for(i in 1:folds){
	start = 1 + (i-1)*val_size
	end = start + val_size - 1
	if(i == folds){
		end = length(y)
	}
	val_index = start:end
	trainX_list[[i]] = X[-val_index,]
	trainy_list[[i]] = y[-val_index]
	testX_list[[i]] = X[val_index,]
	testy_list[[i]] = y[val_index]
}

for(i in 1:folds){
	
	X = trainX_list[[i]]
	y = trainy_list[[i]]
	X_test = testX_list[[i]]
	y_test = testy_list[[i]]
	
	n = length(y)
	w = rep(1/n,n)
	
	result = adaBoost(X,y,B)
	allPars = result$allPars
	alpha = result$alpha
	for(b in 1:B){
		
		sub_alpha = alpha[1:b]
		sub_allPars = allPars[1:b]
		
		train_class = agg_class(X,sub_alpha,sub_allPars)
		test_class = agg_class(X_test,sub_alpha,sub_allPars)
		train_error = sum(train_class!=y)/length(y)
		test_error = sum(test_class!=y_test)/length(y_test)
		
		if(train_error > max_train_error){
			max_train_error = train_error
		}
		
		if(test_error > max_test_error){
			max_test_error = test_error
		}
		
		if(b == 1){
			train_error_list[[i]]=c(0,B)
			train_error_list[[i]][1] = train_error
			test_error_list[[i]]=c(0,B)
			test_error_list[[i]][1] = test_error			
		}else{
			train_error_list[[i]][b] = train_error
			test_error_list[[i]][b] = test_error
		}
		
	}
	
}

for(i in 1:folds){
	if(i==1){
		plot(1:B,train_error_list[[i]],main="training error",type="l",lwd=1,xlab="b",ylab="error",ylim=range(c(0,1.1*max_train_error)),lty=3)
	}else{
		lines(1:B,train_error_list[[i]],main="training error",type="l",lwd=1,xlab="b",ylab="error",lty=3)
	}
}

train_avg = c(0,B)

for(b in 1:B){
	avg = 0
	for(i in 1:folds){
		avg=avg+train_error_list[[i]][b]
	}
	avg=avg/folds
	train_avg[b]=avg
}

lines(1:B,train_avg,main="training error",type="l",lwd=2,xlab="b",ylab="error")



for(i in 1:folds){
	if(i==1){
		plot(1:B,test_error_list[[i]],main="test error",type="l",lwd=1,xlab="b",ylab="error",ylim=range(c(0,1.1*max_test_error)),lty=3)
	}else{
		lines(1:B,test_error_list[[i]],main="test error",type="l",lwd=1,xlab="b",ylab="error",lty=3)
	}
}

test_avg = c(0,B)

for(b in 1:B){
	avg = 0
	for(i in 1:folds){
		avg=avg+test_error_list[[i]][b]
	}
	avg=avg/folds
	test_avg[b]=avg
}

lines(1:B,test_avg,main="test error",type="l",lwd=2,xlab="b",ylab="error")


