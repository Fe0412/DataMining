#problem4
#(a)
train_set = list(hamilton.train, madison.train)
dictionary_train = make.sorted.dictionary.df(train_set)
D1 = nrow(dictionary_train)
mu = c(0.1/D1 ,1/D1, 10/D1, 100/D1, 1000/D1)
correct = matrix(data = NA, nrow = 5, ncol = 5)
false_n = matrix(data = NA, nrow = 5, ncol = 5)
false_p = matrix(data = NA, nrow = 5, ncol = 5)

dtm.h.train = make.document.term.matrix(hamilton.train, dictionary_train)
dtm.m.train = make.document.term.matrix(madison.train, dictionary_train)

for(i in 1:5){
  for(j in 1:5){
    
    test_h_true = vector(mode = "logical", length = 35)
    test_h_true [(7*j- 6):(7*j)] = TRUE
    test_h = hamilton.train[test_h_true]#total 35
    train_h = hamilton.train[!test_h_true]#total 35
    test_m_true = vector(mode = "logical", length = 15)
    test_m_true [(3*j- 2):(3*j)] = TRUE
    test_m = madison.train[test_m_true]#total 15
    train_m = madison.train[!test_m_true]#total 15
    
    log.prior.hamilton = log(35/50)
    log.prior.madison = log(15/50)
    
    logp.h.train = make.log.pvec(dtm.h.train[!test_h_true,], mu[i])
    logp.m.train = make.log.pvec(dtm.m.train[!test_m_true,], mu[i])
    r_h = naive.bayes (logp.h.train, logp.m.train, log.prior.hamilton, log.prior.madison, dtm.h.train[test_h_true,])
    r_m = naive.bayes (logp.h.train, logp.m.train, log.prior.hamilton, log.prior.madison, dtm.m.train[test_m_true,])
    
    true_p = 0
    true_n = 0
    false_p[i,j] = 0#Madison classified as Hamilton divided by the total amount of testing Madison
    false_n[i,j] = 0
    correct[i,j] = 0
    for (m in 1:length(r_h)){
      if(r_h[m] == 1){true_p = true_p + 1}
      else {false_n[i,j] = false_n[i,j] + 1}
    }
    false_n[i,j] = false_n[i,j] / length(r_h)
    for (n in 1:length(r_m)){
      if(r_m[n] == 0){true_n = true_n + 1}
      else {false_p[i,j] = false_p[i,j] + 1}
    }
    false_p[i,j] = false_p[i,j] / length(r_m)
    correct[i,j] = (true_p + true_n)/(length(r_h)+length(r_m))
    
  }
}
matplot(t(correct), type = "l")
matplot(t(false_n), type = "l")
matplot(t(false_p), type = "l")
par(mfrow = c(1,3))
co = rowMeans(correct)
plot(x = mu, y = co, type = "l")
f_n = rowMeans(false_n)
plot(x = mu, y = f_n, type = "l")
f_p = rowMeans(false_p)
plot(x = mu, y = f_p, type = "l")

#(c)
correct = vector()
false_p = vector()
false_n = vector()

for(k in 1:5){
  #k = 2
  logp.hamilton.train = make.log.pvec(dtm.hamilton.train, mu[k])
  logp.hamilton.test = make.log.pvec(dtm.hamilton.test, mu[k])
  logp.madison.train = make.log.pvec(dtm.madison.train, mu[k])
  logp.madison.test = make.log.pvec(dtm.madison.test, mu[k])
  
  result_h = vector()
  result_m = vector()
  
  naive.bayes = function(logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison, dtm.test){
    p_h = vector()
    p_m = vector()
    result= vector()
    for (i in 1:nrow(dtm.test)){
      p_h[i] = log.prior.hamilton + sum( logp.hamilton.train * dtm.test[i,] ) # y = 1
      p_m[i] = log.prior.madison + sum( logp.madison.train * dtm.test[i,] ) # y = 0
      if(p_h[i] > p_m[i]) {result[i] = 1}
      else {result[i] = 0}
    }
    return(result)
  }
  
  result_h = naive.bayes (logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison, dtm.hamilton.test)
  result_m = naive.bayes (logp.hamilton.train, logp.madison.train, log.prior.hamilton, log.prior.madison, dtm.madison.test)
  
  true_p = 0
  true_n = 0
  false_p[k] = 0#Madison classified as Hamilton divided by the total amount of testing Madison
  false_n[k] = 0
  correct[k] = 0
  for (m in 1:length(result_h)){
    if(result_h[m] == 1){true_p = true_p + 1}
    else {false_n[k] = false_n[k] + 1}
  }
  false_n[k] = false_n[k] / length(result_h)
  for (n in 1:length(result_m)){
    if(result_m[n] == 0){true_n = true_n + 1}
    else {false_p[k] = false_p[k] + 1}
  }
  false_p[k] = false_p[k] / length(result_m)
  correct[k] = (true_p + true_n)/(length(result_h)+length(result_m))
}

#(d)
percentage_error_c = abs(co - correct)/correct

