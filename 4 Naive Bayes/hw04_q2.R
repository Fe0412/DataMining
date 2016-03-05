#problem2
h_train_num = nrow(dtm.hamilton.train)
m_train_num = nrow(dtm.madison.train)
train_num = h_train_num + m_train_num
log.prior.hamilton = log(h_train_num/train_num)
log.prior.madison = log(m_train_num/train_num)
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
