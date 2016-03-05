#problem 7
#(b)
#entropy(dictionary)
sum = colSums(dtm.hamilton.train)
row_h_train = nrow(dtm.hamilton.train)
col_h_train = ncol(dtm.hamilton.train)
px_yisone = dtm.hamilton.train
for (i in 1:row_h_train){
  for (j in 1:col_h_train){
    px_yisone[i,j] = dtm.hamilton.train[i,j]/sum[j]
  }
}
row_m_train = nrow(dtm.madison.train)
col_m_train = ncol(dtm.madison.train)
px_yisone_m = dtm.madison.train
for (i in 1:row_m_train){
  for (j in 1:col_m_train){
    px_yisone_m[i,j] = dtm.madison.train[i,j]/sum[j]
  }
}
pyisone_train = row_h_train/(row_h_train + row_m_train)
pyiszore_train = row_m_train/(row_h_train + row_m_train)
n = c(200, 500, 1000, 2500)
I_h = vector()
for (i in 1:row_h_train){
  I_h[i] = px_yisone[i]*pyisone_train*log(px_yisone[i]/dictionary[i])
}
