#problem3
true_positives = 0
true_negatives = 0
false_positives = 0#Madison classified as Hamilton divided by the total amount of testing Madison
false_negatives = 0
for (i in 1:length(result_h)){
  if(result_h[i] == 1){true_positives = true_positives + 1}
  else {false_negatives = false_negatives + 1}
}
for (i in 1:length(result_m)){
  if(result_m[i] == 0){true_negatives = true_negatives + 1}
  else {false_positives = false_positives + 1}
}
correct = (true_positives + true_negatives)/(length(result_h) + length(result_m))
true_positives = true_positives / length(result_h)
false_negatives = false_negatives / length(result_h)
true_negatives = true_negatives / length(result_m)
false_positives = false_positives / length(result_m)
