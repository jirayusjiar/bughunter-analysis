

library(Rnalytica)
library(Jmisc)

dataset <- read.csv2('data/tj.csv', header = T, sep =',')

indep <- names(dataset)[-1]
dep <- indep[length(indep)]
indep <- indep[-length(indep)]

dataset[, dep] <- factor(dataset[, dep], labels = c('clean', 'defect'))

results <- 
  fit(dataset, dep, indep, classifier = 'lr', validation = 'boot', validation.params = list(boot.n=100))
