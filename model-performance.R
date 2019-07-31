


library(Rnalytica)
library(Jmisc)

datasets <- c('tj', 'pronto-dashboard')
models <- c('lr', 'rf')

for(i in seq_along(datasets)){
  dataset <- read.csv2(paste0('data/', datasets[i], '.csv'), header = T, sep = ',')
  
  indep <- names(dataset)[-1]
  dep <- indep[length(indep)]
  indep <- indep[-length(indep)]
  
  dataset[, dep] <-
    factor(dataset[, dep], labels = c('clean', 'defect'))
  
  # convert factor to numeric
  dataset[, indep] <- (as.data.frame(do.call(cbind, lapply(dataset[, indep], function(x) {
    if (is.factor(x)) {
      o = as.numeric(levels(x)[x])
    } else {
      o = x
    }
    o
  }))))
  
  as.indep <- AutoSpearman(dataset, indep)
  
  for(j in seq_along(models)){
    
    all.results <-
      fit(
        dataset,
        dep,
        indep,
        classifier = models[j],
        validation = 'boot',
        validation.params = list(boot.n = 10)
      )
    as.results <-
      fit(
        dataset,
        dep,
        as.indep,
        classifier = models[j],
        validation = 'boot',
        validation.params = list(boot.n = 100)
      )
    
    saveRDS(list(all.indep = all.results,
                 as.indep = as.results),
            file = paste0('output/', datasets[i],'-', models[j], '.rds'))
  }
  
}
