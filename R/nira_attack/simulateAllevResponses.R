simulateAllevResponses <- function(net_obj){
  
  ### Alleviating ----
  set.seed(1234)
  alleviatting_responses <- 
    simulateResponses(net_obj$results$weiadj, 
                      thresholds = net_obj$results$thresholds, 
                      perturbation_type = "alleviating",
                      amount_of_SDs_perturbation = 1)
  
  alleviatting_responses
  
} 
