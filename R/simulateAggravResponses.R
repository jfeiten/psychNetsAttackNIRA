simulateAggravResponses <- function(net_obj){
  
  set.seed(1234)
  aggravating_responses <- 
    simulateResponses(net_obj$results$weiadj, 
                      thresholds = net_obj$results$thresholds, 
                      perturbation_type = "aggravating",
                      amount_of_SDs_perturbation = 1)
  
  aggravating_responses
  
}
