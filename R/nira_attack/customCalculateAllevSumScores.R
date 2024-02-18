customCalculateAllevSumScores <- function(x){
  
  alleviating_sum_scores <- calculateSumScores(x)
  alleviating_sum_scores_mean <- unlist(map(alleviating_sum_scores, mean))
  alleviating_score_original <- alleviating_sum_scores_mean[1]
  alleviating_score_altered <- alleviating_sum_scores_mean[-1]
  alleviating_impact <- alleviating_score_altered - alleviating_score_original
  alleviating_impact
}
