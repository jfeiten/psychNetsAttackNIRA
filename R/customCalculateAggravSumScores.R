customCalculateAggravSumScores <- function(x){
  
  aggravating_sum_scores <- calculateSumScores(x)
  aggravating_sum_scores_mean <- unlist(map(aggravating_sum_scores, mean))
  aggravating_score_original <- aggravating_sum_scores_mean[1]
  aggravating_score_altered <- aggravating_sum_scores_mean[-1]
  aggravating_impact <- aggravating_score_altered - aggravating_score_original
  aggravating_impact
  
}