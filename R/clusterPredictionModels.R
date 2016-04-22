#' Develop prediction model to predict cluster probabilities
#' @description This function creates a binary logistic regression model for each cluster
#' @param covariates  An ffdf containing the covariates and values
#' @param cluster     A dataframe containing row_ids and cluster columns
#' @keywords OHDSI, clustering
#' @details This function trains a predictive model using the input covariates to predict cluster membership
#' @export
#' @return A list containing:
#' \item{predictionModels}{ A named list containing the trained models for each cluster (the names are the clusters predicted by the models)}
#' @examples

developPredictionModels <- function(covariates, cluster){
  #todo


}



applyPredictionModels <- function(covariates, predictionModels){
  #todo

}
