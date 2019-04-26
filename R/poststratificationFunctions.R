#Poststratification Functions



#' Generate poststratified estimates
#'
#' @param pred A vector of predicted values for each row in PSFrame
#' @param PSFrame A dataframe containing the empirical frequency of each unique demographic group within a subnational unit
#' @return A dataframe containing the unit IDs and poststratified estimates
#' @note PSFrame must have 'unit' variable denoting the unit ID, and 'freq' variable denoting the frequency within each cell
#' @note length(pred) must equal nrow(PSFrame)
poststratify <- function(pred, PSFrame){

  dat <- PSFrame %>%
    mutate(pred = pred) %>%
    group_by(unit) %>%
    summarise(poststratifiedEstimate = weighted.mean(pred, freq))

  return(dat)
}

#' Generate a synthetic poststratification frame
#' @description Returns a synthetic frequency distribution derived from the product of marginal distributions. in two input PSFrames. This can be repeated recursively to include more features.
#' @param PSFrame1 A dataframe containing the empirical frequency for each demographic group within each subnational unit
#' @param PSFrame A dataframe containing the empirical frequency for each demographic group within each subnational unit
#' @return A synthetic poststratification frame computed from the marginal frequencies in PSFrame1 and PSFrame2
getSyntheticPSFrame <- function(PSFrame1, PSFrame2){

  #Get Total Populations of Geographic Units
  pop <- PSFrame1 %>%
    group_by(unit) %>%
    summarise(unitPop = sum(freq))

  #Convert frequencies to probabilities
  PSFrame1 %<>%
    left_join(pop, by = 'unit') %>%
    mutate(prob1 = freq/unitPop) %>%
    select(-freq,-unitPop)

  PSFrame2 %<>%
    left_join(pop, by = 'unit') %>%
    mutate(prob2 = freq/unitPop) %>%
    select(-freq,-unitPop)


  #Merge the two and multiply the marginal probabilities
  PSFrame <- left_join(PSFrame1, PSFrame2, by = 'unit') %>%
    mutate(prob = prob1 * prob2)

  #Multiply by unit-level populations to get synthetic frequencies
  PSFrame %<>% left_join(pop, by = 'unit') %>%
    mutate(freq = prob * unitPop) %>%
    select(-prob1, -prob2, -prob, -unitPop)

  return(PSFrame)

}
