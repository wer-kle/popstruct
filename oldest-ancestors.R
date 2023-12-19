find_oldest_ancestor <- function(indiv_id, horse_data){
  current_horse <- horse_data[horse_data$Indiv == indiv_id, ]
  
  # If the current_horse is empty or NA, return NA
  if (nrow(current_horse) == 0) {
    return(NA)
  }
  
  # While the current horse has a known parent in the dataset
  while(TRUE) {
    
    sire_exists <- !is.na(current_horse$Sire) && current_horse$Sire %in% horse_data$Indiv
    dam_exists <- !is.na(current_horse$Dam) && current_horse$Dam %in% horse_data$Indiv
    
    sire_born <- ifelse(sire_exists, horse_data$Born[horse_data$Indiv == current_horse$Sire], Inf)
    dam_born <- ifelse(dam_exists, horse_data$Born[horse_data$Indiv == current_horse$Dam], Inf)
    
    # If there's no known parent, break
    if(!sire_exists && !dam_exists) {
      break
    }
    
    # Check explicitly for NA values before comparison
    if(is.na(sire_born) && is.na(dam_born)) {
      break
    } else if (is.na(sire_born) || sire_born > dam_born) {
      current_horse <- horse_data[horse_data$Indiv == current_horse$Dam, ]
    } else {
      current_horse <- horse_data[horse_data$Indiv == current_horse$Sire, ]
    }
  }
  
  return(current_horse)
}

# Identify the oldest ancestors for all horses in your dataset
oldest_ancestors_list <- lapply(horse_data$Indiv, 
                                function(x) find_oldest_ancestor(x, horse_data))

# Convert the list to a data frame for easier viewing
oldest_ancestors_df <- do.call(rbind, oldest_ancestors_list)
