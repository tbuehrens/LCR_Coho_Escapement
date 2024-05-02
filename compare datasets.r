dat2022<-readRDS(here::here("final data/dissertation versions/standata_2023-06-26.rds"))
dat2022_2024<-readRDS(here::here("final data/standata_2024-05-02.rds"))
dat2022_2024$catch_a_M_mu<-dat2022_2024$catch_a_M_mu[-13,]
dat2022_2024$catch_a_M_SD<-dat2022_2024$catch_a_M_SD[-13,]
dat2022_2024$crc_years<-1:12
dat2022_2024$n_crc_years<-12
  

compare_elements <- function(item1, item2, elements = NULL) {
  if (is.list(item1) && is.list(item2)) {
    # Compare each element of the lists recursively
    changes <- Map(function(x, y) compare_elements(x, y, elements), item1, item2)
    return(changes)
  } else if (is.vector(item1) && is.vector(item2)) {
    if (is.null(elements)) {
      elements <- names(item1)  # Compare all elements if 'elements' is not specified
    }
    item1_numeric <- as.numeric(item1)
    item2_numeric <- as.numeric(item2)
    changes <- item1_numeric[elements] - item2_numeric[elements]
    return(changes)
  } else if (is.matrix(item1) && is.matrix(item2)) {
    if (is.null(elements)) {
      elements <- colnames(item1)  # Compare all elements if 'elements' is not specified
    }
    item1_numeric <- as.numeric(as.matrix(item1))
    item2_numeric <- as.numeric(as.matrix(item2))
    changes <- item1_numeric[elements] - item2_numeric[elements]
    return(changes)
  } else {
    return(NULL)
  }
}


# Compare lists item by item
changes <- compare_elements(dat2022, dat2022_2024)

# Print the changes
print(changes)
