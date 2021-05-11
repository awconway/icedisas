
# *********************************
#' @title Process data for ISAS analysis
#' @rdname process_isas
#' @param trial_data dataframe of other variables
#' @description Iowa Satisfaction with Anesthesia Scale
#' @export
# *********************************
#' @importFrom dplyr recode select rowwise summarise
process_isas <- function(data){


  tmp <- data %>%
    select(starts_with("isas"), -ends_with("factor"))  %>%
    rowwise() %>%
    mutate(isas_mean = mean(c(isasvomit,
                            isassameanesthetic,
                            isasitch,
                            isasrelaxed,
                            isaspain,
                            isassafe,
                            isastoocoldhot,
                            isassurgerypain,
                            isassatisfiedcare,
                            isasfeltgood,
                            isashurt), na.rm = TRUE)) %>%
                            ungroup()

data$isas_mean <- tmp$isas_mean

return(data)

}