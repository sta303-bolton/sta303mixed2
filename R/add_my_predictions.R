#' add_my_predictions
#'
#' @param data
#' @param model_1
#' @param model_2
#' @param model_3
#'
#' @return Adds predictions to testing_data set
#' @export
#'
#' @examples
add_my_predictions <- function(data = testing_data, model_1 = model_1, model_2 = model_1, model_3 = model_1){
  testing_data$model_1 = round(predict(model_1, newdata = testing_data, type="response"))
  testing_data$model_2 = round(predict(model_2, newdata = testing_data, type="response", allow.new.levels = TRUE))
  testing_data$model_3 = round(predict(model_3, newdata = testing_data, type="response"))

  assign("testing_data", testing_data, .GlobalEnv)
}
