#' OpenAIBot: A Chatbot Implementation for OpenAI
#'
#' Extends the ChatBot base class with methods for interacting with the OpenAI API.
#'
#' @param api_key A character string for the OpenAI API key.
#' @param model A character string for the OpenAI model (default: "gpt-4").
#' @return An environment representing the OpenAI chatbot instance.
#' @export
OpenAIBot <- function(api_key, model = "gpt-4") {
  bot <- ChatBot()
  bot$api_key <- api_key
  bot$model <- model
  bot$temperature <- 0.7
  bot$max_tokens <- 150
  
  #' Set Model Parameters
  #'
  #' Adjusts temperature and maximum tokens for the model.
  #'
  #' @param temp A numeric value for temperature (default: 0.7).
  #' @param max_tokens An integer for the maximum number of tokens (default: 150).
  #' @return None. Updates the chatbot's internal parameters.
  bot$set_parameters <- function(temp = 0.7, max_tokens = 150) {
    bot$temperature <- temp
    bot$max_tokens <- max_tokens
  }
  
  #' Get a Response from the OpenAI API
  #'
  #' Sends the conversation history to the OpenAI API and retrieves a response.
  #'
  #' @return A character string representing the assistant's response.
  bot$get_response <- function() {
    url <- "https://api.openai.com/v1/chat/completions"
    req <- httr2::request(url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", bot$api_key),
        `Content-Type` = "application/json"
      ) |>
      httr2::req_body_json(list(
        model = bot$model,
        messages = bot$get_history(),
        temperature = bot$temperature,
        max_tokens = bot$max_tokens
      ))
    
    resp <- req |>
      httr2::req_perform()
    
    if (resp$status_code == 200) {
      content <- resp |>
        httr2::resp_body_json()
      response <- content$choices[[1]]$message$content
      bot$add_message("assistant", response)
      return(response)
    } else {
      stop("Error: ", resp$status_code, "\n", resp |> httr2::resp_body_string())
    }
  }
  
  bot
}
