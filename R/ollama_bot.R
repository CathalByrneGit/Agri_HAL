OllamaBot <- function(model = "qwen2.5-coder:1.5b", url = "http://localhost:11434/api/completion") {
  bot <- ChatBot()
  bot$model <- model
  bot$url <- url
  
  bot$get_response <- function() {
    req <- request(bot$url) |>
      req_headers(`Content-Type` = "application/json") |>
      req_body_json(list(
        model = bot$model,
        messages = bot$get_history()
      ))
    
    resp <- req |> req_perform()
    if (resp$status_code == 200) {
      content <- resp |> resp_body_json()
      response <- content$completion
      bot$add_message("assistant", response)
      return(response)
    } else {
      stop("Error: ", resp$status_code, "\n", resp |> resp_body_string())
    }
  }
  
  bot
}
