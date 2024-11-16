#' ChatBot: A Base Class for Chatbots
#'
#' This base class provides common functionality for managing conversation history,
#' setting system messages, and defining shared behavior for chatbot frameworks like OpenAI or Ollama.
#'
#' @return An environment representing the chatbot instance.
#' @export
ChatBot <- function() {
  bot <- new.env()
  bot$system_message <- "You are a helpful assistant."
  bot$history <- list(list(role = "system", content = bot$system_message))
  
  #' Set the System Message
  #'
  #' Defines the initial context or behavior of the chatbot.
  #' 
  #' @param message A character string describing the system message.
  #' @return None. Updates the chatbot's internal state.
  bot$set_system_message <- function(message) {
    bot$system_message <- message
    bot$history[[1]] <- list(role = "system", content = message)
  }
  
  #' Add a Message to the Conversation History
  #'
  #' Appends a user or assistant message to the conversation history.
  #'
  #' @param role A character string, either "user" or "assistant".
  #' @param content The message content as a character string.
  #' @return None. Updates the chatbot's internal history.
  bot$add_message <- function(role, content) {
    bot$history <- append(bot$history, list(list(role = role, content = content)))
  }
  
  #' Get the Conversation History
  #'
  #' Retrieves the entire conversation history, including system, user, and assistant messages.
  #'
  #' @return A list containing the conversation history.
  bot$get_history <- function() {
    return(bot$history)
  }
  
  bot
}
