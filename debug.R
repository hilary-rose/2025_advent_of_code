do_homework <- function(homework) {
  
  answers <- c()
  
  for (h in 1:length(operators)) {
    
    operator <- operators[[h]]
    pos_from <- operator_positions[[h]]
    pos_to <- operator_positions[[h+1]]-1
    
    question_col <- tibble(question = str_sub(homework, pos_from, pos_to)) %>%
      filter(!str_detect(question, "\\*|\\+")) %>%
      mutate(question = str_remove(question, "\\s$"))
    n_digits <- max(str_length(question_col$question))
    
    numbers <- c()
    
    for (i in 1:n_digits) {
      
      digits <- question_col %>%
        mutate(number = as.numeric(str_sub(question, i, i))) %>%
        pull(number)
      
      number <- as.numeric(glue::glue_collapse(digits[!is.na(digits)]))
      numbers <- c(numbers, number)  
    }
    
    if (operator == "+") {
      answers <- c(answers, (sum(numbers, na.rm = T)))
    } else if (operator == "*") {
      answers <- c(answers, (prod(numbers, na.rm = T)))
    }
  }
  return(sum(answers))
}