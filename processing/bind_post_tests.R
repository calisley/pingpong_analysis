library(tidyverse)
library(fs)
library(lubridate)

ROOT <- here::here()

input_dir <- path(ROOT, "data", "raw", "exam_questions")

# List all posttest files
files <- dir_ls(input_dir, glob = "*.csv")

# Function to process a single file
process_exam_file <- function(file) {
  raw <- read.csv(file)
  if(!"course_name" %in% names(raw)){
    raw$course_name = "Applied ML"
  }
  if(!"question_id" %in% names(raw)){
    raw$question_id = seq_len(nrow(raw))
  }
  if("question_text" %in% names(raw)){
    raw$question = raw$question_text
  }
  if(!"teacher_difficulty" %in% names(raw)){
    raw$teacher_difficulty = raw$difficulty
  }
  if(!"answer" %in% names(raw)){
    if("attempt_2" %in% names(raw)){
      raw$answer = raw$attempt_2
    }
    if("correct_answer_2" %in% names(raw)){
      raw$answer = raw$correct_answer_2
    }
  }
  if(!"base_id" %in% names(raw)){
    raw$base_id =  str_split(raw$exam_for, "_") %>% map_chr(1)
  }
  
    questions <- raw %>%
    transmute(
      class_id = base_id,
      question_id = as.character(question_id), 
      name = course_name,
      question_text = question,
      answer = answer,
      options = options,
      llm_difficulty_label = as.character(teacher_difficulty)
    )
  
}

# Process and bind all results
posttest <- map_dfr(files, process_exam_file)
write_csv(posttest,path(ROOT, "data", "generated", "post_tests_dataset.csv"))








