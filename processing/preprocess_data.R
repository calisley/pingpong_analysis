library(tidyverse)
library(fs)
library(lubridate)

ROOT <- here::here()

################################################################################
#' Data Preprocessing
#' Cleaning up csvs, retaining necessary columns, converting to RDS
################################################################################

################################## Classes #####################################

## All classes ##

read.csv(
  path(ROOT,"data","raw","airtable","College Study Spring 2024 All Study Classes.csv")
) %>%
  transmute(
    ID = ID,
    name= Name,
    treated = grepl("Treatment", Status),
 
    #pre assessment
    pre_assessment_duedate =as.Date(Assignment.Due.Date, format = "%m/%d/%Y"),
    pre_assessment_location = as.factor(Where.Assigned),
    
    #post assessment
    sent_post_assessment = grepl("Sent", PostAssessment),
    
    #course logistics
    n_students = as.integer(Student.Count),
    randomization=as.factor(Randomization.Result..Text.),
    for_first_years = grepl("First-year", Primary.Class.Year),
    for_sophomore = grepl("Sophomore", Primary.Class.Year),
    for_junior = grepl("Junior", Primary.Class.Year),
    for_senior = grepl("Senior", Primary.Class.Year),
    
    undergraduate_level = for_first_years|for_sophomore|for_junior|for_senior,
    graduate_level = grepl("Graduate students", Primary.Class.Year),
    unspecified_level = !(undergraduate_level|graduate_level),
    
    start_date =as.Date(Start.Date, format = "%B %d, %Y"),
    end_date = as.Date(End.Date, format = "%B %d, %Y")
  ) %>%
  saveRDS(path(ROOT,"data","generated","classes.rds"))

############################### Assistants #####################################

read.csv(
  path(ROOT,"data","raw","airtable","College Study Spring 2024 Assistant Templates.csv")
) %>%
  transmute(
    #metadata
    version = Version,
    prev_version = Previous.Version,
    prompt_template = Assistant.Prompt..Template.,
    model_oai = as.factor(Assistant.Model..OAI.),
    model_azure = as.factor(Assistant.Model..Azure.),
    last_modified = as.Date(strptime(Last.Modified, format = "%m/%d/%Y %I:%M%p")),
    #capabilities 
    active = Status == "Active",
    code_interpreter= Enable.Code.Interpreter=="checked", #zero variance for now?
    file_search= Enable.File.Search=="checked",
    use_latex= Use.LaTeX=="checked",
    vision = Experimental.Vision == "checked",
    temperature = as.numeric(Temperature)
  ) %>%
  saveRDS(path(ROOT,"data","generated","assistants.rds"))

## Classes on PP ##
read.csv(
  path(ROOT,"data","raw","airtable","College Study Spring 2024 Classes on PingPong.csv")
) %>%
  transmute(
    ID = ID,
    name = Class.Name,
    term = as.factor(Class.Term),
    airtable_class_id = Airtable.Class,
    assistant_template = Assistant.Templates
  )%>%
  saveRDS(path(ROOT,"data","generated","classes_on_pp.rds"))

############################### Instructors ####################################

read.csv(
  path(ROOT,"data","raw","airtable","College Study Spring 2024 Instructors.csv")
) %>%
  transmute(
    instructor_id = ID,
    class_list = Classes,
    n_classes = Number.of.Classes,
    pp_classes = PingPong.Classes, 
    
    n_pp_classes = as.integer(Added.PingPong.Classes),
    n_classes=as.integer(Count..Classes.),
    n_eligible_classes = as.integer(Eligible.Classes),
    n_treatment_classes = as.integer(Treatment.Classes),
    
    avg_pre_test_completion = as.numeric(Avg.Pre..)/100,
    avg_pre_test_completion = as.numeric(Avg.Post..)/100,

    
    #AI tool usage, dropdown
    use_gen_ai = grepl("ChatGPT or other generative AI", Tools..AI.),
    use_kahoot = grepl("Kahoot!", Tools..AI.),
    use_grammarly = grepl("Grammarly", Tools..AI.),
    use_pollev = grepl("Poll Everywhere", Tools..AI.),
    use_copilot = grepl("Github Copilot", Tools..AI.),
    use_perusall = grepl("Perusall", Tools..AI.),
    use_turnitin = grepl("Turnitin", Tools..AI.),
    use_zybooks = grepl("ZYBooks", Tools..AI.),
    use_gradescope = grepl("Gradescope", Tools..AI.),
    
    #Discussion tool usage
    use_d_canvas = grepl("Canvas", Tools..Discussion.),
    use_d_blackboard = grepl("Blackboard", Tools..Discussion.),
    use_d_d2l = grepl("D2L", Tools..Discussion.),
    use_d_brightspace = grepl("Brightspace", Tools..Discussion.),
    use_d_zoom = grepl("Zoom", Tools..Discussion.),
    use_d_teams = grepl("Microsoft Teams", Tools..Discussion.),
    use_d_openlms = grepl("OpenLMS", Tools..Discussion.),
    use_d_google_classroom = grepl("Google Classroom", Tools..Discussion.),
    use_d_moodle = grepl("Moodle", Tools..Discussion.),
    use_d_piazza = grepl("Piazza", Tools..Discussion.),
    use_d_slack = grepl("slack", Tools..Discussion., ignore.case = TRUE),
    use_d_github = grepl("GitHub", Tools..Discussion., ignore.case = TRUE),
    
  ) %>%
  saveRDS(path(ROOT,"data","generated","instructor_data.rds"))

############################### Students #######################################

read.csv(
  path(ROOT,"data","raw","airtable","College Study Spring 2024 Students.csv")
) %>% transmute(
  student_id = ID,
  pretest_ids = PreAssessment.QUALTRICS.Response.IDs,
  posttest_ids = PostAssessment.QUALTRICS.Response.IDs,
  all_class_ids = All.Classes,
  pp_classes = PingPong.Classes
) %>%
  saveRDS(path(ROOT,"data","generated","students.rds"))

############################### Pre-Test #######################################

pretest<-read.csv(
  path(ROOT,"data","raw","qualtrics","pre-assessment.csv")
) %>% transmute(
  response_id = ResponseId,
  treated = pp_randomization =="Treatment",
  in_study = in_study == 1,
  
  #answers
  q1 = A1,
  q2 = A2,
  q3 = A3,
  q4 = A4,
  q5 = A5,
  q6 = A6,
  q7 = A7,
  q8 = A8,
  q9 = A9,
  q10 = A10,
  q11 = A11,
  
  #demos
  consent = Consent_Choice == "I agree",
  acad_year = factor(D1,
                        levels = c("1st","2nd","3rd","4th","5th","6th or later")),
  gender_f = D3 == "Female",
  race_w = grepl("White", D4),
  race_b = grepl("Black", D4),
  race_a = grepl("Asian", D4),
  race_n = grepl("American Indian", D4),
  race_p = grepl("Native Hawaiian", D4),
  race_o = grepl("Other", D4),
  
  #what is D5?
  
  ai_use = factor(PP_Tool,
                     levels=c("I have never used a generative AI tool before",
                              "Less than 1 hour", "1-5 hours",
                              "More than 5 hours"),
                     labels=c("Never","Less than 1 hour", " 1-5 hours",
                              "More than 5 hours")),
  name = pp_class_name
) %>%
  saveRDS(path(ROOT,"data","generated","pretest.rds"))
  
############################### Post-Test #######################################

## Instructors

read.csv(
  path(ROOT,"data","raw","qualtrics","instructor-post.csv")
) %>%
  transmute(
    response_id = ResponseId,
    instructor_id = ExternalReference,
    eligib_for_pay = InstructorHasTreatmentClasses == 1,
    treated = InstructorHasTreatmentClasses == 1,
    
    ###post assessment
    #affective questions
    #I had the resources I needed to effectively teach my courses.
    resources_to_effectively_teach = factor(AQ...1_1,
      levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
              "Strongly agree")
    ),
    # I was satisfied with student engagement in my courses.
    satisf_student_engagement = factor(AQ...1_2,
      levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
               "Strongly agree")
    ),
    # I was satisfied with student learning outcomes in my courses
    satisf_student_learning = factor(AQ...1_3,
      levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
               "Strongly agree")
    ), 
    # When students had questions, I felt they had sufficient support to find answers.
    student_questions_support = factor(AQ...1_4,
      levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
               "Strongly agree")
    ),
    #gen ai questions
    #How much did students use generative AI in your courses? (Feel free to select two options)
    student_gen_ai_usage = str_split(GenAI...1., ",\\s*") %>% 
      map(~ str_to_lower(str_trim(.x))),
    
    # Generative AI supports my students’ learning
    gen_ai_supports_students = factor(GenAI...2_1,
                                       levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
                                                "Strongly agree")
    ),
    #Generative AI supports student engagement in my courses.
    gen_ai_supports_engagement = factor(GenAI...2_2,
                                       levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
                                                "Strongly agree")
    ),
    #I would like to see instructor-approved generative AI tools used to support learning in future courses.
    see_more_approved_gen_ai = factor(GenAI...2_3,
                                       levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
                                                "Strongly agree")
    ),
    # I would use PingPong in future courses if it were available.
    future_pp_use = factor(GenAI...2_4,
                                       levels=c("Strongly disagree", "Disagree","Neither agree nor disagree", "Agree",
                                                "Strongly agree")
    ),
    
    #open ended questions
    most_helpful_aspects = PPE...1,
    least_helpful_aspects = PPE...2,
    
    #This field population question didn't seem to work
    low_pre_test_comp_expl = Q55,
    low_post_test_completion_expl = Q54,
    
    
    
  ) %>% 
  slice(-(1:2)) %>%
  saveRDS(path(ROOT,"data","generated","instructor_post.rds"))


############################### Threads ########################################

read.csv(
  path(ROOT,"data","raw","pingpong","PingPong Threads, Deidentified.csv")
) %>% 
  transmute(
    student_id = User.AID,
    class_id = Class.ID,
    assistant_id = Assistant.ID,
    assistant_name = Assistant.Name,
    role = Role,
    thread_id = Thread.ID,
    created_at = ymd_hms(Created.At),
    content = Content
  ) %>%
  saveRDS(path(ROOT,"data","generated","threads.rds"))

############################## Post-Tests ######################################

input_dir <- path(ROOT, "data", "raw", "qualtrics", "post")

# List all posttest files
files <- dir_ls(input_dir, glob = "*.csv")

# Function to process a single file
process_post_file <- function(file) {
  raw <- read.csv(file)
  
  # Extract question texts from row 1
  question_texts <- as.character(raw[1, paste0("A", 1:10)])
  names(question_texts) <- paste0("q", 1:10, "_text")
  
  # Drop metadata rows
  raw <- raw[-c(1, 2), ]
  
  # Proceed only if there's real data
  if (nrow(raw) == 0) return(NULL)
  
  raw %>%
    mutate(across(contains("Click"), as.numeric),
           across(contains("Submit"), as.numeric)) %>%
    transmute(
      response_id = ResponseId,
      airtable_class_id = pp_airtable_class_RID,
      airtable_student_id = X__js_airtable_student_RID,
      pp_class_id = pp_class_id,
      class_name = pp_class_name,
      treated = pp_randomization == "Treatment",
      
      q1 = A1, q2 = A2, q3 = A3, q4 = A4, q5 = A5,
      q6 = A6, q7 = A7, q8 = A8, q9 = A9, q10 = A10,
      
      q1_text = question_texts[["q1_text"]],
      q2_text = question_texts[["q2_text"]],
      q3_text = question_texts[["q3_text"]],
      q4_text = question_texts[["q4_text"]],
      q5_text = question_texts[["q5_text"]],
      q6_text = question_texts[["q6_text"]],
      q7_text = question_texts[["q7_text"]],
      q8_text = question_texts[["q8_text"]],
      q9_text = question_texts[["q9_text"]],
      q10_text = question_texts[["q10_text"]],
      
      enjoy_course = factor(AQ_1, levels = c("Not at all","Slightly","Moderately","Very much","Extremely")),
      learn_amount = factor(AQ_2, levels = c("Nothing","A little","A moderate amount", "A substantial amount", "A lot")),
      sufficient_resources_for_questions = factor(AQ_3, levels = c("Strongly Disagree","Somewhat disagree","Neither agree or disagree", "Somewhat agree", "Strongly agree")),
      how_often_used_gen_ai = factor(AQ_5, levels = c("Never","Once or twice throughout the course","A few times per month","Every week","Daily")),
      see_more_approved_gen_ai = factor(AQ_6, levels = c("Strongly Disagree","Somewhat disagree","Neither agree or disagree", "Somewhat agree", "Strongly agree")),
      pp_satisf = factor(AQ_7, levels = c("Very dissatisfied", "Dissatisfied","Neutral", "Satisfied", "Very Satisfied")),
      most_helpful_text = AQ_8,
      least_helpful_text = AQ_9,
      
      used_gpt = AQ_4_1 == "ChatGPT",
      used_pp = AQ_4_2 == "PingPong",
      used_claude = AQ_4_3 == "Claude",
      used_perplexity = AQ_4_4 == "Perplexity",
      used_gemini = AQ_4_5 == "Gemini",
      used_other = AQ_4_6 == "Other",
      other_gen_ai_tool = AQ_4_7,
      
      q1_first_click = A1T_First.Click,
      q2_first_click = A2T_First.Click,
      q3_first_click = A3T_First.Click,
      q4_first_click = A4T_First.Click,
      q5_first_click = A5T_First.Click,
      q6_first_click = A6T_First.Click,
      q7_first_click = A7T_First.Click,
      q8_first_click = A8T_First.Click,
      q9_first_click = A9T_First.Click,
      q10_first_click = A10T_First.Click,
      
      q1_last_click = A1T_Last.Click,
      q2_last_click = A2T_Last.Click,
      q3_last_click = A3T_Last.Click,
      q4_last_click = A4T_Last.Click,
      q5_last_click = A5T_Last.Click,
      q6_last_click = A6T_Last.Click,
      q7_last_click = A7T_Last.Click,
      q8_last_click = A8T_Last.Click,
      q9_last_click = A9T_Last.Click,
      q10_last_click = A10T_Last.Click,
      
      q1_page_submit = A1T_Page.Submit,
      q2_page_submit = A2T_Page.Submit,
      q3_page_submit = A3T_Page.Submit,
      q4_page_submit = A4T_Page.Submit,
      q5_page_submit = A5T_Page.Submit,
      q6_page_submit = A6T_Page.Submit,
      q7_page_submit = A7T_Page.Submit,
      q8_page_submit = A8T_Page.Submit,
      q9_page_submit = A9T_Page.Submit,
      q10_page_submit = A10T_Page.Submit,
      
      q1_n_clicks = A1T_Click.Count,
      q2_n_clicks = A2T_Click.Count,
      q3_n_clicks = A3T_Click.Count,
      q4_n_clicks = A4T_Click.Count,
      q5_n_clicks = A5T_Click.Count,
      q6_n_clicks = A6T_Click.Count,
      q7_n_clicks = A7T_Click.Count,
      q8_n_clicks = A8T_Click.Count,
      q9_n_clicks = A9T_Click.Count,
      q10_n_clicks = A10T_Click.Count,
      
      q1_time_spent_clicks = A1T_Last.Click - A1T_First.Click,
      q2_time_spent_clicks = A2T_Last.Click - A2T_First.Click,
      q3_time_spent_clicks = A3T_Last.Click - A3T_First.Click,
      q4_time_spent_clicks = A4T_Last.Click - A4T_First.Click,
      q5_time_spent_clicks = A5T_Last.Click - A5T_First.Click,
      q6_time_spent_clicks = A6T_Last.Click - A6T_First.Click,
      q7_time_spent_clicks = A7T_Last.Click - A7T_First.Click,
      q8_time_spent_clicks = A8T_Last.Click - A8T_First.Click,
      q9_time_spent_clicks = A9T_Last.Click - A9T_First.Click,
      q10_time_spent_clicks = A10T_Last.Click - A10T_First.Click,
      
      q1_time_spent_submit = A1T_Page.Submit - A1T_First.Click,
      q2_time_spent_submit = A2T_Page.Submit - A2T_First.Click,
      q3_time_spent_submit = A3T_Page.Submit - A3T_First.Click,
      q4_time_spent_submit = A4T_Page.Submit - A4T_First.Click,
      q5_time_spent_submit = A5T_Page.Submit - A5T_First.Click,
      q6_time_spent_submit = A6T_Page.Submit - A6T_First.Click,
      q7_time_spent_submit = A7T_Page.Submit - A7T_First.Click,
      q8_time_spent_submit = A8T_Page.Submit - A8T_First.Click,
      q9_time_spent_submit = A9T_Page.Submit - A9T_First.Click,
      q10_time_spent_submit = A10T_Page.Submit - A10T_First.Click
    )
}

# Process and bind all results
posttest <- map_dfr(files, process_post_file)

# Save
saveRDS(posttest, path(ROOT, "data", "generated", "posttest.rds"))

################################################################################

  