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
  pretest_ids = PreAssessment.QUALTRICS.Response.IDs.,
  posttest_ids = PostAssessment.QUALTRICS.Response.IDs,
  all_class_ids = All.Classes,
  pp_classes = PingPong.Classes
) %>%
  saveRDS(path(ROOT,"data","generated","students.rds"))

############################### Pre-Test #######################################

read.csv(
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

  