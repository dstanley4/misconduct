#' Creates CSAHS Misconduct forms
#' @param usernames Usernames for the students involved.
#' @param assignment Assignment name in Courselink gradebook.
#' @param offense Academic offense. Use short form code(s).
#' @param assignment_grade Assignment grade (unofficial grade).
#' @param assignment_due_date Assignment due date.
#' @param assignment_submit_date Assignment submission date.
#' @param other_assignments Other assignment grades (in Courselink gradebook) to be reported.
#' @param course Course code, name, and section number.
#' @param instructor Instructor's name.
#' @param notes Other notes about evidence from the instructor, long form text.
#' @param interview_date Date of interview with instructor. If not specified, uses today's date.
#' @param interview_date Date of form forwarded to Chair. If not specified, uses today's date.
#' @param chair Name of the Department Chair.
#' @param courselink_filename Name of the .csv grade file exported from Courselink.
#' @export
create_forms <- function(usernames, assignment, assignment_grade, offense,
                         assignment_due_date, assignment_submit_date, other_assignments,
                         course, instructor, notes = "",
                         interview_date = "today", forward_date = "today",
                         chair, courselink_filename) {

  grade_df = readr::read_csv(courselink_filename, show_col_types = FALSE)

  csahs_rmd_template_path <- system.file("rmd",
                                         "csahs_form.Rmd",
                                         package = "misconduct")
  L = length(usernames)

  if (length(notes) < L) {
    notes <- rep(notes, L)
  }

  if (length(assignment_submit_date) < L) {
    assignment_submit_date <- rep(assignment_submit_date, L)
  }


  for (i in 1:L) {
    cur_username <- usernames[i]
    cur_filename <- paste0("misconduct_", cur_username, ".pdf")
    cur_params <- get_params(cur_username = cur_username, usernames = usernames,
                             assignment = assignment, assignment_grade = assignment_grade[i],
                             offense = offense,
                             assignment_due_date = assignment_due_date,
                             assignment_submit_date = assignment_submit_date[i],
                             other_assignments = other_assignments,
                             course = course, instructor = instructor,
                             notes = notes[i], interview_date = interview_date,
                             forward_date = forward_date, chair = chair, grade_df = grade_df)

    rmarkdown::render(csahs_rmd_template_path,
                      rmarkdown::pdf_document(),
                      output_file = cur_filename,
                      output_dir = getwd(),
                      params = cur_params,
                      quiet = TRUE)
  }

}


get_params <- function(cur_username, usernames, assignment, assignment_grade, offense,
                       assignment_due_date, assignment_submit_date, other_assignments,
                       course, instructor, notes, interview_date, forward_date, chair, grade_df) {

  if (interview_date == "today") {
    interview_date =  as.character(lubridate::today("GMT"))
  }

  if (forward_date == "today") {
    forward_date =  as.character(lubridate::today("GMT"))
  }

  other_students_id <- !(usernames == cur_username)
  other_students_user <- usernames[other_students_id]
  other_students = c()

  for (i in 1:length(other_students_user)) {
    other_students[i] <- get_name_from_username(other_students_user[i], grade_df)
  }

  column_info = get_grade_column_information(other_assignments, grade_df)
  grade_df_cur_username <- grade_df[cur_username == grade_df$Username,]
  other_grades_df <- data.frame(grade_item = other_assignments, grade = -9, percent = -9)

  for (i in 1:length(other_assignments)) {
    cur_point = as.numeric(grade_df_cur_username[1, column_info$column_full_names[i]])
    max_point = column_info$column_max_points[i]
    other_grades_df$grade[i] = sprintf("%g/%g", cur_point, max_point)
    other_grades_df$percent[i] = sprintf("%1.0f%%", cur_point/max_point*100)
  }

  other_grades_df <- tibble::as_tibble(other_grades_df)
  names(other_grades_df) <- c("Grade Item", "Grade", "Percent")

  param_list <- list( course_info = course,
                      instructor = instructor,
                      chair = chair,
                      offense = offense,
                      student_name = get_name_from_username(cur_username, grade_df),
                      student_email = paste0(cur_username, "@uoguelph.ca"),
                      student_number = get_number_from_username(cur_username, grade_df),
                      due_date = assignment_due_date,
                      submit_date = assignment_submit_date,
                      instructor_date = interview_date,
                      forward_date = forward_date,
                      prelim_grade = assignment_grade,
                      assignment_weight = get_assignment_weight(assignment, grade_df),
                      assignment_name = assignment,
                      other_students = other_students,
                      notes = notes,
                      other_assignments = other_grades_df)
  return(param_list)
}


get_name_from_username <- function(cur_username, grade_df) {
  user_id <- cur_username == grade_df$Username
  first = grade_df$"First Name"[user_id]
  last = grade_df$"Last Name"[user_id]
  name_out <- paste(first, last)
  return(name_out)
}

get_number_from_username <- function(cur_username, grade_df) {
  user_id <- cur_username == grade_df$Username
  student_number <- grade_df$OrgDefinedId[user_id]
  return(student_number)
}


get_assignment_weight <- function(assignment, grade_df) {
  orig_names <- names(grade_df)
  short_names <- get_column_names_grade_df(grade_df)

  is_column <- assignment == short_names

  cur_assignment_name = orig_names[is_column]
  n_string = nchar(cur_assignment_name)

  start_read <- stringr::str_locate(cur_assignment_name, "CategoryWeight:")[2]+1
  if (is.na(start_read)) {
    # Not part of a category group (easy case)
    start_read <- stringr::str_locate(cur_assignment_name, "Weight:")[2] + 1
    end_read <- stringr::str_locate(cur_assignment_name, ">")[1] - 1
    assignment_weight <- as.numeric(substr(cur_assignment_name, start_read, end_read))
  } else {
    # is part of a category group
    start_read <- stringr::str_locate(cur_assignment_name, "Weight:")[2] + 1
    end_read <- stringr::str_locate(cur_assignment_name, "Category:")[1] - 1
    assignment_weight <- as.numeric(substr(cur_assignment_name, start_read, end_read))

    start_read <- stringr::str_locate(cur_assignment_name, "CategoryWeight:")[2]+1
    end_read <- nchar(cur_assignment_name)-1
    category_weight <- as.numeric(substr(cur_assignment_name, start_read, end_read))

    assignment_weight <- assignment_weight/100 * category_weight
  }

  assignment_weight = sprintf("%1.1f %%", assignment_weight)

  return(assignment_weight)
}


get_grade_column_information <- function(other_grades, grade_df) {
  num_grades <- length(other_grades)
  max_points <- rep(-9, num_grades)

  orig_names <- names(grade_df)
  short_names <- get_column_names_grade_df(grade_df)

  num_names <- length(orig_names)
  m_logical <- matrix(rep(NA,num_grades*num_names), nrow = num_names)

  for (i in 1:num_grades) {
    cur_short_name <- other_grades[i]
    short_name_pos_TF <- cur_short_name == short_names

    # extract max points
    cur_name <- orig_names[short_name_pos_TF]
    start_pos <- stringr::str_locate(cur_name, "MaxPoints:")[2] + 1
    end_pos <- stringr::str_locate(cur_name, "Weight:")[1] - 1
    cur_max_point <- as.numeric(substr(cur_name, start_pos, end_pos))
    max_points[i] <- cur_max_point

    m_logical[,i] <- short_name_pos_TF
  }

  is_required_column <- as.logical(rowSums(m_logical))
  column_full_names <- orig_names[is_required_column]
  column_max_points <- max_points

  output <- list(column_short_names = other_grades,
                 column_full_names = column_full_names,
                 column_max_points = column_max_points)

  return(output)
}


get_column_names_grade_df <- function(grade_df) {
  num_columns <- dim(grade_df)[2]
  columns_long_names <- names(grade_df)
  columns_short_names <- rep("zzz", length(columns_long_names))

  for (i in 1:num_columns) {
    cur_long_name <- columns_long_names[i]
    weight_pos <- stringr::str_locate(cur_long_name, "Weighted Grade")
    if (is.na(weight_pos[1])) {
      short_name = cur_long_name
    } else {
      short_name = substr(cur_long_name, 1, weight_pos[1]-2)
    }
    columns_short_names[i] <- short_name
  }

  return(columns_short_names)
}
