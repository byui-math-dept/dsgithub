#' @title Function to create issues
#' @param one_stdent is a tibble with all the information for one person
#' @param body_lines is the MarkDown file with a predefined format
#' @export
dwv_one_student_issues <-  function(one_student, body_lines) {
  dwv_issue_cs(body_lines, dat = one_student)
  dwv_issue_tasks(body_lines, dat = one_student)
  dwv_issue_semester(body_lines, dat = one_student)
}
#' @title Function to create repo and connect them to group plus push issues
#' @param one_stdent is a tibble with all the information for one person
#' @param body_lines is the MarkDown file with a predefined format
#' @param ta_group the name of the TA group for that semester.
#' @param class_group the name of the class group for that semester.
#' @export
dwv_one_student <- function(one_student, class_group, ta_group, body_lines = NULL){
  dwv_check_names(one_student)
  dwv_check_repos(one_student)
  dwv_copy_repo(one_student$name, one_student$description)
  team_ids <- dwv_get_teams(semester_name = class_group, ta_name = ta_group)

  dwv_invite_group(one_student, pull(team_ids,students_team_id))
  dwv_add_student(one_student)
  dwv_add_group(one_student, "pull", pull(team_ids, students_team_id))
  dwv_add_group(one_student, "admin", pull(team_ids, ta_team_id))
  dwv_remove_watching(one_student) # run authenticad as user.
  dwv_team_repos(pull(team_ids, students_team_id))

  if(!is.null(body_lines)) {

    dwv_issue_cs(body_lines, dat = one_student)
    dwv_issue_tasks(body_lines, dat = one_student)
    dwv_issue_semester(body_lines, dat = one_student)

    }

}



