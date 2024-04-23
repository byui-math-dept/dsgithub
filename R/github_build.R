#' @title gh call that handles errors
#' @description uses the gh package and gh funciton
#' https://github.com/r-lib/gh
#' @export 
safe_gh <- purrr::safely(gh::gh) # in purr package it makes it so code does not break on errors.



#' @title Invite into Organization
#' @description Invite members into the org
#' @param email is the email allowed in org
#' @param org is the created org
#' @param team_ids are the team ids returned from github
#' @param role can be "admin" or "direct_member" defaults to "direct_member"
#' @return A table with "name", "value", and "type" columns as digested from the API. 
#' @details see "https://docs.github.com/en/enterprise-cloud@latest/rest/orgs/members?apiVersion=2022-11-28#create-an-organization-invitation". The id in the table is the invitation id.
#' @export 
ghb_invite_org <- function(email, org, team_ids = NULL,role = "direct_member") {
  info <- safe_gh("POST /orgs/{org}/invitations",
      org = org,
      email = email,
      role = role,
      team_ids = team_ids)

  if (!is.null(info$error)) {
    output <- info$error$response_content$errors[[1]]
    out <- tibble::tibble(name = names(output), value = unlist(output), type = "error")
    } else {
    output <- unlist(output)
    output_names <- names(output)
    out <- tibble::tibble(name = output_names, value = output, type = "response") |>
      dplyr::filter(!stringr::str_detect(name, "\\.") |  name %in% c("inviter.id", "inviter.login"))
    }
  out
}


#' @title Manage SSO members
# https://docs.github.com/en/enterprise-cloud@latest/rest/scim/scim?apiVersion=2022-11-28
# https://docs.github.com/en/enterprise-cloud@latest/rest/enterprise-admin/scim?apiVersion=2022-11-28

#' @title Create Teams
#' @description Create the teams for students
#' 

#' @title Assign Permissions to team


#' @title Manage Members of Teams
#' @description Add and remove students from teams
# https://docs.github.com/en/rest/teams/members?apiVersion=2022-11-28#add-or-update-team-membership-for-a-user

#' @title Create Repo from Template
#' @description Use a template repoe to create repositories
#' 

#' @title Add Teams and Members to Repositories
#' @description Assign members and teams to specific repositories



# #' Check of their name exists
# #' @export
# dwv_check_names <- function(x) {
#   ## ping the usernames
#   ## the safe_gh will protect for bad github usernames
#   res <- map(x$gitName,
#              ~ safe_gh("/users/:username", username = .x, .limit = 100))
#   res <- transpose(res)

#   ("Names that are missing github account username")
#   has_ghname <- res$error %>% map_lgl(is.null)
#   print(x[!has_ghname,])
#   # https://simonsmith.github.io/github-user-search/#/search

#   print("Send these people an email.")
#   print(x[!has_ghname,"BYUI Email"] %>% pull() %>% str_c(collapse = ", "))

#   # List of students with github ids
#   git_students <- x$gitName[has_ghname]
#   list(has_ghname = has_ghname, git_students = git_students)
# }

# #' Check if the repo exists
# #' @export
# dwv_check_repos <- function(x) {
#   ## find out which repos already exist
#   res <- map(x$name,
#              ~ safe_gh("/repos/BYUI335/:repo", repo = .x))
#   res <- transpose(res)
#   oops <- res$error %>% map_lgl(Negate(is.null))

#   print("Repos that need to be created")
#   print(x$name[oops]) ## needs to be created

#   print("Repos that have been created")
#   print(x$name[!oops]) ## been created

#   oops

# }

# #' Create the repo
# #' @export
# dwv_create_repo <- function(name, description) {
#   safe_gh("POST /orgs/BYUI335/repos", name = name, description = description,
#           private = TRUE, has_wiki = FALSE, auto_init = TRUE)
# }

# #' Copy from template repo
# #' @export
# dwv_copy_repo <- function(name, description) {
#   safe_gh("POST /repos/BYUI335/DS350_Template/generate", owner = "BYUI335",
#           name = name, description = description, private = TRUE,
#           .accept = "application/vnd.github.baptiste-preview+json")
# }

# #' @title Github Repo Delete
# #' @param owner_name is the Github group or user where the package is stored.
# #' @param repo_name is the name of the repo to delete.
# #' @export
# dwv_delete_github <- function(repo_name, owner_name = "BYUI335") {

#   safe_gh("DELETE /repos/:owner/:repo", owner = owner_name, repo = repo_name)

# }


# ###########################   First Time  ###############################

# ## create the students team for first time
# #' Create the groups for the semester
# #' @export
# dwv_create_groups <- function(semester_name, ta_name) {
#   gh("POST /orgs/BYUI335/teams", name = semester_name,
#      description = "Students in BYU-I MCS 335")
#   gh("POST /orgs/BYUI335/teams", name = ta_name,
#      description = "TAs in BYU-I MCS 335")
# }

# #' Create team for the semester
# #' @export
# dwv_get_teams <- function(semester_name, ta_name) {
#   teams <- gh("/orgs/BYUI335/teams")

#   students_team_id <- teams %>%
#     map_df(`[`, c("name", "id")) %>%
#     ## a lookup verb would be awesome
#     filter(name == semester_name) %>%
#     .$id

#   ta_team_id <- teams %>%
#     map_df(`[`, c("name", "id")) %>%
#     ## a lookup verb would be awesome
#     filter(name == ta_name) %>%
#     .$id

#   ## who's already on the team?
#   res <- gh("GET /teams/:id/members", id = students_team_id, .limit = 100)
#   team_members <- res %>%
#     map_chr("login")

#   tibble(students_team_id = students_team_id, ta_team_id = ta_team_id, student_members = team_members, )
# }


# #' @title Invite missing to the team.  They should get an email.
# #' @export
# dwv_invite_group <- function(x, students_team_id) {
#   missing <- x$gitName
#   res <- map(missing,
#              ~ safe_gh("PUT /teams/:id/memberships/:username",
#                        id = students_team_id, username = .x))

#   res <- transpose(res)
#   print(res$error %>% map_lgl(is.null) %>% table())
#   out <- res$result %>%
#     map_df(`[`, c("state", "role")) %>%
#     print(n = Inf)
#   print(out)
#   out

# }



# #' @title Give students r/w access to their repository
# #' @note https://developer.github.com/v3/repos/collaborators/#add-user-as-a-collaborator
# #' @export
# dwv_add_student <- function(x) {
#   res <- map2(x$name, x$gitName,
#               ~ safe_gh("PUT /repos/BYUI335/:repo/collaborators/:username",
#                         repo = .x, username = .y))
#   res <- transpose(res)
#   res$error
# }

# ## give the student team read access to these repos.
# ## Now every student in the category can see all repositories
# #' Add group
# #' @export
# dwv_add_group <- function(x, perms = "pull", team_id = students_team_id) {
#   res <- purrr::map(x$name, ~safe_gh("PUT /teams/:id/repos/BYUI335/:repo",
#                        id = team_id, repo = .x, permission = perms))
#   res <- transpose(res)
#   res$error

# }
# ## This removes the a user from getting notified of all the changes made to the repositorys.
# #' Remove user from watching the repo
# #' @export
# dwv_remove_watching <- function(x, username = "hathawayj") {
#   res <- map(x$name,
#              ~ safe_gh("DELETE /repos/:owner/:repo/subscription",
#                        owner = "BYUI335", repo = .x))
#   transpose(res)
# }

# ## look at all the repos associated with this team
# #' Find repos with the team
# #' @export
# dwv_team_repos <- function(team_id) {
#   res <- gh("/teams/:id/repos", id = team_id, .limit = 100)
#   res %>% map_chr("name")
# }





