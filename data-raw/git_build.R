## Historical code to build data files

#Repository Development
#
# https://github.com/jennybc/happy-git-with-r/blob/master/81_github-api-tokens.Rmd
#cat("GITHUB_PAT=JUNKFOREBRIGHTSPACE\n", file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)

#### New Shiny App for GITHUB and CANVAS ####

# API to CANVAS COURSE must be entered by teacher.  Canvas just checks that student is in course.
# GH API connection to current course group.
# 1. Create repo from template repo
# 2. Add repo to semester group
# 4. Add user as pull and push
# 5. Add issues for task tracking
# 6. Report two invite links.


pacman::p_load_current_gh("VerbalExpressions/RVerbalExpressions")
pacman::p_load(tidyverse, readxl, gh, googledrive, googlesheets4, stringr, magrittr, glue, purrr, fs)

semester_id <- "wi20"
safe_gh <- safely(gh) # in purr package it makes it so code does not error out


## definitely ad hoc, interactive code!
## still ad hoc, interactive code with hathaway's edits!

## verifies github usernames
## create / identifies 2016 student team
## check that students are on it or add them
## gets student repos
## makes the ones that don't exist
## add student as collab to their repo
## give 2016 student team read access
## unwatch them (for me)

# Major points:
#
#   * [Create an Organization](https://help.github.com/articles/creating-a-new-organization-account/) for the course.
# - Immediately request an [Education discount](https://education.github.com) for the Organization, so that you get unlimited private repos.
# * Have your students register for free, personal [GitHub accounts](https://github.com).
# - Encourage them to request an [Education discount](https://education.github.com) on their own behalf (aka "student developer pack"). But rest assured, nothing you need for your course machinery will depend on this.
# * Get the GitHub usernames from your students -- we use a [Shiny](http://deanattali.com/blog/shiny-persistent-data-storage/) [app](http://deanattali.com/2015/06/14/mimicking-google-form-shiny/)! -- plus some shred of information that allows you link them back to your official course list.
# * Create a students [Team](https://help.github.com/enterprise/2.7/admin/guides/user-management/organizations-and-teams/) and a TA Team. I make such teams for each run of the course, e.g. `2016_students` and `2016_ta`.
# * Invite students to join your course organization and the students team. Ditto for TAs and the TA team.
# * Create a canonical name for each student, based on the official course list, i.e. `lastname_firstname`.
# * Create a repository for each student, using the student's canonical name.
# - This is a private repository within the course Organization.
# - I turn wikis off and either let GitHub auto-initialize or immediately push files, including a README, into the repos.
# - Give the student team read or pull access to each student's repo. Yes, this allows them to see each others work. I discuss this elsewhere.
# - Give the TA team write or push access to each student's repo.
# - Add the student as collaborator with write or push access.
# - Unwatch these repos personally! Wow such notification.
#
# That's the setup! I use the [gh](https://github.com/gaborcsardi/gh) and [purrr](https://github.com/hadley/purrr) packages to script all of this [GitHub API](https://developer.github.com/v3/) work. *In a second wave, I'll post code snippets for the above operations.*





# The group name for the students
semester_name <- str_c("student_", semester_id)
ta_name <- str_c("ta_", semester_id)

# moc list of github usernames.
# Still need to work out how to get these from students
# mdf <- data.frame(gitName = 1:4)
# mdf$gitName <- c("hathawayj", "dylanjm", "bmwoodruff", "bobdogdogdogdog")
# mdf$gitName


#### Read in from Google Sheets
#### https://www.r-bloggers.com/reading-data-from-google-sheets-into-r/
#gs_ls()
#
drive_auth(email = "hathawayj@gmail.com")
sheets_auth(email = "hathawayj@gmail.com")
ghu <- drive_get("https://docs.google.com/spreadsheets/d/13aQsQYnGTQXyyBUGzE1V9MExEvG5woAmygtkdjltdjk/edit#gid=730524628")


semester_id_upper <- str_to_upper(semester_id)

gh_names <- read_sheet(ss = ghu, sheet = semester_id_upper) %>%
  mutate(`BYUI Email` = str_to_lower(`BYUI Email`) %>%
           str_replace_all(rx() %>%
                             rx_find("@") %>%
                             rx_anything() %>%
                             rx_end_of_line(), "@byui.edu"))

excel_files <- fs::dir_ls(path("class_lists", semester_id_upper), regexp = "xls")

roster <- map(excel_files, ~ read_xls(.x, skip = 1, col_types = "text")) %>%
  bind_rows()

roster_teacher <- tibble(`FERPA Restrict` = "N", `Student ID` = "343419841",
                         Student = c("Hathaway, J.","Palmer, David"), Status = "Teacher",
                         `E-mail` = c("hathawayj@byui.edu", "palmerda@byui.edu"), `Cross-listed Course` = NA,
                         Major = "Faculty", Class = "Faculty")
# roster_add <- NULL
roster_add <- tibble(`FERPA Restrict` = "N",
                     `Student ID` = "999",
                     Student = c("Parkinson, Kristy"),
                     Status = "Visit",
                     `E-mail` = c("kristyleep@byui.edu"),
                     `Cross-listed Course` = NA,
                     Major = "VF",
                     Class = c("Math"))


roster <- bind_rows(roster, roster_teacher, roster_add)

### check for missing in each roster than have github name #
# People with github names in the google doc but don't show up in the roster based
# on email.
(poormatch <- gh_names %>%
    mutate(`E-mail` = `BYUI Email`) %>%
    anti_join(roster))

# check for missing in each
roster %>%
  mutate(`BYUI Email` = `E-mail`) %>%
  anti_join(gh_names) %>%
  select(`Student ID`, Student, `E-mail`, `BYUI Email`)

## This is the object that is used to process.
mdf <- roster %>%
  mutate(`BYUI Email` = `E-mail`) %>%
  left_join(gh_names) %>%
  select(`Student ID`, Student, `BYUI Email`, "gitName" = `GitHub Username`)



## ping the usernames
## the safe_gh will protect for bad github usernames
res <- map(mdf$gitName,
           ~ safe_gh("/users/:username", username = .x, .limit = 100))
res <- transpose(res)
res$error %>% map_lgl(is.null) %>% table() # 83

# return names that are missing github account username
has_ghname <- res$error %>% map_lgl(is.null)
mdf[!has_ghname,]
# https://simonsmith.github.io/github-user-search/#/search
# Send these people an email.
mdf[!has_ghname,"BYUI Email"] %>% pull() %>% str_c(collapse = ", ")

# List of students with github ids
git_students <- mdf$gitName[has_ghname]

### Set-up my path
#cat("GITHUB_PAT=M335_token_Here\n", file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)

##  Build

# Create Repositories

## prepare a data frame to drive repo creation
repo_create_df <- mdf %>%
  mutate(description = paste("MCS 335 repository for", Student, "coursework"),
         has_ghname = has_ghname,
         # a bunch of code to build a repo id for each student
         name = str_c("M335_", semester_id_upper, "_",
                      Student %>% str_split_fixed(",", 2) %>% .[,1] %>% str_replace_all(" ", "_"),
                      "_",
                      Student %>% str_split_fixed(",", 2) %>% .[,2] %>% str_sub(2,5),
                      sep = ""))
# Base file for class work during semester
write_rds(repo_create_df, str_c("class_lists/", semester_id_upper ,"/ClassList_gitnames_", semester_id, ".Rds"))




## find out which repos already exist
res <- map(repo_create_df$name,
           ~ safe_gh("/repos/BYUI335/:repo", repo = .x))
res <- transpose(res)
oops <- res$error %>% map_lgl(Negate(is.null))
table(oops)
repo_create_df$name[oops] ## needs to be created

repo_create_df$name[!oops] ## been created


create_repo <- function(name, description) {
  safe_gh("POST /orgs/BYUI335/repos", name = name, description = description,
          private = TRUE, has_wiki = FALSE, auto_init = TRUE)
}

# This filters to just the new students that don't have a repository in their name and have a github username
(repo_create_df_new <- repo_create_df %>% filter(has_ghname & oops))

## This makes the repositories for each student
res <- map2(repo_create_df_new$name, repo_create_df_new$description, create_repo)
res <- transpose(res)
status <- repo_create_df_new %>%
  mutate(cr_success = map_lgl(res$result, Negate(is.null))) %>%
  select(-description)

###########################  Now run the git_pull.R script  #####################
Sys.getenv("GITHUB_PAT")
# git functions
source("scripts/git_pull.R")

dwv_repos <- list_repos("BYUI335")
(new_dwv <- repo_create_df_new$name)

# clones created repos on local computer
map(new_dwv,~ git_clone(git_dir = "/Users/hathawayj/git", group_dir = "BYUI335",  repo_use = .x))
# for windows computer
#map(new_dwv,~ git_clone(git_dir = "c://git/github", group_dir = "BYUI335",  repo_use = .x))

# Copy folder structure into each repo.



folders_to_move <- dir_ls("/Users/hathawayj/git/BYUI335/M335_Template/", type = "directory")
gitignore_move <-  dir_ls("/Users/hathawayj/git/BYUI335/M335_Template/", type = "file", all = TRUE, regexp = "gitignore")

# Create full set of combinations and then copy to each repository
full_set <-  expand.grid(folders_to_move, new_dwv, stringsAsFactors = FALSE) %>%
  rename(folders_to_move = Var1, new_dwv = Var2)

map2(.x = full_set$folders_to_move, .y = full_set$new_dwv,
     ~dir_copy(path = .x, new_path = glue("/Users/hathawayj/git/BYUI335/{repo}", repo = .y)))

map(.x = new_dwv, ~file_copy(gitignore_move, new_path = glue("/Users/hathawayj/git/BYUI335/{repo}/{filename}", repo = .x, filename = path_file(gitignore_move)), overwrite = TRUE))


map(.x = new_dwv,~ git_push(git_dir = "/Users/hathawayj/git", group_dir = "BYUI335",  repo_use = .x))

# junk for when I mess up gitignore
# gitignore_first <-  dir_ls("/Users/jhathaway/git/BYUI335/hathaway/", type = "file", all = TRUE, regexp = "gitignore")
# map(.x = new_dwv, ~file_copy(gitignore_first, new_path = glue("/Users/jhathaway/git/BYUI335/{repo}/{filename}", repo = .x, filename = path_file(gitignore_first)), overwrite = TRUE))
# end junk






###########################   First Time  ###############################

## create the students team for first time

# gh("POST /orgs/BYUI335/teams", name = semester_name, description = "Students in BYU-I MCS 335")
# gh("POST /orgs/BYUI335/teams", name = ta_name, description = "TAs in BYU-I MCS 335")

###################################################

teams <- gh("/orgs/BYUI335/teams")

(students_team_id <- teams %>%
    map_df(`[`, c("name", "id")) %>%
    ## a lookup verb would be awesome
    filter(name == semester_name) %>%
    .$id)

(ta_team_id <- teams %>%
    map_df(`[`, c("name", "id")) %>%
    ## a lookup verb would be awesome
    filter(name == ta_name) %>%
    .$id)

#students_team_id <- ta_team_id

## who's already on the team?
res <- gh("GET /teams/:id/members", id = students_team_id, .limit = 100)
team_members <- res %>%
  map_chr("login")

## who's missing?
#(missing <- setdiff(git_students, team_members))


##############################################################
##############################################################
## invite missing to the team.  They should get an email.
## #############################################################
# repo_create_df_new <- repo_create_df
missing <- repo_create_df_new$gitName
##########################################################################
##########################################################################
##########################################################################


res <- map(missing,
           ~ safe_gh("PUT /teams/:id/memberships/:username",
                     id = students_team_id, username = .x))

res <- transpose(res)
res$error %>% map_lgl(is.null) %>% table()
res$result %>%
  map_df(`[`, c("state", "role")) %>%
  print(n = Inf)


# Give students r/w access to their repository
## https://developer.github.com/v3/repos/collaborators/#add-user-as-a-collaborator
## PUT /repos/:owner/:repo/collaborators/:username
# could use if you created repositorys before getting usernames.
#repo_create_df_new <- repo_create_df

res <- map2(repo_create_df_new$name, repo_create_df_new$gitName,
            ~ safe_gh("PUT /repos/BYUI335/:repo/collaborators/:username",
                      repo = .x, username = .y))
res <- transpose(res)
res$error


## give the student team read access to these repos.
## Now every student in the category can see all repositories
res <- map(repo_create_df_new$name,
           ~ safe_gh("PUT /teams/:id/repos/BYUI335/:repo",
                     id = students_team_id, repo = .x, permission = "pull"))
res <- transpose(res)
res$error

## give the TA team admin access to these repos.
## Now every TA in the category can see all repositories
res <- map(repo_create_df_new$name,
           ~ safe_gh("PUT /teams/:id/repos/BYUI335/:repo",
                     id = ta_team_id, repo = .x, permission = "admin"))
res <- transpose(res)
res$error


## look at all the repos associated with this team
res <- gh("/teams/:id/repos", id = students_team_id, .limit = 100)
team_repos <- res %>% map_chr("name")

## This removes the owner of BYUI3335 from getting notified of all the changes made to the repositorys.
## In this case hathawayj will be removed from watching all the student repositories.
res <- map(repo_create_df_new$name,
           ~ safe_gh("DELETE /repos/:owner/:repo/subscription",
                     owner = "BYUI335", repo = .x))
transpose(res)
# delete teachers as owners of all the repos for the students.



# Make sure to add TAs to TA group.

#############################################################
#############################################################
#############################################################
# Create Issue in Each Rep with the Task list
#############################################################
#############################################################

replace_links <- function(x, x_text, x_replace){
  x %>% str_replace_all(x_text, x_replace)
}

post_issue_tasks <- function(x, repo_create_df_new, x_title){
  bob <- tempdir()
  ## Need to program in the YAML removal
  write_lines(x, path = file.path(bob, "dir.md"))
  body_text = read_file(file.path(bob, "dir.md"))
  cat(body_text)
  safe_gh <- safely(gh::gh)

  res <- map(repo_create_df_new$name,
             ~ safe_gh("POST /repos/:owner/:repo/issues",
                       owner = "BYUI335", repo = .x, title = x_title, body = body_text))
  # Lock the issues so only the BYUI335 owner and the repo owner can make comments.
  res <- map(repo_create_df_new$name,
             ~ safe_gh("PUT /repos/:owner/:repo/issues/:number/lock",
                       owner = "BYUI335", repo = .x, number = 1))
}


# repo_create_df_new <- read_rds("ClassList_gitnames_fa18.Rds") %>%
#   filter(`BYU Email` == "hathawayj@byui.edu")

# repo_create_df_new <- read_rds(str_c("class_lists/", semester_id_upper ,"/ClassList_gitnames_", semester_id, ".Rds")) %>%
#   filter(has_ghname == TRUE)


# https://developer.github.com/v3/issues/
# https://developer.github.com/v3/issues/comments/#create-a-comment
body_lines = read_lines("docs/tasklist.md")


## Class tasks section


task_title <- "Daily Class Tasks"
start_task <- body_lines %>% str_which("### Tasks 1 - 12")
task_lines <- body_lines[start_task:length(body_lines)] %>%
  replace_links("class_tasks/", "https://byuistats.github.io/M335/class_tasks/")

start_cs <-  body_lines %>% str_which("## Weekly Case Studies")
start_semdel <- body_lines %>% str_which("## Semester Deliverables")

semdel_title <- body_lines[start_semdel] %>% str_remove("## ")
semdel_lines <- body_lines[start_semdel:(start_cs - 1)] %>%
  replace_links("weekly_projects/", "https://byuistats.github.io/M335/weekly_projects/") %>%
  replace_links("teamlead.html", "https://byuistats.github.io/M335/teamlead.html") %>%
  replace_links("project.html", "https://byuistats.github.io/M335/project.html")


(dat <- repo_create_df_new)
source("scripts/Post_issue.R")
for (case.study in 13:1) eval(post_issues)

post_issue_tasks(x = task_lines, repo_create_df_new = repo_create_df_new, x_title = task_title)
post_issue_tasks(x = semdel_lines, repo_create_df_new = repo_create_df_new, x_title = semdel_title)

##### Email invites
# https://github.com/BYUI335/M335_WI20_Parkinson_Kris/invitations # to a repo
# https://github.com/orgs/BYUI335/invitation

# Send these people an email that haven't provided info
mdf[!has_ghname,"BYUI Email"] %>% pull() %>% str_c(collapse = ", ")
