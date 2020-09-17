

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


# https://developer.github.com/v3/issues/
# https://developer.github.com/v3/issues/comments/#create-a-comment
# body_lines = read_lines("https://byuistats.github.io/M335/tasklist.md")

dwv_issue_tasks <- function(x = body_lines, task_title = "Daily Class Tasks",
                            dat = repo_create_df_new) {

  start_task <- x %>% str_which("### Tasks 1 - 12")
  task_lines <- x[start_task:length(x)] %>%
    replace_links("class_tasks/", "https://byuistats.github.io/M335/class_tasks/")
  post_issue_tasks(x = task_lines,
                   repo_create_df_new = dat,
                   x_title = task_title)

}

dwv_issue_semester <- function(x = body_lines, dat = repo_create_df_new) {
  start_semdel <- x %>% str_which("## Semester Deliverables")
  start_cs <- x %>% str_which("## Weekly Case Studies")

  semdel_title <- x[start_semdel] %>% str_remove("## ")
  semdel_lines <- x[start_semdel:(start_cs - 1)] %>%
    replace_links("weekly_projects/", "https://byuistats.github.io/M335/weekly_projects/") %>%
    replace_links("teamlead.html", "https://byuistats.github.io/M335/teamlead.html") %>%
    replace_links("project.html", "https://byuistats.github.io/M335/project.html")
  post_issue_tasks(x = semdel_lines,
                   repo_create_df_new = dat,
                   x_title = semdel_title)

}

dwv_issue_cs <- function(x, n_cs = 13, print = FALSE, dat = repo_create_df_new) {

  start_cs <-   x %>% str_which("## Weekly Case Studies")
  end_cs <- x %>% str_which("## Daily Class Tasks")
  end_cs <- end_cs - 1
  cs_lines <- x[start_cs:end_cs] %>%
    replace_links("weekly_projects/", "https://byuistats.github.io/M335/weekly_projects/")

  for (i in n_cs:1) {

    cs_i_start <- str_which(cs_lines,
                            glue::glue("#### \\[Case Study {number}\\]*", number = i))[1]
    cs_i_end <- ifelse(i == 13, length(cs_lines),
      str_which( cs_lines, glue::glue("#### \\[Case Study {number}\\]*", number = i+1)) - 1)

    body_lines <- cs_lines[cs_i_start:cs_i_end]

    title <- glue::glue("Case Study {number}", number = i)
    body_lines <- body_lines[2:length(body_lines)]

    bob <- tempdir()
    ## Need to program in the YAML removal
    write_lines(body_lines, path = file.path(bob, "dir.md"))
    body_text <-  read_file(file.path(bob, "dir.md"))
    body_text <- paste0("### Directions\n\n See the next comment from the repo owner for file link to be reviewed. After reviewing, post a comment for them to address.\n",body_text)

    if (print) {
      cat(body_text)
      cat(title)
    }
    ## Push the task information
    res <- map(dat$name,
               ~ safe_gh("POST /repos/:owner/:repo/issues",
                         owner = "BYUI335", repo = .x,
                         title = title,
                         body = body_text))

  }

}

