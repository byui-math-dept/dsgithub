#' @title Clone Repo
#' @param git_dir is the folder on your local computer where you store your git repositories
#' @param group_dir is the folder for the GitHub group id.  It
#' @param repo_use is the repository name for the clone
#' @export

git_clone <- function(git_dir, group_dir, repo_use){

  folder_dir <- file.path(git_dir, group_dir, repo_use)
  git_command <- glue::glue("git clone https://github.com/{group}/{repo}.git {folder}", group = group_dir, repo = repo_use, folder = folder_dir)
  system(git_command)

}

#' @title Pull Latest from Repo
#' @param git_dir is the folder on your local computer where you store your git repositories
#' @param group_dir is the folder for the GitHub group id.
#' @param repo_use is the repository name for the pull
#' @export

git_pull <- function(git_dir, group_dir, repo_use){

  #git --git-dir=/Users/jhathaway/git/byuistats/datasciencedegree/.git
  folder_dir <- file.path(git_dir, group_dir, repo_use)
  git_command <- glue::glue("git -C {folder} pull", group = group_dir, repo = repo_use, folder = folder_dir)
  system(git_command)

}

#' @title Commit and Push Latest Updates to Repo
#' @param git_dir is the folder on your local computer where you store your git repositories
#' @param group_dir is the folder for the GitHub group id.
#' @param repo_use is the repository name for the push
#' @param message is the commit message to use
#' @export

git_push <- function(git_dir, group_dir, repo_use, message = "'First Push from Hathaway'"){

  folder_dir <- file.path(git_dir, group_dir, repo_use)

  # The three commands to move files to github
  git_add <- glue::glue("git -C {folder}  add .", folder = folder_dir)
  git_commit <- glue::glue("git -C {folder} commit -m {message}", folder = folder_dir, message = message)
  git_push <- glue::glue("git -C {folder} push ", group = group_dir, repo = repo_use, folder = folder_dir)

  # Execute commands on system
  system(git_add)
  system(git_commit)
  system(git_push)

}


#' @title List all Repos
#' @param group_dir is the folder for the GitHub group id.
#' @return a vector of repository names.  Can be used in `git_clone` and `git_pull`
#' @export

list_repos <- function(group_dir, org = TRUE){

  # "Generate new token". Give it a nickname that reminds you of the intended purpose, e.g., "devtools".
  #cat("GITHUB_PAT=dkdlkfdlkdflkjfdljkdflkjdflkjdfs\n",
  #    file = file.path(normalizePath("~/"), ".Renviron"), append = TRUE)

  if(org == TRUE){
    repos <- gh::gh("GET /orgs/:org/repos", org = group_dir, .limit = 20000)
    out <- unlist(purrr::map(repos, "name"))
  }
  if(org == FALSE){
    repos <- gh::gh("GET /users/:username/repos", username = group_dir, .limit = 20000)
    out <- unlist(purrr::map(repos, "name"))
  }
  out
}








