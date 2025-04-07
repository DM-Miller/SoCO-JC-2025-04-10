library(fs)
library(git2r)
library(quarto)

render_commit_push <- function(branch = "main", clean_docs = FALSE) {
  # Step 0: Optionally delete and recreate docs/
  if (clean_docs && dir_exists("docs")) {
    dir_delete("docs")
    dir_create("docs")
    message("🧹 Cleaned and recreated docs/ folder.")
  }
  
  # Step 1: Render all index.qmd files
  index_files <- dir_ls(path = ".", recurse = TRUE, regexp = "index\\.qmd$")
  for (file in index_files) {
    message(paste0("📄 Rendering: ", file))
    quarto_render(input = file)
  }
  
  # Step 2: Initialize repo
  repo <- repository(".")
  
  # Step 3: Check for deleted files
  status_pre <- status(repo)
  deleted_files <- names(status_pre$staged$deleted)
  if (length(deleted_files) > 0) {
    message("🗑 Removing deleted files from Git...")
    for (f in deleted_files) {
      git2r::remove(repo, f)
    }
  }
  
  # Step 4: Stage all changes
  git2r::add(repo, path = ".")
  
  # Step 5: Commit if there are changes
  status_post <- status(repo)
  if (length(status_post$staged) > 0) {
    message("✅ Committing and pushing changes...")
    git2r::commit(repo, message = "Render all index.qmd files, update docs/, and sync deletions")
    system(paste("git push origin HEAD:refs/heads", branch))
  } else {
    message("✔ No changes to commit.")
  }
}
