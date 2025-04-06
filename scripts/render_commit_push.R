# Load necessary libraries
library(fs)
library(git2r)

# Define the function to render, commit, and push
render_commit_push <- function() {
  # Find all index.qmd files in the project
  index_files <- dir_ls(path = ".", recurse = TRUE, regexp = "index\\.qmd$")
  
  # Render each index.qmd file
  for (file in index_files) {
    quarto::quarto_render(input = file)
  }
  
  # Initialize the repository
  repo <- repository(".")
  
  # Add all changes to staging
  add(repo, path = ".")
  
  # Commit the changes
  commit(repo, message = "Render all index.qmd files and update HTML output")
  
  # Push the changes to the remote repository
  system("git push origin HEAD:refs/heads/main")
}

# Execute the function
render_commit_push()
