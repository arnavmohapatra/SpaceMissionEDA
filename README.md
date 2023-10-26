# SpaceMissionEDA
Analyzing historical Space Mission Launch Data in order to understand the fluctuations in the space launch industry since 1957. Project completed for EAS 345: Introduction to Data Analysis

Clone the Repository:
  Open a terminal or command prompt.
  Navigate to the folder where you want to clone the repository using the cd command.
  Run the clone command:
  git clone {link of repository}


Commit Changes
  Add the changes to the staging area:
  git add .
  The . adds all changes in the current directory. You can replace . with specific file names if you prefer.
  Commit the changes:
  git commit -m "Commit message"

Push Changes:
  Before pushing, it’s a good practice to pull the latest changes from the remote repository:
    git pull origin main
  Replace "main" with the branch you are working on if it's not the main branch.
  
  Push your changes to the remote repository:
    git push origin main

Merge:
  git checkout main\n
  git fetch origin\n
  git merge origin/feature_branch\n
  git add .
  git commit
  git push origin main
