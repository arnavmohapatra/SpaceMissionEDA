# SpaceMissionEDA
Analyzing historical Space Mission Launch Data in order to understand the fluctuations in the space launch industry since 1957. Project completed for EAS 345: Introduction to Data Analysis

**Cloning the Repository**:
  1. Open a terminal or command prompt.
  2. Navigate to the folder where you want to clone the repository using the cd command.
  3. Run the clone command:
      `git clone {link of repository}`


**Commiting Changes**:
  1. Add the changes to the staging area:
      `git add .`
     The `.` adds all changes in the current directory. You can replace `.` with specific file names if you prefer.
  2. Commit the changes:
      `git commit -m "Commit message"`

**Pushing Changes**:
  1. Before pushing, it’s a good practice to pull the latest changes from the remote repository:
      `git pull origin main`
  2. Replace `"main` with the branch you are working on if it's not the `main` branch.
  3. Push your changes to the remote repository:
      `git push origin main`

**Merging Changes**:
  1. `git checkout main`
  2. `git fetch origin`
  3. `git merge origin/feature_branch`
  4. `git add .`
  5. `git commit`
  6. `git push origin main`
