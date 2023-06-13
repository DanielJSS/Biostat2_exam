git_default_branch_rename()

use_github()
use_git_config(
  user.name = "DanielJSS", 
  user.email = "danieljss98@gmail.com"
)
gitcreds::gitcreds_set()
3
git remote add origin https://github.com/DanielJSS/Biostat2_exam.git
git branch -M main
git push -u origin main
