# Group Project Collaboration Guide
## Using GitHub with RStudio

Welcome to our group project! This guide will help you set up and use our shared GitHub repository for collaboration.

## 🔗 Repository Link
**Our Project Repository:** https://github.com/lal-quintero/group-rmarkdown-project

---

## 📋 One-Time Setup (Do This First!)

### Step 1: Install Required Software
1. **Install R and RStudio** (if you haven't already)
2. **Install Git**: Download from https://git-scm.com/download/win (Windows) or use Homebrew on Mac
3. **Create a GitHub account** at https://github.com (free)

### Step 2: Configure Git in RStudio
Open RStudio and run these commands in the **Console**:

```r
# Install required packages
install.packages(c("usethis", "gitcreds"))
library(usethis)

# Set up your Git identity (use YOUR name and email)
use_git_config(user.name = "Your Full Name", 
               user.email = "your.email@gmail.com")

# Create a GitHub token
create_github_token()
```

When the GitHub website opens:
1. Log in to your GitHub account
2. Give the token a name like "RStudio Group Project"
3. Click "Generate token"
4. **Copy the token** (starts with `ghp_`)

Back in RStudio:
```r
# Store your token
gitcreds::gitcreds_set()
```
Paste your token when prompted.

### Step 3: Set Git Path in RStudio
1. Tools → Global Options → Git/SVN
2. Set Git executable path to: `C:/Program Files/Git/bin/git.exe` (Windows)
3. Click OK and restart RStudio

---

## 📥 Getting the Project (Clone the Repository)

### Method 1: Through RStudio (Recommended)
1. Open RStudio
2. File → New Project → Version Control → Git
3. **Repository URL:** `https://github.com/lal-quintero/group-rmarkdown-project.git`
4. **Project directory name:** `group-rmarkdown-project`
5. **Create project as subdirectory of:** Choose where you want it saved
6. Click "Create Project"

### Method 2: Through R Console
```r
# Navigate to where you want the project
setwd("C:/Users/YourName/Documents")

# Clone the repository
system("git clone https://github.com/lal-quintero/group-rmarkdown-project.git")
```

---

## 📁 Project Structure

Our project is organized as follows:
```
group-rmarkdown-project/
├── workings/
│   ├── data-cleaning.R      # Data preparation scripts
│   ├── analysis.R           # Statistical analysis
│   └── visualization.R      # Plots and charts
├── final.assignments/
│   ├── final-report.Rmd     # Main report document
│   └── group-presentation.qmd  # Presentation slides
```

---

## 🔄 Daily Workflow (Very Important!)

### Before You Start Working:
Always pull the latest changes first!
```r
system("git pull")
```

### After Making Changes:
1. **Check what you changed:**
```r
system("git status")
```

2. **Add your changes:**
```r
system("git add .")
```

3. **Commit with a descriptive message:**
```r
system('git commit -m "Added data visualization for sales analysis"')
```

4. **Push to GitHub:**
```r
system("git push")
```

---

## 💡 Best Practices

### ✅ DO:
- **Pull before you start working** each time
- **Commit frequently** with clear messages
- **Work on different files** when possible to avoid conflicts
- **Use descriptive commit messages** like:
  - "Fixed data cleaning script"
  - "Added regression analysis"
  - "Updated presentation slides"

### ❌ DON'T:
- Work without pulling first
- Use vague commit messages like "updates" or "changes"
- Work on the same file simultaneously without coordination

---

## 🎯 File Assignments

To avoid conflicts, let's work on different files:

| Team Member | Primary Files |
|-------------|---------------|
| Person 1 | `workings/data-cleaning.R` |
| Person 2 | `workings/analysis.R` |
| Person 3 | `workings/visualization.R` |
| Person 4 | `final.assignments/final-report.Rmd` |
| Everyone | `final.assignments/group-presentation.qmd` |

---

## 🆘 Troubleshooting

### "Git not found" error:
- Check Tools → Global Options → Git/SVN
- Make sure Git executable path is set correctly

### Push/pull conflicts:
1. Pull first: `system("git pull")`
2. If conflicts, resolve them in RStudio
3. Commit the resolution
4. Push again

### Forgot to pull before working:
1. Commit your changes first
2. Try: `system("git pull")`
3. Resolve any conflicts
4. Push your changes

---

## 🔧 Useful Commands Reference

```r
# Check status
system("git status")

# See recent changes
system("git log --oneline -5")

# Pull latest changes
system("git pull")

# Add, commit, and push (the main workflow)
system("git add .")
system('git commit -m "Your message here"')
system("git push")
```

---

## ❓ Need Help?

1. Check this guide first
2. Ask in our group chat
3. Check the GitHub repository issues tab
4. Google the error message + "git"

---

**Remember:** Communication is key! Let the team know what you're working on to avoid conflicts.

**Happy collaborating! 🚀**
