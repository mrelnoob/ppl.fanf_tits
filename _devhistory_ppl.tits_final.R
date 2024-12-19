############## *-------------------------------------------------* #############
####################### DEV_HISTORY FOR {ppl.fanf_tits} ########################
############## *-------------------------------------------------* #############

# All comments in this file or in this project are firstly meant for future me and, secondly,
# for others so they can reproduce my work. That is why there are so many comments, so anyone can
# do what I did, regardless of his/her own experience. Note also that English and R are not my
# native languages, so please forgive any mistakes.





# -------------------------------- #
##### 0. Important R reminders #####
# -------------------------------- #

##### * 0.1. NA handling -------------------------------------------------------
# ---------------------------------------------------------------------------- #

# Although it may seem strange, R doesn't handles NA as we could expect it. The use of "is.na()" or
# "!is.na()" should be preferred over the use of classical equality operators "==" or "!=".
# Cf. https://stackoverflow.com/questions/28857653/removing-na-observations-with-dplyrfilter:
# "The == operator does not treat NAs as you would expect it to. Think of NA as meaning
# "I don't know what's there". The correct answer to 3 > NA is obviously NA because we don't know
# if the missing value is larger than 3 or not. Well, it's the same for NA == NA. They are both
# missing values but the true values could be quite different, so the correct answer is "I don't
# know." R doesn't know what you are doing in your analysis, so instead of potentially introducing
# bugs that would later end up being published and embarrass you, it doesn't allow comparison
# operators to think NA is a value".
# CONSEQUENTLY, I should not use "blabla != 'NA'" either but instead use the is.na() function:
# e.g. "is.na(blabla) == FALSE" or "!is.na(blabla)"!



##### * 0.2. Factor and character variables ------------------------------------
# ---------------------------------------------------------------------------- #

# R is kind of a pain in the arse when it comes to dealing with categorical variables.
# Theoretically, these are `character` objects but most R functions expect `factor` objects.
# So you have to frequently juggle between those two types of data format and remember that
# a `factor` has `levels` and thus that what you see on your screen is not how R handles
# categorical variables internally. So to modify a `factor` object, you often have to convert
# it first into a `character` object first.
# Cf. many of my custom functions.



##### * 0.3. Package building, {targets} and RMarkdown -------------------------
# ---------------------------------------------------------------------------- #

# To optimally associate stuff like Package building, {targets}, renv and RMarkdown, I should
# probably refer to: https://books.ropensci.org/targets/





###################### ********************************************* ###########################
# ------------------------- #
##### 1. Project set up #####
# ------------------------- #

##### * 1.1. Keeping track of changes (Git) ------------------------------------
# ---------------------------------------------------------------------------- #

# To keep track of all future changes and have a backup, I need to initiate a Git version control
# repository and link my R project folder to my GitHub account.
# As I'm connecting my 'ppl.fanf_tits" project AFTER its creation, I'm following a 'GitHub-last'
# procedure:
# STEP 1: Make sure Git is installed:
system("git status") # If not, install it (along with Git Bash)!
# STEP 2: Enter your R project and type 'git init' in the R Terminal, it will initiate a Git
# repository within your project:
system("git init") # NOTE: the 'system()' function enables sending commands to the Terminal/CLI,
# that is to work in "command-lines" (hence the name CLI, for "command-lines interface").
# It works but RStudio seems unable to display the CLI responses like a regular Terminal,
# preventing interactions with it. That is why I sometimes say that I had to do stuff directly
# in a CLI/Terminal.
# STEP 3: Verify your configuration:
# To verify your "user.name" and "user.email", you can use a CLI to run the following line:
# git config user.name # OR 'git config user.email' ==> If nothing appears on the prompt, then your
# git account isn't set up and you should run:
git config --global user.name 'yourname'
git config --global user.email 'your_email@example.com'
# Of course, you need to personalize the user.name and email. If you want the config to be
# only true for the current project (and not global), remove the '--global' part from the
# previous lines.
# STEP 4: If you've never done it before (on this computer), you also need to set-up SSH keys
# to be able to connect to Git and GitHub without supplying your username and password every
# time you do something.
# The full procedure is described here: https://help.github.com/articles/generating-ssh-keys
# But here goes. First, open Git Bash and run:
ls -al ~/.ssh # Check for existing SSH keys. The "~" obviously represents the relative
# path to the folder your investigating.If no SSH key is found, you must generate one.
ssh-keygen -t ed25519 -C "your_email@example.com" # Generates a new SSH key using the ed25519
# algorithm (if your system doesn't handle this algorithm, refer to the URL above). The prompt
# will ask you to enter a file name to which save the key. You can simply press enter to create
# a '.ssh' hidden folder in the default directory: c:/Users/YOURNAME/

# Second, you have to assess whether the 'ssh-agent' tool is running. To do that, you need to
# open a Windows PowerShell in admin mode (with a right click) and run the following line:
Get-Service -Name ssh-agent # If it is disabled, please refer to the URL above to start it.
# You can then use a regular CLI to add the existing (or created) SSH key to the 'ssh-agent':
ssh-add c:/Users/YOURNAME/.ssh/id_ed25519

# The next step it to add the public key to your GitHub account settings. For more info, you
# can go to:
# https://kbroman.org/github_tutorial/pages/first_time.html, or
# https://docs.github.com/fr/authentication/connecting-to-github-with-ssh/
# Overall, you need to first copy the ssh key content without adding anything, not even a
# blank space. In the CLI run:
clip < c:/Users/YOURNAME/.ssh/id_ed25519.pub
# Go to GitHub, click on your profile picture then 'Settings' > 'SSH and GPG keys' > 'New SSH
# key', give it a title (e.g. "Work computer key"), choose the 'authentication' mode, paste
# your key, and click on 'Add SSH key'. You'll be asked to enter your password, and then it's
# done.
# To check if it works:
ssh -T git@github.com # If it does, it should answer with something like "Hi USERNAME! You've
# successfully authenticated, but Github does not provide shell access".
# It works, hooray!

# STEP 5: Make some changes in the project and make your first commit:
usethis::use_git(message = ":tada: Initial commit") # Then restart RStudio and the Git tab will
# appear on the top-right box of RStudio.
# STEP 6: Log in your GitHub account and create a new repository (without anything in it).
# STEP 7: Use the following command to associate your R project with the GitHub project:
git remote add origin git@github.com:mrelnoob/ppl.fanf_tits # Here also, personalize
# with your own account and project names! It's ok if nothing happens.
# STEP 8: Finally, you can push the changes made to your local files to GitHub:
git push -u origin master # There, something should appear to tell you what you've pushed to
# your GitHub project.
# Even using a CLI (e.g. GitBash), you may receive an error message saying that the remote
# repository contains work that you do not have locally (...). It happens if you included files
# in your GitHub project when you created it (e.g. a README or a LICENCE file). So theoretically,
# you need to always start with a "pull" before you push anything! If, like me, you forgot,
# you'll be in trouble and won't be able to pull. To force Git, you may use "git push -f origin
# master" (the -f means to "force" the push).
# IMPORTANT NOTE: because of the CLI-RStudio bugs, I can "commit" from RStudio but I cannot push,
# so I will always be forced to do it from a CLI every time!

# To ignore changes made to the Rproj file:
usethis::use_git_ignore("jk.fanf.tarping.Rproj")



##### * 1.2. Managing the project library (renv) -------------------------------
# ---------------------------------------------------------------------------- #
renv::init() # To initiate the project local library manager named 'renv'. Among other things, it
# created a 'renv' folder and a 'renv.lock' file. This file is the only thing that you need to be
# able to recreate the same R environment (correct package versions) as the one I used in this
# project PROVIDED THAT you use the same R version as me (currently R 4.4.1).

# To install a package into the 'renv' project, use:
renv::install() # You can either specify the package name manually (e.g. "ggplot2") or you can
# call the function as is and it will install all the packages of your DESCRIPTION file (if you
# have one), and also screen R and Rmd files and install all packages mentioned as 'library(pkg)',
# 'require(pkg)', and 'pkg::fun()'! Pretty cool, right?

# To check the status of your 'renv' project:
renv::status()

# To update the lockfile (to add or remove packages):
renv::snapshot() # It will compare what's inside the R project and what is listed in the lockfile
# and ask if you want to add or remove the packages that are not yet on both projects. You can
# also use:
renv::clean()

# To restore another 'renv' project (if you want to create the local library of another project,
# yours or someone else's):
renv::restore() # You may need to use 'renv::init()' first if you only have a lockfile. Remember
# though, you need to install the same major R version as the one used to create the 'renv'
# project!

# Other functions:
renv::deactivate()       # Deactivate local environment
renv::activate()         # (Re)activate local environment
renv::dependencies()     # List used packages (R and Rmd files)
renv::history()          # Browse previous commits (with git)





##### * 1.3. Project architecture ----------------------------------------------
# ---------------------------------------------------------------------------- #

# To create a folder containing my data and functions:
dir.create("data") # In which I manually paste my raw dataset!
dir.create("R")
# To create other useful folders for my project:
dir.create("output")
dir.create("output/plots")
dir.create("output/tables")
dir.create("output/texts") # I could also create folders directly in my functions, which could be
# particularly appropriate if I implement a {target} pipeline.

# If I am building an R package, then these folders are not supposed to be here and should be
# ignored (here it's not the case, so I don't run these lines):
# usethis::use_build_ignore("output/")
# usethis::use_build_ignore("tables/")
# usethis::use_build_ignore("texts/")
# usethis::use_build_ignore("plots/")
# usethis::use_git_ignore("plots/") # To avoid saturating Git, I ignore the folders prone to
# # contain rather heavy files such as spatial layers and plots, but not tables and texts!
# # NOTE: the best way to build a package is not this one (see other '_devhistory' files or
# # refer to section 0.3. of this document).





##### * 1.4. Creating scripts for custom functions -----------------------------
# ---------------------------------------------------------------------------- #
usethis::use_pipe() # To be able to use pipes (i.e. %>%). ONLY WORKS WITH PACKAGES! If you are not
# building a package (like me today), you'll have to explicitly call the {magrittr} package in
# your R files.
file.create(... = "R/01_01_importing_data.R")
file.create(... = "R/01_02_utility_functions.R")
file.create(... = "R/01_03_preparing_data.R")





##### * 1.5. Creating reports (RMarkdown) --------------------------------------
# ---------------------------------------------------------------------------- #

file.create(... = "output/texts/exploratory_data_analyses.Rmd") # Using this command,
# a .Rmd file will be created but will lack the YAML header skeleton that should thus be manually
# placed at the top of the document.



### ** 1.5.1. To manage citations and bibliography ----
# _____________________________________________________

# To manage citations and get an automatic bibliography with RMarkdown, I have to follow these
# steps:
#  1) Using Zotero (or something similar), I have to 'export' the references to be cited in the
#     report in a BibTex format (.bib) and place this text file in the same folder as my .Rmd file.
#  2) Call this document in the `bibliography` field in the YAML metadata header (e.g.
#     bibliography: my_example.bib).
#  3) In text, I use arobases (@) and brackets ([], use semi-colons ";" for separation between
#     references) to add citations (e.g. "@Martin2022 said that..." or "blabla [@Martin2022;
#     see also @Darwin1832]").
#  4) I can change the citation style by using the `csl` field in the YAML metadata header
#     (e.g. csl: my_style.csl) and pasting the said style in the same folder as before.
# Thus, I pasted my BibTex file in the same folder as my .Rdm file. But in the case where my .Rmd
# file would be at the root of my package, I need to tell R to ignore it:
usethis::use_build_ignore("mybiblio.bib") # Does not exist, it's just an example!
# For practical reasons, this .bib file will certainly be updated many times during the duration
# of the project. Also, it may be useful to manually edit the file to shorten the reference tags
# since Zotero tends to create long tag using the name of the 1st author, the 1st work of the
# title and the year of publication.





##### * 1.6. Add a README file to the project ----------------------------------
# ---------------------------------------------------------------------------- #

usethis::use_readme_rmd() # Creates a README.Rmd and adds it automatically to .Rbuildignore (and
# opens it). After manually editing the file, we need to compile it into a .md document
# (otherwise, sites such as GitHub or the CRAN won't be able to display it on their pages):
rmarkdown::render("README.Rmd")
# As render() also produces a .html file that is not useful here, we will ignore it:
usethis::use_build_ignore("README.html")
usethis::use_git_ignore("README.html")





########### *-----------------------------------------------------* ############
############################ TO DO LIST ##################################
# ---------------------------------------------------------------------------- #
# *** Produire un fichier de metadata (qui explique notamment comment ont été
#     calculé chaque données).
# *** Renvoyer vers le metadata partout où j'en parle (e.g. fonctions, etc.) ==> il
#     faut que je fasse une recherche "***".
# ---------------------------------------------------------------------------- #


########### *-----------------------------------------------------* ############
############################ Main Git commits ##################################
# ---------------------------------------------------------------------------- #
usethis::use_git(message = ":boom: Exported the 1st updated results!")
usethis::use_git(message = ":metal: Created a new function")
usethis::use_git(message = ":zap: Ignoring something")
usethis::use_git(message = ":pencil: Documented a function or wrote something")
usethis::use_git(message = ":hammer: Ongoing programming!")
usethis::use_git(message = ":white_check_mark: Proofed the 'dev_history' file")
usethis::use_git(message = ":x: Problem detected!")
#system("git push") # Or using a CLI!
# Don't forget to push your commits once you're sure you made no mistakes.
# ---------------------------------------------------------------------------- #
# ------------------------------- THE END ------------------------------------ #
########### *-----------------------------------------------------* ############
