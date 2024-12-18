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
# Cf. https://stackoverflow.com/questions/28857653/removing-na-observations-with-dplyrfilter



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
git config user.name # OR 'git config user.email' ==> If nothing appears on the prompt, then your
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
system2("git remote add origin git@github.com:mrelnoob/jk.fanf.tarping") # Here also, personalize
# with your own account and project names! And here again, it does not work (so --> CLI).
# STEP 8: Finally, you can push the changes made to your local files to GitHub:
system2("git push -u origin master") # Same (CLI).
# Even using a CLI (e.g. GitBash), you may receive an error message saying that the remote
# repository contains work that you do not have locally (...). It happens if you included files
# in your GitHub project when you created it (e.g. a README or a LICENCE file). So theoretically,
# you need to always start with a "pull" before you push anything! If, like me, you forgot,
# you'll be in trouble and won't be able to pull. To force Git, you may use "git push -f origin
# master" (the -f means to "force" the push).
# IMPORTANT NOTE: because of the CLI-RStudio bugs, I can "commit" from RStudio but I cannot push,
# so I will always be forced to do it from a CLI every time!

