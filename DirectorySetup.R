# This file initializes the directories for the state level campaign Finance data, 
# It also gives the power shell command for the relevant docker instance


#______  _______  ______   ______ 
#/ |        | |   / |  | \ | |  | \
#'------.   | |   | |  | | | |__|_/
# ____|_/   |_|   \_|__|_/ |_|     

#Notice: This script is going to make 52 folders. Please make sure you have the
# Working directory set to the desired directory. You can do this by hitting
# ctrl+shift+h on windows or Cmd+shift+H on Mac and selecting a directory

################################################################################
# Creates Directory
dir.create("DockerHome") #Docker TMP home directory
dir.create("Processing") #Spot to store non permanent files

for(i in 1:length(state.name)){ # Creates States
  dir.create(state.name[i])
}

################################################################################
#Selenium Overhead
DockerPrompt <- function() {
  content <- paste("Now go to powershell and run the following command:\n\n", "docker run -d -p 4445:4444 --name DockerName -v ", gsub("//", "\\\\", getwd()),"/DockerHome:/tmp selenium/standalone-firefox:2.53.1\n\n", sep = "")
  cat(content)
  readline(prompt= "Enter [Y] to Continue: ")
  return("Y")
  
} 
DockerPrompt()