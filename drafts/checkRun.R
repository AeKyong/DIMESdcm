initiatedReps = 1:60000

# get list of jobs running currently (need to check what happens when 0 are running)
runningJobs = system("qstat -u aekjung", intern=TRUE)


if (length(runningJobs) > 2){
  runningJobs = runningJobs[3:length(runningJobs)]
}


list_of_jobs <- lapply(runningJobs, function(line) {

  # 1. Split the line by any amount of whitespace and get the last element.
  # This is more robust than splitting by a single space.
  parts <- strsplit(line, "\\s+")[[1]]
  task_id_string <- parts[length(parts)]

  # 2. Check if the string contains a hyphen, which indicates a range.
  if (grepl("-", task_id_string)) {

    # --- It's a range (e.g., "40958-41000:1") ---

    # First, remove the step size (e.g., ":1") if it exists
    range_only <- gsub(":.*", "", task_id_string)

    # Split the remaining "start-end" string into two parts
    start_end <- strsplit(range_only, "-")[[1]]

    # Convert the parts to numbers
    start_num <- as.numeric(start_end[1])
    end_num <- as.numeric(start_end[2])

    # Generate the sequence of numbers and return it
    return(seq(from = start_num, to = end_num))

  } else {

    # --- It's just a single number ---
    return(as.numeric(task_id_string))
  }
})


currentlyRunning = unlist(list_of_jobs)
# system(paste0("cat", length(currentlyRunning)))
nCurrentlyRunning = length(currentlyRunning)

# determine which files are complete
completedJobs = system("ls *.RData", intern=TRUE)

# remove *.RData
completedJobs = as.numeric((gsub(x = completedJobs, pattern="rep_|.RData", replacement="")))
# system(paste0("cat", length(completedJobs)))


terminatedJobs = which(!initiatedReps %in% c(currentlyRunning, completedJobs))
# system(paste0("cat", length(terminatedJobs)))

submittingJobs = terminatedJobs[1:(10000-nCurrentlyRunning)]

system("pwd")
write.table(submittingJobs, "../terminatedJobs.txt", sep="\n", row.names = F, col.names = F, quote=F)


