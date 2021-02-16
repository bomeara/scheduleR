#' Gathers hosts input data and formats
#'
#' This takes a CSV with columns for Name, Room, and Times (separated by commas and spaces). This sort of file could come from a Google Sheet, for example, where hosts have filled in information about times in a form with checkboxes. If there are times like 8:30 am, 9 am, etc. and slot_minutes are 15 minutes, it will assume that if someone selects 8:30 they can make both 8:30 and 8:45 slots. It assumes last names are unique and uses the humaniformat package to extract them. If you want more precise slots, you can have slot_minutes less than input_minutes: they said this, say, 30 minute slot was available, which means that both 15 minute slots within could be free.
#' @param data Input CSV file
#' @param slot_minutes How long to make each slot
#' @param input_minutes Resolution of the input slots
#' @param extract_last_name If TRUE, extract last name; if FALSE, assume only last name given
#' @return a list with availability array and a room vector (where names are last names)
#' @export
host_import <- function(data, slot_minutes=15, input_minutes=30, extract_last_name=TRUE) {
  last_names <- data$Name
  if(extract_last_name) {
  	last_names <- humaniformat::last_name(data$Name)
  }
  all_times <- unique(c(unname(unlist(sapply(unname(sapply(unique(unlist(strsplit(data$Times, ", "))), times_format)), time_interpolate, slot_minutes=slot_minutes, input_minutes=input_minutes)))))
  availability <- matrix(data=0, nrow=length(last_names), ncol=length(all_times))
  rownames(availability) <- last_names
  colnames(availability) <- all_times
  for (person_index in seq_along(last_names)) {
    person_times <- unique(c(unname(unlist(sapply(unname(sapply(unique(unlist(strsplit(data$Times[person_index], ", "))), times_format)), time_interpolate, slot_minutes=slot_minutes, input_minutes=input_minutes)))))
    for (time_index in seq_along(person_times)) {
      matching_time_index <- which(colnames(availability)==person_times[time_index])
      availability[person_index,  matching_time_index] <- 1
    }
  }
  room <- data$Room
  names(room) <- last_names
  return(list(availability=availability, room=room))
}

#' Gathers guest input data and computes availability array
#'
#' This takes a CSV with columns for Name, Room, Email, and Times (separated by commas and spaces) and Essential and Optional people to meet with (separated by commas and spaces).This sort of file could come from a Google Sheet, for example, where hosts have filled in information about times in a form with checkboxes. If there are times like 8:30 am, 9 am, etc. and slot_minutes are 15 minutes, it will assume that if someone selects 8:30 they can make both 8:30 and 8:45 slots. If you want more precise slots, you can have slot_minutes less than input_minutes: they said this, say, 30 minute slot was available, which means that both 15 minute slots within could be free.
#' @param data Input CSV file
#' @param slot_minutes How long to make each slot
#' @param input_minutes Resolution of the input slots
#' @return an availability array
#' @export
guest_availability <- function(data, slot_minutes=15, input_minutes=30) {
  #last_names <- humaniformat::last_name(data$Name)
  data$Times <- as.character(data$Times)
  data$Name <- as.character(data$Name)
  all_times <- unique(c(unname(unlist(sapply(unname(sapply(unique(unlist(strsplit(data$Times, ", "))), times_format)), time_interpolate, slot_minutes=slot_minutes, input_minutes=input_minutes)))))
  availability <- matrix(data=0, nrow=length(data$Name), ncol=length(all_times))
  rownames(availability) <- data$Name
  colnames(availability) <- all_times
  for (person_index in seq_along(data$Name)) {
    person_times <- unique(c(unname(unlist(sapply(unname(sapply(unique(unlist(strsplit(data$Times[person_index], ", "))), times_format)), time_interpolate, slot_minutes=slot_minutes, input_minutes=input_minutes)))))
    for (time_index in seq_along(person_times)) {
      matching_time_index <- which(colnames(availability)==person_times[time_index])
      availability[person_index,  matching_time_index] <- 1
    }
  }
  return(availability)
}




#' Converts times
#'
#' Handles times like 8, 8 AM, 8 pm, 8:30 am, 8:30 PM and converts them all to the hour and minute, with the hour being a 24 hour clock. If you have a time like "2:00" it looks at the hour; if it's before assume_pm, it assumes you mean 2 pm, not 2 am
#' @param time The string of the time
#' @param assume_pm The latest hour at which you assume the time means PM if not otherwise specified
#' @return The hour:minute
#' @export
times_format <- function(time, assume_pm = 7) {
  segments <- unlist(strsplit(time, ":| ")[[1]])
  hour <- as.numeric(segments[1])
  minute <- 0
  if(grepl("m", segments[length(segments)], ignore.case=TRUE)) {
    if(length(segments)>2) {
      minute <- segments[2]
    }
    if(grepl("p", segments[length(segments)], ignore.case=TRUE)) {
	  if(hour!=12) {
        hour <- hour+12 # since PM
	  }
    }
  } else {
    if(length(segments)>1) {
      minute <- segments[2]
    }
    if(hour<=assume_pm) {
      hour <- hour+12
    }
  }
  minute <- as.numeric(minute)
  return(paste0(hour, ':', stringr::str_pad(minute, 2, pad="0")))
}

#' More finely divide a time slot
#'
#' If people give the hours available, and you want to split those up into 5 minute segments, this will do that.
#' @param time The string of the time
#' @param slot_minutes How long to make each slot
#' @param input_minutes Resolution of the input slots
#' @return The hour:minute
#' @export
time_interpolate <- function(time, slot_minutes=15, input_minutes=30) {
  interpolated_times <- time
  total_minutes <- slot_minutes
  raw_time <- as.numeric(strsplit(time, ":")[[1]])
  while(total_minutes < input_minutes) {
    hour <- raw_time[1]
    minute <- raw_time[2] + total_minutes
    hour <- hour + floor(minute/60)
    if(floor(minute/60)>0) {
      minute <- minute%%60 #since we absorbed the other minutes with the hours
    }
    interpolated_times <- c(interpolated_times, paste0(hour, ':', stringr::str_pad(minute, 2, pad="0")))
    total_minutes <- total_minutes + slot_minutes
  }
  return(interpolated_times)
}
