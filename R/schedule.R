#' Create array of hosts, guests, and possible times
#'
#' This will take an availability array of hosts and guests and delete any times that aren't possible for any host-guest pair
#' @param host_availability Availability matrix for hosts
#' @param guest_availability Availability matrix for guests
#' @return 3D array with dimensions host, guest, times
#' @export
possible_availability_create <- function(host_availability, guest_availability) {
  times <- intersect(colnames(host_availability), colnames(guest_availability))
  possible_availability <- array(0, dim=c(nrow(host_availability), nrow(guest_availability), length(times)), dimnames=list(host=rownames(host_availability), guest=rownames(guest_availability), times=times))
  for (host_index in seq_along(rownames(host_availability))) {
    for (guest_index in  seq_along(rownames(guest_availability))) {
      for (time_index in seq_along(times)) {
        host_local <- rownames(host_availability)[host_index]
        guest_local <- rownames(guest_availability)[guest_index]
        time_local <- times[time_index]
        if(host_availability[host_local, time_local] == 1 & guest_availability[guest_local, time_local] == 1) {
          possible_availability[host_local, guest_local, time_local] <- 1
        }

      }
    }
  }
  return(possible_availability)
}

#' Compute the minutes between two times in HH:MM format
#'
#' @param times The vector the two times
#' @return The difference in minutes
time_difference <- function(times) {
  time_1 <- as.numeric(strsplit(times[1],":")[[1]])
  time_2 <- as.numeric(strsplit(times[2],":")[[1]])
  return(abs( (time_1[1]*60+time_1[2]) - (time_2[1]*60+time_2[2]) ))
}



#' Fill in meetings
#'
#' This will take an availability array and info on guest priorities and fill in the essential meetings. It will make sure the slots are continuous.
#' @param possible_availability 3D array with dimensions host, guest, times
#' @param guests The data.frame of guest info, including a column of Name of the guest and Desired the hosts to meet
#' @param desired_length The amount of time to require in a slot
#' @param slot_length The amount of time each slot represents
#' @param earliest_possible If TRUE, tries to do this meeting as early in the day as it can; if FALSE, as late
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_fill <- function(possible_availability, guests, desired_length=60, slot_length=15, earliest_possible=TRUE) {
  slots_required <- ceiling(desired_length/slot_length)
  guest_names <- sample(as.character(guests$Name), size=nrow(guests), replace=FALSE) #pull guests in random order
  for (guest_index in seq_along(guest_names)) {
    guest_vector <- guests[which(guests$Name==guest_names[guest_index]),]
    guest_local <- as.character(guest_vector$Name)
    desired_hosts <- strsplit(guest_vector$Desired, ', ')[[1]]
    for (desired_index in seq_along(desired_hosts)) {
      host_local <- as.character(desired_hosts[desired_index])
      #print(c(host_local=host_local, guest_local=guest_local))
      if(host_local %in% dimnames(possible_availability)$host) {
        #print(dim(possible_availability))
        local_possibility <- possible_availability[host_local,guest_local,]
        potential_times <- names(local_possibility[which(local_possibility==1)])
        best_solution <- c()
        for (slot_index in seq_along(potential_times)) {
          local_solution <- potential_times[slot_index]
          last_time <- local_solution
          for (additional_offset in sequence(slots_required-1)) {
            additional_index <- slot_index + additional_offset
            if(additional_index<=length(potential_times)) {
              #print((c(local_solution[additional_index-1], additional_index-1, local_solution)))
              next_time <- potential_times[additional_index]
              if(time_difference(c(last_time, next_time))==slot_length) { # tests to make sure they're adjacent
                local_solution <- c(local_solution, next_time)
                last_time <- next_time
              } else {
                break
              }
            }
          }
          if(earliest_possible) {
            if(length(local_solution)>length(best_solution)) {
              best_solution <- local_solution
            }
          } else {
            if(length(local_solution)>=length(best_solution)) {
              best_solution <- local_solution
            }
          }
        }
        if(length(best_solution)>0) {
          # So for this pair, there is only one set of slots that will work: the one we assign them to (state 2)
          possible_availability[host_local, guest_local, ] <- 0
          for (solution_index in seq_along(best_solution)) {
            possible_availability[host_local, , best_solution[solution_index]] <- 0
            possible_availability[, guest_local, best_solution[solution_index]] <- 0
            possible_availability[host_local, guest_local, best_solution[solution_index]] <-2
          }
        }

      } else {
        warning(paste0("Guest ", guest_local, " wants to meet unavailable ", host_local))
      }
    }
  }
  return(possible_availability)
}

#' Convert schedule to easier information
#' @param availability_array 3d array of availability with entries 0, 1, and 2
#' @param person The person to make the schedule for
#' @param is_host If TRUE, do for host; otherwise, do guest
#' @param host_rooms The vector of host rooms
#' @return A data.frame of times, the person being met, and the room
#' @export
schedule_summary <- function(availability_array, person, is_host=TRUE, host_rooms) {
  local_schedule <- data.frame()
  if(is_host) {
     local_schedule <- availability_array[person, ,]
   } else {
     local_schedule <- availability_array[, person,]
   }
  simple_schedule <- data.frame(Times=colnames(local_schedule), Person="", Room="", stringsAsFactors=FALSE)
  for (col_index in sequence(ncol(local_schedule))) {
      matching_people <- names(which(local_schedule[,col_index]==2))
      simple_schedule$Person[col_index] <- paste0(matching_people, collapse=", ")
      if(is_host) {
        if (length(matching_people)>0) {
          simple_schedule$Room[col_index] <- host_rooms[person]
        } else {
          simple_schedule$Room[col_index] <- ""
        }
      } else {
        if (length(matching_people)>0) {
          simple_schedule$Room[col_index] <- host_rooms[matching_people[1]]
        } else {
          simple_schedule$Room[col_index] <- ""
        }
      }
  }
  return(simple_schedule)
}
