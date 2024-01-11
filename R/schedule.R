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

#' Block undesired pairings
#'
#' If there are particular combinations of hosts and guest who shouldn't meet, this can block those slots. With the block_guests and block_hosts, it's possible to have a set of them: block_guests = c("Bob, Mary", "Pat") and block_hosts = c("Joe", "Jim, Amy"): so that Bob and Mary won't meet with Joe, and
#' @param possible_availability 3D array with dimensions host, guest, times
#' @param block_guests Vector of guests who shouldn't meet particular hosts.
#' @param block_hosts Vector of hosts who shouldn't meet particular guests.
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_block <- function(possible_availability, block_guests, block_hosts) {
  #print(dim(possible_availability))
  if(length(block_guests) != length(block_hosts)) {
    stop("The length of block_guests and block_hosts are not the same")
  }
  for (i in seq_along(block_guests)) {
    all_guests <- strsplit(block_guests[i], ", ")[[1]]
    all_hosts <- strsplit(block_hosts[i], ", ")[[1]]
    for (host_index in seq_along(all_hosts)) {
      for (guest_index in seq_along(all_guests)) {
        possible_availability[all_hosts[host_index], all_guests[guest_index], ] <-0
      }
    }

  }
  #print(dim(possible_availability))
  return(possible_availability)
}


#' Fill in meetings
#'
#' This will take an availability array and info on guest priorities and fill in the essential meetings. It will make sure the slots are contiguous in time (so if there's a lunch break, it won't schedule a meeting across it). If you include a vector of rooms, where the vector if alphabetized puts nearby rooms together ("Smith 101", "Smith 201", "Adams 404") it will try to schedule appointments so that appointments are scheduled in nearby rooms.
#' @param possible_availability 3D array with dimensions host, guest, times
#' @param guests The data.frame of guest info, including a column of Name of the guest and Desired the hosts to meet
#' @param desired_length The amount of time to require in a slot
#' @param slot_length The amount of time each slot represents
#' @param earliest_possible If TRUE, tries to do this meeting as early in the day as it can; if FALSE, as late
#' @param host_rooms The vector of host rooms: room is entry, host name is names
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_fill <- function(possible_availability, guests, desired_length=60, slot_length=15, earliest_possible=TRUE, host_rooms=c()) {
  slots_required <- ceiling(desired_length/slot_length)
  guest_names <- sample(as.character(guests$Name), size=nrow(guests), replace=FALSE) #pull guests in random order
  for (guest_index in seq_along(guest_names)) {
    guest_vector <- guests[which(guests$Name==guest_names[guest_index]),]
    guest_local <- as.character(guest_vector$Name)
    desired_hosts <- strsplit(guest_vector$Desired, ', ')[[1]]
    if(length(host_rooms)>1) {
      desired_host_vector <- sort(host_rooms[desired_hosts], decreasing=(runif(1)<0.5)) #so we get neighboring rooms if possible
      desired_hosts <- names(desired_host_vector)
    }
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
          #print(paste("host", host_local, "guest", guest_local))
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

#' Fill in meetings randomly
#'
#' This will take an availability array and info on guest priorities and fill in the essential meetings. It will make sure the slots are contiguous in time (so if there's a lunch break, it won't schedule a meeting across it). If you include a vector of rooms, where the vector if alphabetized puts nearby rooms together ("Smith 101", "Smith 201", "Adams 404") it will try to schedule appointments so that appointments are scheduled in nearby rooms. It will randomly do pairs of guests and hosts
#' @param possible_availability 3D array with dimensions host, guest, times
#' @param guests The data.frame of guest info, including a column of Name of the guest and Desired the hosts to meet
#' @param desired_length The amount of time to require in a slot
#' @param slot_length The amount of time each slot represents
#' @param earliest_possible If TRUE, tries to do this meeting as early in the day as it can; if FALSE, as late
#' @param host_rooms The vector of host rooms: room is entry, host name is names
#' @param allow_shorter_meetings If TRUE, allow meetings shorter than desired_length
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_fill_random <- function(possible_availability, guests, desired_length=60, slot_length=15, earliest_possible=TRUE, host_rooms=c(), allow_shorter_meetings=FALSE) {
  slots_required <- ceiling(desired_length/slot_length)
  guest_names <- sample(as.character(guests$Name), size=nrow(guests), replace=FALSE) #pull guests in random order

  guests_by_hosts <- data.frame()
  for (guest_index in seq_along(guest_names)) {
    guest_vector <- guests[which(guests$Name==guest_names[guest_index]),]
    guest_local <- as.character(guest_vector$Name)
    desired_hosts <- strsplit(guest_vector$Desired, ', ')[[1]]
    if(length(desired_hosts)>0) {
      guests_by_hosts <- rbind(guests_by_hosts, data.frame(guest=guest_local, host=desired_hosts, stringsAsFactors=FALSE))
    }
  }
  guests_by_hosts <- guests_by_hosts[sample.int(nrow(guests_by_hosts), replace=FALSE),]
  for (row_index in sequence(nrow(guests_by_hosts))) {
    host_local <- guests_by_hosts$host[row_index]
    guest_local <- guests_by_hosts$guest[row_index]
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
        #print(paste("host", host_local, "guest", guest_local))
        possible_availability[host_local, guest_local, ] <- 0
		if(length(best_solution)==slots_required | allow_shorter_meetings) { #don't want too short meetings
			for (solution_index in seq_along(best_solution)) {
				possible_availability[host_local, , best_solution[solution_index]] <- 0
				possible_availability[, guest_local, best_solution[solution_index]] <- 0
				possible_availability[host_local, guest_local, best_solution[solution_index]] <-2
			}
		}
      }

    } else {
      warning(paste0("Guest ", guest_local, " wants to meet unavailable ", host_local))
    }
  }
  return(possible_availability)
}


#' Fill in meetings with a greedy algorithm
#'
#' This will take an availability array and info on guest priorities and fill in the essential meetings. It will make sure the slots are contiguous in time (so if there's a lunch break, it won't schedule a meeting across it). If you include a vector of rooms, where the vector if alphabetized puts nearby rooms together ("Smith 101", "Smith 201", "Adams 404") it will try to schedule appointments so that appointments are scheduled in nearby rooms. It will randomly do pairs of guests and hosts. Then it will repeat this, trying to maximize the number of meetings scheduled.
#' @param possible_availability 3D array with dimensions host, guest, times
#' @param guests The data.frame of guest info, including a column of Name of the guest and Desired the hosts to meet
#' @param desired_length The amount of time to require in a slot
#' @param slot_length The amount of time each slot represents
#' @param earliest_possible If TRUE, tries to do this meeting as early in the day as it can; if FALSE, as late
#' @param host_rooms The vector of host rooms: room is entry, host name is names
#' @param max_iterations The maximum number of iterations to try to schedule
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_fill_greedy <- function(possible_availability, guests, desired_length=60, slot_length=15, earliest_possible=TRUE, host_rooms=c(), max_iterations=100) {
	best_score <- 0
	best_availability <- possible_availability
	for(iteration in sequence(max_iterations)) {
		local_availability <- availability_fill_random(possible_availability, guests, desired_length=desired_length, slot_length=slot_length, earliest_possible=earliest_possible, host_rooms=host_rooms)
		
		local_score <- sum(local_availability==2) - var(count_unique_meetings(local_availability, is_host=FALSE))
		print(paste("Iteration", iteration, "score", local_score))
		if(local_score>best_score) {
			best_score <- local_score
			best_availability <- local_availability
		}
	}	
	return(best_availability)
}


#' Convert schedule to easier information for a person
#' @param person The person to make the schedule for
#' @param availability_array 3d array of availability with entries 0, 1, and 2
#' @param is_host If TRUE, do for host; otherwise, do guest
#' @param host_rooms The vector of host rooms: room is entry, host name is names
#' @return A data.frame of times, the person being met, and the room
#' @export
schedule_summary <- function(person, availability_array, is_host=TRUE, host_rooms=NULL) {
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
        if (length(matching_people)>0 & !is.null(host_rooms)) {
          simple_schedule$Room[col_index] <- host_rooms[person]
        } else {
          simple_schedule$Room[col_index] <- ""
        }
      } else {
        if (length(matching_people)>0 & !is.null(host_rooms)) {
          simple_schedule$Room[col_index] <- host_rooms[matching_people[1]]
        } else {
          simple_schedule$Room[col_index] <- ""
        }
      }
  }
  return(simple_schedule)
}

#' Convert schedule to easier information for a person including durations
#' @param simple_schedule A data.frame of times, the person being met, and the room
#' @param input_minutes The amount of time each slot represents
#' @return A data.frame of times, the person being met, the room, and the duration
#' @export
schedule_durations <- function(simple_schedule, input_minutes=20) {
	simple_schedule$Duration <- input_minutes
	rows_to_remove <- c()
	for (row_index in 2:nrow(simple_schedule)) {
		if(simple_schedule$Person[row_index]==simple_schedule$Person[row_index-1]) {
			rows_to_remove <- c(rows_to_remove, row_index-1)
			simple_schedule$Duration[row_index] <- simple_schedule$Duration[row_index] + simple_schedule$Duration[row_index-1]
		}
	}
	simple_schedule <- simple_schedule[-rows_to_remove,]
	simple_schedule$Duration <- paste0(simple_schedule$Duration, " min")
	return(simple_schedule)
}

#' Flatten everyone's schedules to one data.frame
#' @param availability_array 3d array of availability with entries 0, 1, and 2
#' @param is_host If TRUE, do for host; otherwise, do guest
#' @return a data.frame of times and who each person (host or guest) is meeting with at each time
#' @export
schedule_flatten <- function(availability_array, is_host=TRUE) {
  people <- c()
  if(is_host) {
    people <- dimnames(availability_array)$host
  } else {
    people <- dimnames(availability_array)$guest
  }
  people <- sort(people)
  schedule_flat <- data.frame(Times=dimnames(availability_array)$times, stringsAsFactors=FALSE)
  for (i in seq_along(people)) {
    local_schedule <- schedule_summary(person=people[i], availability_array=availability_array, is_host=is_host)
    if(max(nchar(local_schedule$Person))>0) { #So, toss a person if they have no meetings
      schedule_flat$NewPerson <- local_schedule$Person
      colnames(schedule_flat)[ncol(schedule_flat)] <- people[i]
    }
  }
  return(schedule_flat)
}

#' Counts per person how many unique people they meet
#' @param availability_array 3d array of availability with entries 0, 1, and 2
#' @param is_host If TRUE, do for host; otherwise, do guest
#' @return a vector of the number of unique people each person meets
#' @export
count_unique_meetings <- function(availability_array, is_host=TRUE) {
	summarized_schedule <- schedule_flatten(availability_array, is_host=is_host)
	summarized_schedule <- summarized_schedule[, -1] #remove the times column
	length_unique <- function(x) {
		x <- x[nchar(x)>0] #remove empty entries
		return(length(unique(x)))	
	}
	meeting_counts <- apply(summarized_schedule, 2, length_unique)
	return(meeting_counts)
}