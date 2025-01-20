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
#' @param add_gap If TRUE, add a gap between meetings of one slot if possible
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_fill_random <- function(possible_availability, guests, desired_length=60, slot_length=15, earliest_possible=TRUE, host_rooms=c(), allow_shorter_meetings=FALSE, add_gap=TRUE) {
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
	  trailing_time <- c()
	  best_trailing_time <- c()
      for (slot_index in seq_along(potential_times)) {
        local_solution <- potential_times[slot_index]
		try({trailing_time <- potential_times[slot_index+1]}, silent=TRUE)
        last_time <- local_solution
        for (additional_offset in sequence(slots_required-1)) {
          additional_index <- slot_index + additional_offset
          if(additional_index<=length(potential_times)) {
            #print((c(local_solution[additional_index-1], additional_index-1, local_solution)))
            next_time <- potential_times[additional_index]
            if(time_difference(c(last_time, next_time))==slot_length) { # tests to make sure they're adjacent
              local_solution <- c(local_solution, next_time)
			  try({trailing_time <- potential_times[additional_index+1]}, silent=TRUE)

              last_time <- next_time
            } else {
              break
            }
          }
        }
        if(earliest_possible) {
          if(length(local_solution)>length(best_solution)) {
            best_solution <- local_solution
			best_trailing_time <- trailing_time
          }
        } else {
          if(length(local_solution)>=length(best_solution)) {
            best_solution <- local_solution
			best_trailing_time <- trailing_time
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
				if(add_gap) {
					try({
						if(max(possible_availability[host_local, , best_trailing_time])<2) {
							possible_availability[host_local, , best_trailing_time] <- 0
						#	print("added trailing host")
						}
						if(max(possible_availability[, guest_local, best_trailing_time])<2) {
							possible_availability[, guest_local, best_trailing_time] <- 0
						#	print("added trailing guest")
						}
					}, silent=TRUE)	
				}
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
#' @param add_gap If TRUE, add a gap between meetings of one slot if possible
#' @return An array in same format as possible_availability, but with 2 for the assigned slots, 0 for the unavailable slots, and 1 for available but still unfilled.
#' @export
availability_fill_greedy <- function(possible_availability, guests, desired_length=60, slot_length=15, earliest_possible=TRUE, host_rooms=c(), max_iterations=1000, add_gap=TRUE) {
	best_score <- 0
	best_availability <- possible_availability
	for(iteration in sequence(max_iterations)) {
		local_availability <- availability_fill_random(possible_availability, guests, desired_length=desired_length, slot_length=slot_length, earliest_possible=earliest_possible, host_rooms=host_rooms, add_gap=add_gap)
		
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
	simple_schedule$Person[nchar(simple_schedule$Person)==0] <- "Break"
	rows_to_remove <- c()
	for (row_index in nrow(simple_schedule):2) {
		if(simple_schedule$Person[row_index]==simple_schedule$Person[row_index-1]) {
			simple_schedule$Duration[row_index-1] <- simple_schedule$Duration[row_index-1] + simple_schedule$Duration[row_index]
			rows_to_remove <- c(rows_to_remove, row_index)
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

#' Fill gaps
#' @param availability_array 3d array of availability with entries 0, 1, and 2
#' @param too_long The maximum number of slots a gap should be
#' @param desired_length The amount of time to require in a slot
#' @param slot_length The amount of time each slot represents
#' @param earliest_possible If TRUE, tries to do this meeting as early in the day as it can; if FALSE, as late
#' @param host_rooms The vector of host rooms: room is entry, host name is names
#' @param excluded_hosts A vector of hosts who shouldn't be scheduled
#' @param included_hosts A vector of hosts who should be scheduled. Only include one of these two.
#' @param allow_shorter_meetings If TRUE, allow meetings shorter than desired_length
#' @param add_gap If TRUE, add a gap between meetings of one slot if possible
#' @return a 3d array of availability with entries 0, 1, and 2
#' @export
fill_gaps <- function(availability_array, too_long=4, desired_length=60, slot_length=15, earliest_possible=TRUE, host_rooms, excluded_hosts=c(), included_hosts=c(), allow_shorter_meetings=FALSE, max_tries=1000, add_gap=FALSE) {
	attempt <- 0
	possible_hosts <- dimnames(availability_array)$host
	if(length(excluded_hosts)>0) {
		possible_hosts <- possible_hosts[!possible_hosts %in% excluded_hosts]
	}
	if(length(included_hosts)>0) {
		possible_hosts <- included_hosts
	}
	while(max(get_gaps_per_guest(availability_array))>too_long & attempt < max_tries) {
		gaps_by_people <- get_gaps_per_guest(availability_array)
		focal_person <- sample(names(gaps_by_people)[which(gaps_by_people>too_long)], size=1)
		guest_df = data.frame(Name=focal_person, Desired=sample(possible_hosts, size=1))
		availability_array <- availability_fill_random(availability_array, guests=guest_df, desired_length=desired_length, slot_length=slot_length, earliest_possible=earliest_possible, host_rooms=host_rooms, allow_shorter_meetings=allow_shorter_meetings, add_gap=add_gap)
		attempt <- attempt + 1
	}

  	return(availability_array)	
}


#' Get the maximum number of adjacent slots a guest is available for
#' 
#' This ignores things like lunch (i.e., if there is a gap before lunch and two after, it will return 3)
#' @param availibility_array 3d array of availability with entries 0, 1, and 2
#' @param guest_name The name of the guest
#' @return The maximum number of adjacent slots a guest is available for
#' @export
get_max_gap_per_guest <- function(availibility_array, guest_name) {
	guest_availability <- availibility_array[, guest_name, ]
	meetings_by_slot <- apply(guest_availability,2,max)
	y <- rle(meetings_by_slot) #uses strategy from https://stackoverflow.com/questions/28731582/maximum-consecutive-repeats-in-a-vector-in-r
	return(max(y$lengths[y$values==1]))
}

get_max_fill_per_guest <- function(availibility_array, guest_name) {
	guest_availability <- availibility_array[, guest_name, ]
	meetings_by_slot <- apply(guest_availability,2,max)
	y <- rle(meetings_by_slot) #uses strategy from https://stackoverflow.com/questions/28731582/maximum-consecutive-repeats-in-a-vector-in-r
	return(max(y$lengths[y$values==2]))
}

#' Get the maximum number of adjacent slots any guest is available for
#' 
#' This ignores things like lunch (i.e., if there is a gap before lunch and two after, it will return 3)
#' @param availibility_array 3d array of availability with entries 0, 1, and 2
#' @return Vector of the maximum number of adjacent slots each guest is available for
#' @export
get_gaps_per_guest <- function(availibility_array) {
	gap_count <- rep(0, length(dimnames(availibility_array)$guest))
	for (guest_index in seq_along(dimnames(availibility_array)$guest)) {
		gap_count[guest_index] <- get_max_gap_per_guest(availibility_array, dimnames(availibility_array)$guest[guest_index])
  	}
	names(gap_count) <- dimnames(availibility_array)$guest
  	return(gap_count)	
}

#' Get the maximum number of adjacent slots a guest has filled
#' 
#' This ignores things like lunch (i.e., if there is a gap before lunch and two after, it will return 3)
#' @param availibility_array 3d array of availability with entries 0, 1, and 2
#' @return The maximum number of adjacent slots a guest has filled
#' @export
get_fills_per_guest <- function(availibility_array) {
	fill_count <- rep(0, length(dimnames(availibility_array)$guest))
	for (guest_index in seq_along(dimnames(availibility_array)$guest)) {
		fill_count[guest_index] <- get_max_fill_per_guest(availibility_array, dimnames(availibility_array)$guest[guest_index])
  	}
	names(fill_count) <- dimnames(availibility_array)$guest
  	return(fill_count)	
}

#' Get the maximum number of adjacent slots a guest has filled
#'
#' This ignores things like lunch (i.e., if there is a gap before lunch and two after, it will return 3)
#' @param availibility_array 3d array of availability with entries 0, 1, and 2
#' @return The maximum number of adjacent slots a guest has filled
#' @export
get_fills_per_host <- function(availibility_array) {
	fill_count <- rep(0, length(dimnames(availibility_array)$host))
	for (host_index in seq_along(dimnames(availibility_array)$host)) {
		fill_count[host_index] <- get_max_fill_per_guest(availibility_array, dimnames(availibility_array)$host[host_index])
  	}
	names(fill_count) <- dimnames(availibility_array)$host
  	return(fill_count)	
}