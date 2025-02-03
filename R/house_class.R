# Wrapper

new_house <- function(grid, outdoor_temperature, rooms, time, radiators_power,
                      doors, h, ht, alpha, change_radiators_state){
  structure(.Data = grid,
            outdoor_temperature = outdoor_temperature,
            rooms = rooms,
            time = time,
            radiators_power = radiators_power,
            doors = doors,
            h = h,
            ht = ht,
            alpha = alpha,
            change_radiators_state = change_radiators_state,
            calculated = FALSE)
}

# Validator

validate_new_house <- function(house){
  if(attr(house, "radiators_power") < 0)
    stop("Power < 0.")
  
  house
}

# Constructor

house <- function(coords, outdoor_temperature, rooms, min_time, max_time,
                  radiators_power, h, ht, alpha, change_radiators_state = NULL){
  grid <- list(x = seq(coords[["x"]][1], coords[["x"]][2], by = h),
               y = seq(coords[["y"]][1], coords[["y"]][2], by = h))
  
  time <- round(seq(min_time, max_time, by = ht), 2)
  
  rooms <- lapply(rooms, function(room){
    add_initial_temperature(room, outdoor_temperature[1], length(time))
  })
  
  doors <- list()
  
  if(length(rooms) > 1){
    for(room1_num in 1:(length(rooms)-1)){
      for(room2_num in (room1_num+1):length(rooms)){
        for(door1 in attr(rooms[[room1_num]], "doors_grids")){
          for(door2 in attr(rooms[[room2_num]], "doors_grids")){
            if(any(identical(round(door1[["x"]],2), round(door2[["x"]]+h,2)),
                   identical(round(door1[["x"]],2), round(door2[["x"]]-h,2))) &&
               identical(round(door1[["y"]],2), round(door2[["y"]],2))){
              doors <- append(doors, list(list(rooms = c(room1_num, room2_num),
                                          door = list(room_1 = door1,
                                                      room_2 = door2))))
            }else if(any(identical(round(door1[["y"]],2), round(door2[["y"]]+h,2)),
                         identical(round(door1[["y"]],2), round(door2[["y"]]-h,2))) &&
                     identical(round(door1[["x"]],2), round(door2[["x"]],2))){
              doors <- append(doors, list(list(rooms = c(room1_num, room2_num),
                                          door = list(room_1 = door1,
                                                      room_2 = door2))))
            }
          }
        }
      }
    }
  } else doors <- NULL
  
  validate_new_house(
    new_house(grid, outdoor_temperature, rooms, time, radiators_power, doors,
              h, ht, alpha, change_radiators_state)
  )
}

###########################

calculate_temperature_in_time <- function(house){
  if(attr(house, "calculated"))
    warning("Temperature was already calculated.")
  time <- attr(house, "time")
  rooms <- attr(house, "rooms")
  radiators_state <- attr(house, "change_radiators_state")
  ht <- attr(house, "ht")
  h <- attr(house, "h")
  alpha <- attr(house, "alpha")
  radiators_power <- attr(house, "radiators_power")
  doors <- attr(house, "doors")
  outdoor_temperature <- attr(house, "outdoor_temperature")
  rooms_temperatures <- lapply(rooms, function(room) attr(room, "temperature"))
  
  for(t in 2:length(time)){
    print(t)
    for(room_num in 1:length(rooms)){
      room <- rooms[[room_num]]
      dimension <- attr(room, "dimension")
      windows_grids <- attr(room, "windows_grids")
      radiator <- attr(room, "radiator_grid")
      
      if(!is.null(radiators_state) && (time[t] %in% radiators_state))
        attr(house, "rooms")[[room_num]] <- change_radiator_state(attr(house, "rooms")[[room_num]])
      
      for(x in 2:(dimension[["x"]] - 1)){
        for(y in 2:(dimension[["y"]] - 1)){
          rooms_temperatures[[room_num]][y,x,t] <-
            rooms_temperatures[[room_num]][y,x,t-1] + ht/h^2*alpha*
            (rooms_temperatures[[room_num]][y,x+1,t-1] +
              rooms_temperatures[[room_num]][y,x-1,t-1] +
              rooms_temperatures[[room_num]][y+1,x,t-1] +
              rooms_temperatures[[room_num]][y-1,x,t-1] -
             4*rooms_temperatures[[room_num]][y,x,t-1])
        }
      }
      rooms_temperatures[[room_num]][,1,t] <- rooms_temperatures[[room_num]][,2,t]
        
      rooms_temperatures[[room_num]][,dim(rooms_temperatures[[room_num]])[2],t] <-
        rooms_temperatures[[room_num]][,(dim(rooms_temperatures[[room_num]])[2] - 1),t]
        
      rooms_temperatures[[room_num]][1,,t] <-  rooms_temperatures[[room_num]][2,,t]
        
      rooms_temperatures[[room_num]][dim(rooms_temperatures[[room_num]])[1],,t] <-
        rooms_temperatures[[room_num]][(dim(rooms_temperatures[[room_num]])[1] - 1),,t]
        
      if(!is.null(windows_grids)){
        for(window in windows_grids){
          rooms_temperatures[[room_num]][round(nrow(rooms_temperatures[[room_num]])-
                                                 (window[["y"]]-min(room[["y"]]))/h),
                                         round((window[["x"]] - min(room[["x"]]))/h+1),
                                         t] <- outdoor_temperature[t] + 273.15
        }
      }
      
      if(!is.null(radiator))
        rooms_temperatures[[room_num]][,,t] <- rooms_temperatures[[room_num]][,,t] +
          ht*get_radiator_temperature_change(attr(house, "rooms")[[room_num]], t, radiators_power)
      
      attr(attr(house, "rooms")[[room_num]], "temperature")[,,t] <- rooms_temperatures[[room_num]][,,t]
    }
    
    if(!is.null(doors)){
      for(door in doors){
        door_rooms <- attr(house, "rooms")[door[["rooms"]]]
        room1_temperature <- attr(door_rooms[[1]], "temperature")[,,t]
        room2_temperature <- attr(door_rooms[[2]], "temperature")[,,t]
        room1_door <- door[["door"]][["room_1"]]
        room2_door <- door[["door"]][["room_2"]]
        room1_door_pos <- list(rows = nrow(room1_temperature) -
                                 round((room1_door[["y"]]-min(door_rooms[[1]][["y"]]))/h),
                               cols = round((room1_door[["x"]]-min(door_rooms[[1]][["x"]]))/h+1))
        room2_door_pos <- list(rows = nrow(room2_temperature) -
                                 round((room2_door[["y"]]-min(door_rooms[[2]][["y"]]))/h),
                               cols = round((room2_door[["x"]]-min(door_rooms[[2]][["x"]]))/h+1))
        
        room1_door_temperature <- room1_temperature[room1_door_pos[["rows"]], room1_door_pos[["cols"]]]
        
        room2_door_temperature <- room2_temperature[room2_door_pos[["rows"]], room2_door_pos[["cols"]]]
        
        door_temp <- mean(c(room1_door_temperature, room2_door_temperature))
        
        rooms_temperatures[[door[["rooms"]][1]]][room1_door_pos[["rows"]],
                                                 room1_door_pos[["cols"]],
                                                 t] <- door_temp
        
        rooms_temperatures[[door[["rooms"]][2]]][room2_door_pos[["rows"]],
                                                 room2_door_pos[["cols"]],
                                                 t] <- door_temp
        
        attr(attr(house, "rooms")[[door[["rooms"]][1]]], "temperature")[room1_door_pos[["rows"]],
                                                                        room1_door_pos[["cols"]],
                                                                        t] <- door_temp
        
        attr(attr(house, "rooms")[[door[["rooms"]][2]]], "temperature")[room2_door_pos[["rows"]],
                                                                        room2_door_pos[["cols"]],
                                                                        t] <- door_temp
      }
    }
  }
  
  for(room_num in 1:length(rooms)){
    attr(attr(house, "rooms")[[room_num]], "temperature") <-
      attr(attr(house, "rooms")[[room_num]], "temperature") - 273.15
  }
  
  attr(house, "calculated") <- TRUE
  
  house
}

calculate_energy_cons <- function(house){
  if(!attr(house, "calculated"))
    stop("Calculate temperature in time first")
  
  radiators_power <- attr(house, "radiators_power")
  change_state <- attr(house, "change_radiators_state")
  time <- attr(house, "time")
  
  if(!is.null(change_state)){
    t_open <- c()
    state <- TRUE
    for(t in time[2:length(time)]){
      if(t %in% change_state) state <- !state
      t_open <- c(t_open, state)
    }
  }else t_open <- NULL
  
  attr(house, "energy_consumption") <-
    sapply(attr(house, "rooms"), function(room){
      calculate_energy_cons_in_room(room, radiators_power, t_open)
    })
  
  colnames(attr(house, "energy_consumption")) <- paste0("room ", 1:length(attr(house, "rooms")))
  
  attr(house, "energy_consumption") <- cbind(attr(house, "energy_consumption"),
                                             time = attr(house, "time"))
  
  house
}
