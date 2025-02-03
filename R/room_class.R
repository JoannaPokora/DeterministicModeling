# Wrapper

new_room <- function(grid, dimension, radiator_grid, thermostat,
                     windows_grids, doors_grids, h){
  structure(.Data = grid,
            dimension = dimension,
            radiator_grid = radiator_grid,
            thermostat = thermostat,
            radiator_state = "open",
            windows_grids = windows_grids,
            doors_grids = doors_grids,
            h = h)
}

# Validator

validate_new_room <- function(room){
  if(!is.null(attr(room, "windows_grids"))){
    for(i in 1:length(attr(room, "windows_grids"))){
      if(length(attr(room, "windows_grids")[[i]][["x"]]) != 1 &&
         length(attr(room, "windows_grids")[[i]][["y"]]) != 1)
        stop(paste0("Window ", i, " must be placed horizontaly or verticaly."))
    }
  }
  
  if(!is.null(attr(room, "radiator_grid"))){
    if(!all(attr(room, "radiator_grid")[["x"]] %in% room[["x"]]) ||
       !all(attr(room, "radiator_grid")[["y"]] %in% room[["y"]]))
      stop("Radiator is not inside the room.")
  }
  
  if(!all(unlist(sapply(attr(room, "windows_grids"), function(x) x[["x"]])) %in% room[["x"]]) ||
     !all(unlist(sapply(attr(room, "windows_grids"), function(x) x[["y"]])) %in% room[["y"]]))
    stop("Windows are not inside the room.")
  
  if(!is.null(attr(room, "thermostat")) && !(attr(room, "thermostat") %in% 15:24))
    stop("Thermostat must be set to temperature between 15°C and 24°C.")
  
  if(!is.null(attr(room, "doors_grids"))){
    for(i in 1:length(attr(room, "doors_grids"))){
      if(length(attr(room, "doors_grids")[[i]][["x"]]) != 1 &&
         length(attr(room, "doors_grids")[[i]][["y"]]) != 1)
        stop(paste0("Door ", i, " must be placed horizontaly or verticaly."))
    }
  }
  
  room
}

# Constructor

room <- function(coords, thermostat, h, radiator_coords = NULL,
                 windows_coords = NULL, doors_coords = NULL){
  grid <- list(x = round(seq(coords[["x"]][1], coords[["x"]][2], by = h), 3),
               y = round(seq(coords[["y"]][1], coords[["y"]][2], by = h), 3))
  
  dimension <- c(x = length(grid[["x"]]), y = length(grid[["y"]]))
  
  if(!is.null(radiator_coords))
    radiator_grid <- list(
      x = round(seq(radiator_coords[["x"]][1],
                    radiator_coords[["x"]][min(length(radiator_coords[["x"]]),2)],
                    by = h), 3),
      y = round(seq(radiator_coords[["y"]][1],
                    radiator_coords[["y"]][min(length(radiator_coords[["y"]]),2)],
                    by = h), 3)
    )
  else radiator_grid <- NULL
  
  if(!is.null(windows_coords))
    windows_grids <- lapply(windows_coords, function(coords){
        list(x = round(seq(coords[["x"]][1],
                           coords[["x"]][min(length(coords[["x"]]),2)],
                           by = h), 3),
             y = round(seq(coords[["y"]][1],
                           coords[["y"]][min(length(coords[["y"]]),2)],
                           by = h), 3))
      })
  else windows_grids <- NULL
  
  if(!is.null(doors_coords))
    doors_grids <- lapply(doors_coords, function(coords){
      list(x = round(seq(coords[["x"]][1],
                         coords[["x"]][min(length(coords[["x"]]),2)],
                         by = h), 2),
           y = round(seq(coords[["y"]][1],
                         coords[["y"]][min(length(coords[["y"]]),2)],
                         by = h), 2))
    })
  else doors_grids <- NULL
  
  validate_new_room(
    new_room(grid, dimension, radiator_grid, thermostat, windows_grids, doors_grids, h)
  )
}

###############################

change_radiator_state <- function(room){
  attr(room, "radiator_state") <- ifelse(attr(room, "radiator_state") == "open",
                                         "closed",
                                         "open")
  
  print(paste0("Radiator is now ", attr(room, "radiator_state")))
  
  room
}

add_initial_temperature <- function(room, outdoor_temperature, time_length){
  attr(room, "temperature") <-
    array(dim = c(attr(room, "dimension")[2], attr(room, "dimension")[1], time_length))
  
  attr(room, "temperature")[,,1] <- outdoor_temperature[1] + 273.15
  
  room
}

get_radiator_temperature_change <- function(room, step, power){
  if(is.null(attr(room, "temperature")))
    stop("First add room to a house.")
  
  if(attr(room, "radiator_state") == "open" &&
     mean(attr(room, "temperature")[,,step - 1]) < (attr(room, "thermostat") + 273.15)){
    rad_x <- attr(room, "radiator_grid")[["x"]]
    rad_y <- attr(room, "radiator_grid")[["y"]]
    
    area <- (max(rad_x)-min(rad_x))*(max(rad_y)-min(rad_y))
    
    radiator_fun <- function(y, x, area){
      radiator_x <- rad_x
      radiator_y <- rad_y
      
      if(all(c(any(sapply(radiator_x, function(rx) x == rx)),
               any(sapply(radiator_y, function(ry) y == ry)))))
        3600*power/(area * 1.205 * 1005)
      else 0
    }
    
    outer(room[["y"]][length(room[["y"]]):1], room[["x"]], Vectorize(radiator_fun), area = area)
  }else matrix(0, nrow = length(room[["y"]]),
               ncol = length(room[["x"]]))
}

calculate_energy_cons_in_room <- function(room, power, t_open){
  temperature <- attr(room, "temperature")
  
  if(is.null(attr(room, "radiator_grid"))){
    rep(0, dim(temperature)[3])
  }else{
    energy <- sapply(2:dim(temperature)[3], function(i){
      if(mean(temperature[,,i-1]) <= attr(room, "thermostat"))
        3600*power/(1.205 * 1005)
      else 0
    })
    
    if(!is.null(t_open)) energy[!t_open] <- 0
    
    energy <- c(0, energy)
    cumsum(energy)
  }
}
