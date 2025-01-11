# Wrapper

new_room <- function(grid, radiator_grid, radiator_power, windows_grids){
  structure(.Data = grid,
            radiator_grid,
            radiator_power,
            radiator_state = "open",
            windows_grids,
            windows_state = "closed")
}

# Validator

validate_new_room <- function(room){
  if(all(dim(attr(room, "radiator_grid")) != 1 ))
    stop("Radiator must be placed horizontaly or verticaly.")
  
  windows_error <- sapply(1:length(attr(room, "windows_grids")), function(i){
    if(all(dim(attr(room, "windows_grids")[[i]]) != 1))
      paste0("Window ", i, " must be placed horizontaly or verticaly.")
    else NULL
  })
  
  if(!is.null(windows_error))
    stop(paste0(windows_error, collapse = "\n"))
  
  if(!all(attr(room, "radiator_grid") %in% attr(room, "grid")))
    stop("Radiator is not inside room.")
  
  if(!all(unlist(attr(room, "windows_grids")) %in% attr(room, "grid")))
    stop("Windows are not inside the room.")
  
  if(attr(room, "power") < 0)
    stop("Power < 0.")
  
  radiator
}

# Constructor

room <- function(coords, radiator_coords, radiator_power, windows_coords, h){
  grid <- list(x = seq(coords[["x"]][1], coords[["x"]][2], by = h),
               y = seq(coords[["y"]][1], coords[["y"]][2], by = h))
  
  radiator_grid <- list(
    x = ifelse(length(radiator_coords[["x"]]) == 1,
               radiator_coords[["x"]],
               seq(radiator_coords[["x"]][1],
                   radiator_coords[["x"]][2], by = h)),
    y = ifelse(length(radiator_coords[["y"]]) == 1,
               radiator_coords[["y"]],
               seq(radiator_coords[["y"]][1],
                   radiator_coords[["y"]][2], by = h))
  )
  
  windows_grids <- sapply(windows_coords, function(coords){
      list(
        x = ifelse(length(coords[["x"]]) == 1,
                   coords[["x"]],
                   seq(coords[["x"]][1], coords[["x"]][2], by = h)),
        y = ifelse(length(coords[["y"]]) == 1,
                   coords[["y"]],
                   seq(coords[["y"]][1], coords[["y"]][2], by = h))
      )
    }, simplify = FALSE)
  
  validate_new_room(new_room(grid, radiator_grid, radiator_power, windows_grid))
}

###############################

move_radiator <- function(room, new_coords, h){
  new_grid <- list(
    x = ifelse(length(new_coords[["x"]]) == 1,
               new_coords[["x"]],
               seq(new_coords[["x"]][1],
                   new_coords[["x"]][2], by = h)),
    y = ifelse(length(new_coords[["y"]]) == 1,
               new_coords[["y"]],
               seq(new_coords[["y"]][1],
                   new_coords[["y"]][2], by = h))
  )
  
  if(identical(attr(room, "radiator_grid"), new_grid))
    message("Position of radiator didn't change.")
  
  room
}

change_radiator_state <- function(room){
  attr(room, "radiator_state") <- ifelse(attr(room, "radiator_state") == "open",
                                         "closed",
                                         "open")
  
  print(paste0("Radiator is now", attr(room, "radiator_state")))
  
  room
}

change_windows_state <- function(room){
  attr(room, "windows_state") <- ifelse(attr(room, "windows_state") == "open",
                                         "closed",
                                         "open")
  
  print(paste0("Windows are now", attr(room, "windows_state")))
  
  room
}

get_radiator_temperature_change <- function(room){
  if(is.null(attr(room, "temperature")))
    stop("First add temperature by creating home.")
  
  if(attr(room, "radiator_state") == "open"){
    area <- ifelse(
      length(attr(room, "radiator_grid")[["x"]]) == 1,
      max(attr(room, "radiator_grid")[["y"]]) - 
        min(attr(room, "radiator_grid")[["y"]]),
      max(attr(room, "radiator_grid")[["x"]]) - 
        min(attr(room, "radiator_grid")[["x"]])
    )
    
    density <- 1013.25/(attr(room, "temperature")*287.05)
    
    radiator_fun <- function(x, y, room, area, density){
      if(x %in% attr(room, "radiator_grid")[["x"]] &
         y %in% attr(room, "radiator_grid")[["y"]])
        attr(room, "radiator_power")/(area*density*1.005)
      else 0
    }
    
    outer(x, y, Vectorize(radiator_fun), room = room, area = area,
          density = density)
  } else matrix(0, nrow = length(attr(room, "grid")[["y"]]),
                ncol = length(attr(room, "grid")[["x"]]))
}
