require(ggplot2)
require(plotly)
require(dplyr)

source("R/room_class.R")
source("R/house_class.R")

first_room <- room(coords = list(x = c(0.1, 5), y = c(5.1, 10)),
                   thermostat = 22,
                   h = 0.1,
                   radiator_coords = list(x = c(2.3, 2.7), y = c(9.8, 9.9)),
                   windows_coords = list(list(x = c(0.7, 2), y = 10),
                                         list(x = c(3, 4.3), y = 10)),
                   doors_coords = list(list(x = c(4.1, 4.8), y = 5.1)))

second_room <- room(coords = list(x = c(5.1, 8), y = c(4.1, 10)),
                   thermostat = 20,
                   h = 0.1,
                   radiator_coords = list(x = c(7.5, 7.9), y = c(9.8, 9.9)),
                   windows_coords = list(list(x = c(5.6, 6.5), y = 10),
                                         list(x = c(7.1, 7.8), y = 10)),
                   doors_coords = list(list(x = 5.1, y = c(4.2, 4.9))))

third_room <- room(coords = list(x = c(5.1, 8), y = c(2.1, 4)),
                   thermostat = 20,
                   h = 0.1,
                   radiator_coords = list(x = c(5.4, 5.8), y = c(3.8, 3.9)),
                   windows_coords = list(list(x = 8, y = c(2.8, 3.6))),
                   doors_coords = list(list(x = 5.1, y = c(2.4, 3.1))))

fourth_room <- room(coords = list(x = c(0.1, 3), y = c(3.1, 5)),
                   thermostat = 18,
                   h = 0.1,
                   radiator_coords = list(x = c(0.2, 0.3), y = c(3.8, 4.2)),
                   doors_coords = list(list(x = 3, y = c(3.6, 4.3))))

fifth_room <- room(coords = list(x = c(3.1, 5), y = c(0.1, 5)),
                    h = 0.1,
                    thermostat = NULL,
                    doors_coords = list(list(x = c(4.1, 4.8), y = 5),
                                        list(x = 5, y = c(4.2, 4.9)),
                                        list(x = 5, y = c(2.4, 3.1)),
                                        list(x = 3.1, y = c(3.6, 4.3))))

first_house <- house(coords = list(x = c(0.1, 8), y = c(0.1, 10)),
                   outdoor_temperature = rep(0, each = 50*24+1),
                   rooms = list(first_room, second_room, third_room,
                                fourth_room, fifth_room),
                   min_time = 0,
                   max_time = 24,
                   radiators_power = 684,
                   h = 0.1,
                   ht = 0.02,
                   alpha = 0.07704,
                   change_radiators_state = c(7, 17))

second_house <- house(coords = list(x = c(0.1, 8), y = c(0.1, 10)),
                     outdoor_temperature = rep(8, each = 50*24+1),
                     rooms = list(first_room, second_room, third_room,
                                  fourth_room, fifth_room),
                     min_time = 0,
                     max_time = 24,
                     radiators_power = 684,
                     h = 0.1,
                     ht = 0.02,
                     alpha = 0.07704,
                     change_radiators_state = c(7, 17))

third_house <- house(coords = list(x = c(0.1, 8), y = c(0.1, 10)),
                     outdoor_temperature = rep(0, each = 50*24+1),
                     rooms = list(first_room, second_room, third_room,
                                  fourth_room, fifth_room),
                     min_time = 0,
                     max_time = 24,
                     radiators_power = 684,
                     h = 0.1,
                     ht = 0.02,
                     alpha = 0.07704)

fourth_house <- house(coords = list(x = c(0.1, 8), y = c(0.1, 10)),
                     outdoor_temperature = rep(8, each = 50*24+1),
                     rooms = list(first_room, second_room, third_room,
                                  fourth_room, fifth_room),
                     min_time = 0,
                     max_time = 24,
                     radiators_power = 684,
                     h = 0.1,
                     ht = 0.02,
                     alpha = 0.07704)

fifth_house <- house(coords = list(x = c(0.1, 8), y = c(0.1, 10)),
                      outdoor_temperature = rep(15, each = 50*24+1),
                      rooms = list(first_room, second_room, third_room,
                                   fourth_room, fifth_room),
                      min_time = 0,
                      max_time = 24,
                      radiators_power = 684,
                      h = 0.1,
                      ht = 0.02,
                      alpha = 0.07704,
                      change_radiators_state = c(7, 17))


sixth_house <- house(coords = list(x = c(0.1, 8), y = c(0.1, 10)),
                      outdoor_temperature = rep(15, each = 50*24+1),
                      rooms = list(first_room, second_room, third_room,
                                   fourth_room, fifth_room),
                      min_time = 0,
                      max_time = 24,
                      radiators_power = 684,
                      h = 0.1,
                      ht = 0.02,
                      alpha = 0.07704)

first_house_temp <- calculate_temperature_in_time(first_house)
second_house_temp <- calculate_temperature_in_time(second_house)
third_house_temp <- calculate_temperature_in_time(third_house)
fourth_house_temp <- calculate_temperature_in_time(fourth_house)
fifth_house_temp <- calculate_temperature_in_time(fifth_house)
sixth_house_temp <- calculate_temperature_in_time(sixth_house)

create_animated_heatmap <- function(house, step = 10, min_temp = 0, max_temp = 30){
  temperatures <- lapply(attr(house, "rooms"), function(room){
    as.vector(attr(room, "temperature")[,,seq(1, length(attr(house, "time")), by = step)])
  })
  
  heat_len <- (length(attr(house, "time"))-1)/step+1
  
  frames <- data.frame(
    z = unlist(temperatures),
    time = unlist(lapply(temperatures, function(x){
      rep(attr(house, "time")[seq(1, length(attr(house, "time")), by = step)],
          each = length(x)/heat_len1)})),
    field = unlist(sapply(1:length(temperatures), function(x){
      rep(paste0("Field ", x), length(temperatures[[x]]))}))
  )
  
  grid_expanded <- lapply(attr(house, "rooms"), function(room){
    expand.grid(y = room[["y"]][length(room[["y"]]):1],
                x = room[["x"]])
  })
  
  frames[["x"]] <- unlist(sapply(grid_expanded, function(expand){
    rep(expand[["x"]], heat_len)
  }))
  frames[["y"]] <- unlist(sapply(grid_expanded, function(expand){
    rep(expand[["y"]], heat_len)
  }))
  
  frames <- arrange(frames, time)
  
  plot_ly(
    data = frames,
    x = ~x, y = ~y, z = ~z, frame = ~time,
    type = "heatmap", coloraxis = "coloraxis") %>%
    layout( xaxis = list(title = "x"), yaxis = list(title = "y")) %>%
    animation_opts(
      frame = 0.01,
      transition = 0,
      easing = "linear",
      mode = "immediate") %>%
    layout(
      coloraxis = list(
        colorscale = "Viridis",
        cmin = min_temp,
        cmax = max_temp
      )
    )
}

create_animated_heatmap(first_house_temp)
create_animated_heatmap(second_house_temp)
create_animated_heatmap(third_house_temp)
create_animated_heatmap(fourth_house_temp)
create_animated_heatmap(fifth_house_temp)
create_animated_heatmap(sixth_house_temp)

houses <- list(first_house_temp, second_house_temp, third_house_temp,
               fourth_house_temp, fifth_house_temp, sixth_house_temp)

houses <- lapply(houses, function(h) calculate_energy_cons(h))

cum_energy <- lapply(houses, function(h){
  data.frame(energy = rowSums(attr(h, "energy_consumption")),
             time = attr(h, "time"),
             case = paste0(attr(h, "outdoor_temperature"),
                           ifelse(is.null(attr(h, "change_radiators_state")),
                                  "", ", closed")))
})
cum_energy <- rbind(cum_energy[[1]],cum_energy[[2]],cum_energy[[3]],
                    cum_energy[[4]],cum_energy[[5]],cum_energy[[6]])

ggplot(cum_energy, aes(x = as.numeric(time), y = as.numeric(energy),
                       color = case,
                       group = case)) +
  geom_line() +
  labs(x = "time", y = "energy") +
  theme_light()

save.image("data/problem2.RData")
