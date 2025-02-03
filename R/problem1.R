require(ggplot2)
require(plotly)
require(dplyr)

source("R/room_class.R")
source("R/house_class.R")

temperatures <- read.csv("data/data.csv", sep = ";")
temperatures <- temperatures[1:169,]
temperatures[["time"]] <- 0:(nrow(temperatures)-1)

ggplot(temperatures, aes(x = time, y = averageAirTemp)) +
  geom_line() +
  labs(y = "temperature") +
  theme_light()

first_room <- room(coords = list(x = c(0.1, 2.5), y = c(0.1, 4)),
                   thermostat = 22,
                   h = 0.1,
                   radiator_coords = list(x = c(1.9, 2.3), y = c(3.8, 3.9)),
                   windows_coords = list(list(x = c(0.5, 1), y = 4),
                                         list(x = c(1.3, 1.8), y = 4),
                                         list(x = c(2.1, 2.3), y = 4)))

new_house <- house(coords = list(x = c(0.1, 2.5), y = c(0.1, 4)),
                   outdoor_temperature = rep(temperatures[["averageAirTemp"]],
                                             each = 50),
                   rooms = list(first_room),
                   min_time = 0,
                   max_time = 168,
                   radiators_power = 684,
                   h = 0.1,
                   ht = 0.02,
                   alpha = 0.07704)

house_with_temp <- calculate_temperature_in_time(new_house)

room_with_temp <- attr(house_with_temp, "rooms")[[1]]

frames <- data.frame(z = as.vector(attr(room_with_temp, "temperature")
                                   [,,seq(1, length(attr(new_house, "time")), by = 25)]),
                     time = rep(seq(1, nrow(temperatures), by = 0.5),
                                each = dim(attr(room_with_temp, "temperature"))[1]*
                                  dim(attr(room_with_temp, "temperature"))[2]))
grid_expanded <- expand.grid(y = room_with_temp[["y"]][length(room_with_temp[["y"]]):1],
                             x = room_with_temp[["x"]])
frames[["x"]] <- grid_expanded[["x"]]
frames[["y"]] <- grid_expanded[["y"]]

animated_heatmap <- plot_ly(
  data = frames,
  x = ~x, y = ~y, z = ~z, frame = ~time, type = "heatmap",
  coloraxis = "coloraxis") %>%
  layout( xaxis = list(title = "x"),
          yaxis = list(title = "y"),
          coloraxis = list(
            colorscale = "Viridis",
            cmin = 8,
            cmax = 30)) %>%
  animation_opts(
    frame = 0.01,
    transition = 0,
    easing = "linear",
    mode = "immediate")

animated_heatmap

second_room <- room(coords = list(x = c(0.1, 2.5), y = c(0.1, 4)),
                    thermostat = 22,
                    h = 0.1,
                    radiator_coords = list(x = c(0.4, 0.8), y = c(0.2, 0.3)),
                    windows_coords = list(list(x = c(0.5, 1), y = 4),
                                          list(x = c(1.3, 1.8), y = 4),
                                          list(x = c(2.1, 2.3), y = 4)))

second_house <- house(coords = list(x = c(0.1, 2.5), y = c(0.1, 4)),
                      outdoor_temperature = rep(temperatures[["averageAirTemp"]],
                                                each = 50),
                      rooms = list(second_room),
                      min_time = 0,
                      max_time = 168,
                      radiators_power = 684,
                      h = 0.1,
                      ht = 0.02,
                      alpha = 0.07704)

second_house_with_temp <- calculate_temperature_in_time(second_house)

second_room_with_temp <- attr(second_house_with_temp, "rooms")[[1]]

second_frames <- data.frame(z = as.vector(attr(second_room_with_temp, "temperature")
                                   [,,seq(1, length(attr(second_house, "time")), by = 25)]),
                     time = rep(seq(1, nrow(temperatures), by = 0.5),
                                each = dim(attr(room_with_temp, "temperature"))[1]*
                                  dim(attr(room_with_temp, "temperature"))[2]))
second_grid_expanded <- expand.grid(y = second_room_with_temp[["y"]][length(second_room_with_temp[["y"]]):1],
                             x = second_room_with_temp[["x"]])
second_frames[["x"]] <- second_grid_expanded[["x"]]
second_frames[["y"]] <- second_grid_expanded[["y"]]

sec_animated_heatmap <- plot_ly(
  data = second_frames,
  x = ~x, y = ~y, z = ~z, frame = ~time, type = "heatmap",
  coloraxis = "coloraxis") %>%
  layout( xaxis = list(title = "x"),
          yaxis = list(title = "y"),
          coloraxis = list(
            colorscale = "Viridis",
            cmin = 8,
            cmax = 30)) %>%
  animation_opts(
    frame = 0.01,
    transition = 0,
    easing = "linear",
    mode = "immediate")

sec_animated_heatmap

house_with_temp <- calculate_energy_cons(house_with_temp)
second_house_with_temp <- calculate_energy_cons(second_house_with_temp)

cum_energy <- rbind(attr(house_with_temp, "energy_consumption"),
                    attr(second_house_with_temp, "energy_consumption"))
colnames(cum_energy)[1] <- "energy"
cum_energy <- cbind(cum_energy,
                `Umiejscowienie kaloryfera` =
                  rep(c("pod oknami", "na przeciwko okien"), each = nrow(cum_energy)/2))

ggplot(cum_energy, aes(x = as.numeric(time), y = as.numeric(energy),
                       color = `Umiejscowienie kaloryfera`,
                       group = `Umiejscowienie kaloryfera`)) +
  geom_line() +
  labs(x = "czas", y = "energia") +
  theme_light()

save.image("data/problem1.RData")
