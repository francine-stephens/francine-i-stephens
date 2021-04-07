---
# A Demo section created with the Blank widget.
# Any elements can be added in the body: https://sourcethemes.com/academic/docs/writing-markdown-latex/
# Add more sections by duplicating this file and customizing to your requirements.
widget: "blank"  # See https://sourcethemes.com/academic/docs/page-builder/
headless: true  # This file represents a page section.
active: true # Activate this widget? true/false
weight: 10  # Order that this section will appear.

title: About
subtitle: ''

design:
  # Choose how many columns the section has. Valid values: 1 or 2.
  columns: 1

design.background:
  # Apply a background color, gradient, or image.
  #   Uncomment (by removing `#`) an option to apply it.
  #   Choose a light or dark text color by setting `text_color_light`.
  #   Any HTML color name or Hex value is valid.

  # Background color.
  # color = "navy"
  
  # Background gradient.
  # gradient_start = "DeepSkyBlue"
  # gradient_end = "SkyBlue"
  
  # Background image.
  image: ""  # Name of image in `static/img/`.
  image_darken: 0.6  # Darken the image? Range 0-1 where 0 is transparent and 1 is opaque.

  # Text color (true=light or false=dark).
  text_color_light: false

design.spacing:
  # Customize the section spacing. Order is top, right, bottom, left.
  padding: ["20px", "0", "20px", "0"]

advanced:
 # Custom CSS. 
 css_style: ""
 
 # CSS class.
 css_class: "mini"

---






```r
us_states <- ggplot2::map_data("state")
residence <- tribble(
  ~city,           ~state,  ~lat,   ~long, ~years, ~description,
  "San Antonio", "TX", 29.3,  -99.0,  5,     "Early Childhood",
  "New Braunfels", "TX", 29.8,  -98.3,  13,   "K-12 Schooling",
  "Philadelphia",  "PA", 39.95281,  -75.19321,  4, "Undergrad at UPenn",
  "Stanford", "CA", 37.42816, -122.16976,   3.75, "Grad school at Stanford",
  "New Braunfels", "TX",  29.8,  -98.3,   0.5, "Covid-19 Remote School<br>2020 Election Work"
) 

residence_connections_prelim <- residence %>% 
  mutate(
    # need this to create transition state ----
    city_order = row_number() + 1,
    # where I moved to next, for curved arrows ----
    lat_next = lead(lat),
    long_next = lead(long),
    # label to show in plot, styled using ggtext ---
    label = glue::glue("**{city}, {state}** ({years} yrs)<br>*{description}*"),
    # label of next location ----
    label_next = lead(label)
  ) 

residence_connections <- residence_connections_prelim %>%
  # get first row of residence ----
  slice(1) %>% 
  # manually modify for plotting ----
  mutate(
    city_order = 1,
    label_next = label,
    lat_next = lat,
    long_next = long,
    ) %>% 
  # combine with all other residences ----
  bind_rows(residence_connections_prelim) %>% 
  # last row irrelevant ----
  slice(1:5) %>% 
  # keep what we neeed ----
  dplyr::select(city_order, lat, long, lat_next, long_next, label_next)


base_map <- ggplot() +
  # plot states ----
  geom_polygon(
    data = us_states,
    aes(
      x     = long, 
      y     = lat, 
      group = group
      ),
    fill  = "#F2F2F2",
    color = "white"
  ) +
  # lines for pins ----
  geom_segment(
    data = residence,
    aes(
      x    = long,
      xend = long,
      y    = lat,
      yend = lat + 0.5
      ),
    color = "#181818",
    size = 0.3
    ) +
    # pin heads, a bit above actual location, color with R ladies lighter purple ----
  geom_point(
    data = residence,
    aes(
      x = long, 
      y = lat + 0.5
      ),
    size = 0.5,
    color = "#88398A"
  ) +
  theme_void()


anim <- base_map +
  # show arrows connecting residences ----
  geom_curve(
    # do not include 1st residence in arrows as no arrow is intended ----
    # and inclusion messes up transition ---
    data = residence_connections %>% slice(-1),
    # add slight adjustment to arrow positioning ----
    aes(
      y     = lat - 0.1,
      x     = long,
      yend  = lat_next - 0.2,
      xend  = long_next,
      # group is used to create the transition ----
      group = seq_along(city_order)
    ),
    color = "#181818",
    curvature = 0.5,
    arrow = arrow(length = unit(0.02, "npc")),
    size  = 0.2
  ) +
  # add in labels for pins, with inward positioning ----
  # show labels either top left or top right of pin ----
  geom_richtext(
    data = residence_connections,
    aes(
      x     = ifelse(long_next < -100, long_next + 1, long_next - 1),
      y     = lat_next + 5,
      label = label_next,
      vjust = "top",
      hjust = ifelse(long_next < -100, 0, 1),
      # group is used to create the transition ----
      group = seq_along(city_order)
    ),
    size = 2,
    label.colour = "white",
    # R ladies purple ----
    color = "#562457",
    # R ladies font used in xaringan theme Lato ----
    family = "Candara Light"
  ) +
  # title determined by group value in transition ----
  ggtitle("Home {closest_state} of 6") +
  # create animation ----
  transition_states(
    city_order,
    transition_length = 2,
    state_length = 5
    ) +
  # style title ----
  theme(
    plot.title = element_text(
      color = "#562457",
      family = "Candara",
      size = 12
      )
    )
```




![my homes](homes_animation.gif)

This gif tells you a little about me, both professionally and personally. I love to make maps and visuals! Hence, I created the gif to portray my biography geographically and visually. As you can see, I am originally from south-central Texas. I was born in San Antonio, Texas and grew up just up to road in a bedroom community called New Braunfels. I left home for college to attend the [University of Pennsylvania](https://www.upenn.edu/), where I received my BA in Sociology with a concentration in Quantitative methods. I then moved to the west coast to study for a PhD in Sociology at Stanford. 


My research focuses on understanding how demographic changes and environmental changes are related. 

Between my own research and some time spent working for the Texas Democratic Party during the 2020 election cycle, I became an avid user of the statistical programming language R. I developed a passion for programming and data visualization. I try to bring these passions to my research as well as the courses I teach.


