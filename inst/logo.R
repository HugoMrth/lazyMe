library(hexSticker)


#### Pour enlever le fond d'une image
# https://www.remove.bg/fr


# Create the sticker
sticker(
  subplot = "inst/image.png",
  filename = "inst/logo.png",  # Output file name
  #img = "C:/Users/MARTHINETEX/Documents/02 - Code/logo.png",     # Path to your background image
  package = "lazy",                 # Package name
  p_size = 32,
  p_y = 0.75,
  p_x = 1.45,# Text size
  s_x = 0.78,                            # X position of the image
  s_y = 1.15,                         # Y position of the image
  s_width = 1.2,                      # Width of the image
  h_fill = "#FFFFFF",                 # Background color
  h_color = "#242625",                # Hex border color
  p_color = "#242625",                # Package name color
  layout = "straight",                # Text layout
  text_y = 1.2                       # Y position of the text
)
