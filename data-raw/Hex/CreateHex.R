# Some code to create a hex sticker for the IMOS BOO project
#
# Last updated: Monday 3rd July 2023
#
# Jason D. Everett (UQ/CSIRO/UNSW)
#
# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")


hexSticker::sticker(file.path("data-raw", "Hex", "MultiPlanktonImage.png"),
  package = "planktonr",
  p_y = 1,
  p_color = "grey90",
  p_size = 90,
  s_x = 1.0,
  s_y = 1.0,
  s_width = 0.9,
  h_fill = "#3B6E8F", # "#40B4ED",# "#54bceb", #
  h_color = "#213f52", # "grey90",
  url = "planktonteam.github.io/planktonr",
  u_color = "grey90",
  # u_family = "sans",
  u_size = 16.5,
  u_x = 0.98,
  u_y = 0.07,
  dpi = 1000,
  asp = 1,
  filename = file.path("man", "figures", "planktonr.png")
)



# sticker("IMOS_Crop.png",
#         package="",
#         p_size=5,
#         p_y = 1.7,
#         s_x=1,
#         s_y=1,
#         s_width=0.92,
#         s_height=0.4,
#         h_fill = "#3B6E8F",
#         h_color = "white",
#         url = "www.imos.org.au",
#         u_color = "white",
#         u_family = "sans",
#         dpi = 800,
#         asp = 1,
#         filename="IMOS_Hex.png")
