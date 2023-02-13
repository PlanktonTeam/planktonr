# Some code to create a hex sticker for the IMOS BOO project
#
# Last updated: Monday 13th February 2023
#
# Jason D. Everett (UQ/CSIRO/UNSW)
#
# For installation instructions see here:
# https://github.com/GuangchuangYu/hexSticker

# devtools::install_github("GuangchuangYu/hexSticker")


hexSticker::sticker(file.path("data-raw", "Hex", "copepod2_rotate22.svg"),
                    package = "planktonr",
                    p_y = 0.95,
                    p_color = "white",
                    p_size = 90,
                    s_x = 0.81,
                    s_y = 1.05,
                    s_width = 0.75,
                    h_fill = "#54bceb", # "#3B6E8F",
                    h_color = "white",
                    url = "https://PlanktonTeam.github.io/planktonr/",
                    u_color = "white",
                    u_family = "sans",
                    u_size = 13,
                    u_x = 1.02,
                    u_y = 0.07,
                    dpi = 1000,
                    asp = 1,
                    filename = file.path("man", "figures", "planktonr.png"))



# hexSticker::sticker(file.path("data-raw", "Hex", "copepod2_rotate22.svg"),
#                     package="planktonr",
#                     p_y = 0.95,
#                     p_color = "white",
#                     p_size = 8,
#                     s_x=0.81,
#                     s_y=1.05,
#                     s_width=0.75,
#                     # s_height=2.5,
#                     h_fill = "#54bceb", # "#3B6E8F",
#                     h_color = "white",
#                     url = "https://github.com/PlanktonTeam/planktonr",
#                     u_color = "white",
#                     u_family = "sans",
#                     u_size = 1.2,
#                     dpi = 1000,
#                     asp = 1,
#                     filename="planktonr_Hex.png")


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


