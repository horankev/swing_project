
###############################################################################
##### Packages
###############################################################################



# install.packages("devtools")
# devtools::install_github("m-clark/gammit")

# required packages
packages <- c(
  "tidyverse", # family of packages
  "sf", # for managing spatial data
  "here", # file management
  "spdep", # areal data
  "kableExtra", # making tables
  "cartogram", # making cartograms
  "parlitools", # data source
  "mgcv", # for GAMs
  "ggforce", # for geom_circle function
  "ggpubr", # arranging multiple plots
  "patchwork",# arranging multiple plots
  "ggsflabel", # has repel labels for sf
  "ggnewscale", # for multiple scales in one ggplot
  "broom", # make tidy tibbles from model objects
  "latex2exp", # for latex in ggplot subtitles
  "PieGlyph", # making swingometers
  "broom.mixed", # extract tidy output
  "merTools", # with plot functions
  "gammit" # for pulling out random and fixed effects
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE, quietly = TRUE))

# to over-ride conflict with `MASS` package
select <- dplyr::select



###############################################################################
##### Data preparation
###############################################################################



# prepare the data
# extract and join census and election data from parlitools package
census_11 <- parlitools::census_11 |> 
  select(-constituency_name,-constituency_type,-pano, -region, -country)
bes_2019 <- parlitools::bes_2019

elect_results <- left_join(bes_2019,census_11, by=c("ons_const_id"))

# get constituency and higher level geometry data
uk_map_download <- st_read(
  "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/WPC_Dec_2019_UGCB_UK_2022/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson",
  quiet = TRUE)

# only need the boundaries and the IDs for merging with parlitools data
uk<- uk_map_download |> 
  select(pcon19cd,geometry) |> 
  st_transform(crs=27700) |> 
  st_make_valid() # ensure valid line overlaps etc

# create dataframe for filtering-out speaker constituencies below
speakers <- data.frame(
  year = c(2017,2019),
  constituency_name = c("Buckingham","Chorley")
)

# join constituency polygons to parlitools data
# filter out Scotland and Northern Ireland
# filter out the speakers' constituencies
# calculate the desired explanatory variables
# make levels into factors for use with `mgcv`

df <- elect_results |> 
  left_join(uk, by=c("ons_const_id"="pcon19cd")) |> 
  filter(!country %in% c("Scotland","Northern Ireland")) |> 
  filter(!constituency_name %in% speakers$constituency_name) |>
  mutate(degree_educated = qual_level_4,
         health_not_good = health_fair + health_bad + health_very_bad,
         white = ethnicity_white,
         con_change = con_19 - con_17, # difference in %
         lab_change = lab_19 - lab_17,
         con_swing = (con_change - lab_change)/2, # Butler swing
         region = factor(ifelse(county == "Merseyside", "Merseyside", region)),
         county = factor(county),
         constituency_name = factor(constituency_name)) |> 
  st_as_sf()

# create region boundaries layer for mapping
regions <- df |> group_by(region) |> 
  summarise() |> 
  st_as_sf() |> 
  st_transform(crs=27700)

# create county boundaries layer for mapping
counties <- df |> group_by(county) |> 
  summarise() |> 
  st_as_sf() |> 
  st_transform(crs=27700)

# create outline layer for mapping
uk_outline <- df |> 
  summarise() |> 
  st_as_sf() |> 
  st_transform(crs=27700)

# filter out required independent variables and scale them
# then add back to the features which were not scaled
df_scaled <- df |> 
  st_drop_geometry() |> 
  select(degree_educated,
         health_not_good,
         white) |> 
  scale() |> 
  as.data.frame() |> 
  mutate(con_swing = df$con_swing,
         population = df$population,
         region = factor(df$region),
         county = factor(df$county),
         constituency_name = factor(df$constituency_name))

# create a simple features (spatial) version of this with a geometry column
df_scaled_sf <- df_scaled |> 
  mutate(geometry = df$geometry) |> 
  st_as_sf() |> 
  st_transform(crs=27700)



###############################################################################
##### Create cartograms
###############################################################################



# create cartograms sized by population
# by constituency

df_temp <- elect_results |> 
  left_join(uk, by=c("ons_const_id"="pcon19cd")) |> 
  filter(!country %in% c("Scotland","Northern Ireland")) |> 
  mutate(region = factor(ifelse(county == "Merseyside", "Merseyside", region))) |> 
  st_as_sf()

# construct the contiguities with the speaker's seats still present, then remove them
const_contig_full <- cartogram_cont(df_temp |> st_simplify(),weight="population",itermax = 5) |> 
  st_simplify()

speaker_carto <- const_contig_full |> 
  filter(constituency_name %in% speakers$constituency_name)

const_contig <- const_contig_full |> 
  filter(!constituency_name %in% speakers$constituency_name) |> 
  mutate(degree_educated = df$degree_educated,
         health_not_good = df$health_not_good,
         white = df$white,
         con_swing = df$con_swing)

# by region
carto_region <- const_contig_full |> 
  group_by(region) |> 
  summarise()

# by county
carto_county <- const_contig_full |> 
  group_by(county) |> 
  summarise()

# an outline map
carto_outline <- const_contig_full |> 
  summarise()



###############################################################################
##### Data visualisation
###############################################################################



#######
### Dependent variable and guide map
#######

# make maps for the variables using these cartograms

int1 <- ggplot() +
  geom_sf(data=const_contig,aes(fill=degree_educated, colour=degree_educated), linewidth=0) +
  geom_sf(data=carto_region, fill=NA, linewidth=0.2, colour="black") + 
  # geom_sf(data=carto_outline, fill=NA, linewidth=0.5, colour="black") + 
  geom_sf(data=speaker_carto, fill="black", colour="black") + 
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "BrBG", direction = 1) +
  scale_colour_distiller(palette = "BrBG", direction = 1) +
  labs(fill="Degree\neducated\n(%)") +
  theme_bw() +
  guides(colour="none") +
  theme(legend.position = c(0.16, 0.7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))

int2 <- ggplot() +
  geom_sf(data=const_contig,aes(fill=health_not_good, colour=health_not_good), linewidth=0) +
  geom_sf(data=carto_region, fill=NA, linewidth=0.2, colour="black") + 
  # geom_sf(data=carto_outline, fill=NA, linewidth=0.5, colour="black") +
  geom_sf(data=speaker_carto, fill="black", colour="black") + 
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "BrBG", direction = 1) +
  scale_colour_distiller(palette = "BrBG", direction = 1) +
  labs(fill="Health\nnot good\n(%)") +
  theme_bw() +
  guides(colour="none") +
  theme(legend.position = c(0.16, 0.7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))

int3 <- ggplot() +
  geom_sf(data=const_contig,aes(fill=white, colour=white), linewidth=0) +
  geom_sf(data=carto_region, fill=NA, linewidth=0.2, colour="black") + 
  # geom_sf(data=carto_outline, fill=NA, linewidth=0.5, colour="black") +
  geom_sf(data=speaker_carto, fill="black", colour="black") + 
  coord_sf(datum = NA) +
  scale_fill_distiller(palette = "BrBG", direction = 1) +
  scale_colour_distiller(palette = "BrBG", direction = 1) +
  labs(fill="White\n(%)") +
  theme_bw() +
  guides(colour="none") +
  theme(legend.position = c(0.16, 0.7),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))

guide_map <- ggplot() +
  # geom_sf(data=carto_outline, fill=NA, linewidth=0.5, colour="black") +
  geom_sf(data=carto_region, aes(fill=region), linewidth=0.25, colour="black", alpha=0.5) + 
  geom_sf_label_repel(data=carto_region, aes(label=region), 
                      fill="white", size=1.5, colour="black") +
  scale_fill_brewer(palette = "Set3") + 
  coord_sf(datum = NA) +
  labs(subtitle = "Regions guide") + 
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank()) + 
  guides(colour="none",
         fill="none") + 
  theme(panel.background = element_rect(fill = "ivory2", color = "black"),
        plot.subtitle = element_text(hjust = 0.5))

group1 <- ggarrange(int1,int2,int3, nrow = 1, ncol = 3)

int5 <- ggplot() +
  geom_sf(data=const_contig,aes(fill=con_swing, colour=con_swing), linewidth=0) +
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0) +
  scale_colour_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0) +
  geom_sf(data=carto_region, fill=NA, linewidth=0.25, colour="black") +
  geom_sf(data=speaker_carto, fill="black", colour="black") + 
  coord_sf(datum = NA) +
  labs(fill="Conservative\nswing (%)") +
  theme_bw() +
  theme(legend.position = c(0.18, 0.77),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black")) +
  guides(colour = "none")

group2 <- ggarrange(int5,guide_map, nrow = 1, ncol = 2, widths = c(1.3,1))

group2



###############################################################################
##### Swingometers
###############################################################################



# Graphical representation of swing by region and county. 
# Swing is scaled relative to circle such that the maximum swing is 90 degrees from the vertical. 
# In this regional map, the North East swing of 8.36% is the maximum.

# to create a swing-ometer
# calculate the swing by region
# first sum up the total votes of each party by region
# and find the total electorate for each region

#######
### Region swingometer
#######

temp_pie_region <- df |> 
  group_by(region) |>
  summarise(con_vote_19=sum(con_vote_19, na.rm = TRUE),
            lab_vote_19=sum(lab_vote_19, na.rm = TRUE),
            con_vote_17=sum(con_vote_17, na.rm = TRUE),
            lab_vote_17=sum(lab_vote_17, na.rm = TRUE),
            total_vote_19=sum(total_vote_19),
            total_vote_17=sum(total_vote_17))

# find centroids of regions where the swing-ometer will be centred
temp_centroids <- st_centroid(regions$geometry) |> 
  st_coordinates() |> 
  as.data.frame() |> 
  mutate(region = regions$region)

# move london centroid to the east, for visual purposes
temp_centroids[temp_centroids$region=="London",]$X <- temp_centroids[temp_centroids$region=="London",]$X + 40000
# move merseyside centroid to the south, for visual purposes
temp_centroids[temp_centroids$region=="Merseyside",]$Y <- temp_centroids[temp_centroids$region=="Merseyside",]$Y - 20000

# calculations for region swings
# also shows how steed swing could be calculated
temp_pie_region1 <- temp_pie_region |> 
  select(region,con_vote_19,con_vote_17,lab_vote_19,lab_vote_17,total_vote_19,total_vote_17) |> 
  mutate(
    # for steed swing
    # tot_19 = con_vote_19 + lab_vote_19,
    # tot_17 = con_vote_17 + lab_vote_17,
    tot_19 = total_vote_19,
    tot_17 = total_vote_17,
    c19 = con_vote_19/tot_19,
    c17 = con_vote_17/tot_17,
    l19 = lab_vote_19/tot_19,
    l17 = lab_vote_17/tot_17,
    swing = ((c19 - c17) - (l19 - l17))/2)

# calculate the overall swing
temp_tot_19 <- sum(df$total_vote_19, na.rm = TRUE)
temp_tot_17 <- sum(df$total_vote_17, na.rm = TRUE)
temp_tot_c19 <- sum(df$con_vote_19, na.rm = TRUE)/temp_tot_19
temp_tot_c17 <- sum(df$con_vote_17, na.rm = TRUE)/temp_tot_17
temp_tot_l19 <- sum(df$lab_vote_19, na.rm = TRUE)/temp_tot_19
temp_tot_l17 <- sum(df$lab_vote_17, na.rm = TRUE)/temp_tot_17
temp_tot_swing <- ((temp_tot_c19 - temp_tot_c17) - (temp_tot_l19 - temp_tot_l17))/2

# duplicate the North East row, which is nearest to the desired position on the top left
# change its name to Overall
temp_pie_region1[12,] <- temp_pie_region1[temp_pie_region1$region=="North East",]
temp_pie_region1[12,]$swing <- temp_tot_swing
temp_pie_region1$region <- as.character(temp_pie_region1$region)
temp_pie_region1[12,]$region <- "Overall"

# same with the centroids dataframe
temp_centroids[12,] <- temp_centroids[temp_centroids$region=="North East",]
temp_centroids$region <- as.character(temp_centroids$region)
temp_centroids[12,]$region <- "Overall"

# move Overall centroid (North East duplicate) to west and north (for top left position)
temp_centroids[temp_centroids$region=="Overall",]$X <- 150000
temp_centroids[temp_centroids$region=="Overall",]$Y <- 620000

# calculate the angles the swing-ometer should make
temp_pie_region2 <- temp_pie_region1 |> 
  mutate(mag_swing = swing * (pi/(2*max(temp_pie_region1$swing))), # make max swing be pi/2
         X = temp_centroids$X,
         Y = temp_centroids$Y,
         theta = mag_swing,
         xend = X - 48000*sin(theta),
         yend = Y - 48000*cos(theta),
         prop1 = pi,
         prop2 = theta,
         prop3 = pi-theta,
         region_abb = c("E","EM","L","M","NE","NW","SE","SW","W","WM","YH","Overall"))

# plot the region swing-ometer
region_smeter <- ggplot() + 
  geom_sf(data=regions, fill="white", linetype="dashed") + 
  geom_sf(data=uk_outline, fill=NA, colour="black", linewidth=0.3) + 
  geom_pie_glyph(data=temp_pie_region2,aes(x=X,y=Y), slices=c("prop1","prop2","prop3"),
                 radius=0.5, colour="black") +
  scale_fill_manual(values = c("gray90","#A1B1F9","gray90")) +
  geom_segment(data=temp_pie_region2,aes(x=X,y=Y,xend=xend,yend=yend), 
               arrow = arrow(length = unit(0.1, "inches")), colour="black", linewidth=1) + 
  geom_label(data=temp_pie_region2, 
             aes(label=paste0(region_abb,": ",round(swing*100,2),"%"), y=Y+25000, x=X), 
             size=2, colour="black", fontface="bold", fill="khaki") + 
  geom_circle(data=temp_pie_region2,aes(x0=X,y0=Y,r=3000), fill="black") + 
  coord_sf(datum=NA) + 
  guides(fill="none") + 
  labs(title = "swingometer",
       subtitle = "by region") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) + 
  theme(panel.background = element_rect(fill = "ivory2", color = "black"))

#######
### County swingometer
#######

# same process as above, but for county swingometer
temp_pie_county <- df |> group_by(county) |>
  summarise(con_vote_19=sum(con_vote_19, na.rm = TRUE),
            lab_vote_19=sum(lab_vote_19, na.rm = TRUE),
            lab_vote_17=sum(lab_vote_17, na.rm = TRUE),
            con_vote_17=sum(con_vote_17, na.rm = TRUE),
            total_vote_19=sum(total_vote_19),
            total_vote_17=sum(total_vote_17))

temp_centroids_county <- st_centroid(counties$geometry) |> 
  st_coordinates() |> 
  as.data.frame() |> 
  mutate(county = counties$county)

temp_pie_county1 <- temp_pie_county |> 
  select(county,con_vote_19,con_vote_17,lab_vote_19,lab_vote_17,total_vote_19,total_vote_17) |> 
  mutate(
    # for steed swing
    # tot_19 = con_19 + lab_19,
    # tot_17 = con_17 + lab_17,
    tot_19 = total_vote_19,
    tot_17 = total_vote_17,
    c19 = con_vote_19/tot_19,
    c17 = con_vote_17/tot_17,
    l19 = lab_vote_19/tot_19,
    l17 = lab_vote_17/tot_17,
    swing = ((c19 - c17) - (l19 - l17))/2)

temp_pie_county2 <- temp_pie_county1 |> 
  mutate(mag_swing = temp_pie_county1$swing * (pi/(2*max(temp_pie_county1$swing))), # make max swing be pi/2
         X = temp_centroids_county$X,
         Y = temp_centroids_county$Y,
         theta = mag_swing,
         xend = X - 25000*sin(theta),
         yend = Y - 25000*cos(theta),
         prop1 = pi,
         prop2 = theta,
         prop3 = pi-theta)

county_smeter <- ggplot() + 
  geom_sf(data=counties, fill="white", linetype="dashed") + 
  geom_sf(data=uk_outline, fill=NA, colour="black", linewidth=0.3) + 
  geom_pie_glyph(data=temp_pie_county2,aes(x=X,y=Y), slices=c("prop1","prop2","prop3"),
                 radius=0.2, colour="black") +
  scale_fill_manual(values = c("gray90","#A1B1F9","gray90")) +
  geom_segment(data=temp_pie_county2,aes(x=X,y=Y,xend=xend,yend=yend), 
               arrow = arrow(length = unit(0.1, "inches")), linewidth=0.75, colour="black") + 
  geom_circle(data=temp_pie_county2,aes(x0=X,y0=Y,r=2000), fill="black") + 
  # add in the Overall layers from row 12 of the temp_pie_region dataframe
  geom_pie_glyph(data=temp_pie_region2[12,],aes(x=X,y=Y), slices=c("prop1","prop2","prop3"),
                 radius=0.5, colour="black") +
  geom_segment(data=temp_pie_region2[12,],aes(x=X,y=Y,xend=xend,yend=yend), 
               arrow = arrow(length = unit(0.1, "inches")), colour="black", linewidth=1) + 
  geom_label(data=temp_pie_region2[12,], 
             aes(label=paste0(region_abb,": ",round(swing*100,2),"%"), y=Y+25000, x=X), 
             size=2, colour="black", fontface="bold", fill="khaki") + 
  geom_circle(data=temp_pie_region2[12,],aes(x0=X,y0=Y,r=3000), fill="black") + 
  coord_sf(datum=NA) + 
  guides(fill="none") + 
  labs(title = "",
       subtitle = "by county") + 
  theme_bw() +
  theme(axis.title.x = element_blank()) +
  theme(axis.title.y = element_blank()) + 
  theme(panel.background = element_rect(fill = "ivory2", color = "black"))

ggarrange(
  region_smeter,
  county_smeter,
  ncol = 2
)



###############################################################################
##### Contiguities
###############################################################################



# Create contiguity matrix of constituencies, 
# with some manual alterations to account for islands, bridges, ferries and tunnels.

# make a list of contiguities
nlistconst <- df %>% st_touches()

# make it a named list (required for `mgcv`)
names(nlistconst) <- df$constituency_name

# customise it somewhat by fixing islands and inlets etc...

# fix island contiguities (Isle of Wight and Ynys Mon to nearest mainland constituency/ies)

# Portsmouth South is 384
nlistconst[[384]] <- c(202,252,383) |> as.integer()
# Gosport is 202
nlistconst[[202]] <- c(188,252,384) |> as.integer()
# Fareham is 188
nlistconst[[188]] <- c(171,202,252,303,383) |> as.integer()
# New Forest East is 329
nlistconst[[329]] <- c(252,330,403,414,436) |> as.integer()
# New Forest West is 330
nlistconst[[330]] <- c(122,252,329,339,414) |> as.integer()

# Isle of Wight is 252
nlistconst[[252]] <- c(188,202,329,330,384) |> as.integer()


# Arfon is 8
nlistconst[[8]] <- c(2,162,569) |> as.integer()
# Aberconwy is 2
nlistconst[[2]] <- c(8,129,162,569) |> as.integer()

# Ynys Mon is 569
nlistconst[[569]] <- c(2,8) |> as.integer()

# Humber estuary bridge. Cleethorpes (127) to Kingston Upon Hull West and Hessle (264)
nlistconst[[127]] <- c(74,196,206,264,290) |> as.integer()
nlistconst[[264]] <- c(127,214,262,263) |> as.integer()

# Poole (381) and South Dorset (440) are ferry connected
nlistconst[[381]] <- c(61,310,440) |> as.integer()
nlistconst[[440]] <- c(310,339,381,538) |> as.integer()

# Mersey crossings. Wallasey (517) and Birkenhead (34) to Liverpool Riverside (284)
nlistconst[[517]] <- c(34,284,551) |> as.integer()
nlistconst[[34]] <- c(284,517,550,551) |> as.integer()
nlistconst[[284]] <- c(34,57,197,285,286,517) |> as.integer()

# Dartford crossing. Dartford (146) and Thurrock (499)
nlistconst[[146]] <- c(33,205,368,420,499,501) |> as.integer()
nlistconst[[499]] <- c(144,146,241,437) |> as.integer()

# St Ives (470), connect it to 'Truro and Falmouth' (508), one constituency further back on peninsula
nlistconst[[470]] <- c(94,508) |> as.integer()
nlistconst[[508]] <- c(94,466,470) |> as.integer()

# it should be of class "nb" for `mgcv`
class(nlistconst) <- "nb"

# First order queen contiguity structure of constituencies in England and Wales, 
# shown as edges radiating from nodes at the centroid of constituencies.

# to show the contiguities on a map
# first, the dataframe must be a spdf, spatial datafram
df_sp <- as(df, 'Spatial')

# make lines where there are contiguities
neighbors <- nlistconst
neighbors_sf <- as(nb2lines(neighbors, coords = df_sp), 'sf')
neighbors_sf <- st_set_crs(neighbors_sf, st_crs(df))

# make a layer with only the two excluded speaker constituencies
# this can be overlayed on subsequent maps
speaker_seats <- uk_map_download |> 
  filter(pcon19nm %in% c("Chorley","Buckingham")) |> 
  summarise() |> 
  st_transform(crs=27700) |> 
  mutate(title="")

# map the connections
ggplot(df) + 
  geom_sf(data=regions, fill="white", colour="black", linewidth=0.2) + 
  geom_sf(data=df_scaled_sf, fill=NA, colour="gray70", linewidth=0.1) +
  geom_sf(data=uk_outline, fill=NA, colour="black", linewidth=0.4) + 
  geom_sf(data=speaker_seats, aes(fill=title), linewidth=0.05, colour="black") + 
  geom_sf(data = neighbors_sf, aes(colour="first-order\nqueen contiguity"), linewidth=0.08) +
  scale_colour_manual(values="darkred") + 
  scale_fill_manual(values = "black") + 
  labs(colour = "Neighbourhood\nstructure:                    ",
       fill = "Speaker seats") + 
  coord_sf(datum=NA) + 
  theme_minimal() + 
  theme_bw() +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) + 
  theme(legend.position = c(0.18, 0.82),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=6),
        legend.key.size = unit(0.5, "cm"),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))



###############################################################################
##### Explanatory variables
###############################################################################



var_table <- data.frame(
  `post-industrial / knowledge-economy` = c(
    "degree educated",
    "professional occupations",
    "younger adults",
    "",
    ""),
  `diversity / values / outcomes` = c(
    "english-speaking",
    "single-ethnicity",
    "health not good",
    "white",
    "christian"),
  `metropolitan / big-city` = c(
    "EU born, not UK",
    "own home",
    "don't own car",
    "private transport to work",
    "")
)

names(var_table) <- c("post-industrial / knowledge-economy",
                      "diversity / values / outcomes",
                      "metropolitan / big-city")

k1 <- kbl(var_table, 
          booktabs = TRUE,
          caption = "Explanatory variables considered by Beecham et al. (2018), separated into three thematic groupings.") |> 
  kable_classic(full_width = T, html_font = "Cambria") |> 
  row_spec(0, bold=T) |> 
  column_spec(1, border_right = TRUE) |>  
  column_spec(2, border_right = TRUE)

k1


var_table2 <- data.frame(
  variable2 = c("degree educated",
                "professional occupations",
                "younger adults",
                "health not good",
                "english-speaking",
                "single-ethnicity",
                "white",
                "christian"),
  justification2 = c("post-industrial / knowledge-economy /",
                     "peripherality",
                     "life outcomes / ",
                     "young people",
                     "ethnic / cultural diversity /",
                     "values",
                     "",
                     ""),
  rep_var = c("degree educated",
              "",
              "health not good",
              "",
              "white",
              "",
              "",
              "")
)

names(var_table2) <- c("grouped candidate variables","justification / theory","representative variable")

k2 <- kbl(var_table2, booktabs = TRUE,
          caption = "Reduced list of relevant explanatory variables grouped by justification, and the choice of one representative variable from each group for subsequent models.") |>
  kable_classic(full_width = F, html_font = "Cambria") |>
  # kable_styling(font_size = 12) |>
  pack_rows("1", 1, 2) |>
  pack_rows("2", 3, 4) |>
  pack_rows("3", 5, 8) |>
  column_spec(2, border_right = TRUE, italic = T) |>
  column_spec(1, border_right = TRUE, italic = F) |>
  column_spec(3, bold = T, italic = F)

k2


#######
### Further tables
#######


var_expl <- data.frame(
  `Explanatory variable` = c("degree educated",
                             "health not good",
                             "white"),
  `Calculation from census` = c("percentage of population with level 4 qualification or higher",
                                "percentage of the population self-reporting 'poor', 'bad', or 'very bad' health",
                                "percentage of population of white ethnicity")
)
names(var_expl) <- c(
  "explanatory variable",
  "calculation from census"
)
k3 <- kbl(var_expl, booktabs = TRUE,
          caption = "Description of explanatory variables used in subsequent models.") |>
  kable_classic(full_width = F, html_font = "Cambria") |>
  column_spec(1, border_right = TRUE, italic = F, bold = T) |> 
  column_spec(2, italic = T)

k3

# Values of independent variables mapped across England and Wales. 
# Figures projected as Dougenik cartograms such that equal populations occupy equal area 
# while maintaining constituency contiguities.

group1



###############################################################################
##### Modelling
###############################################################################



#######
### Simplest model
#######


mod0 <- lm(con_swing ~ 
             degree_educated + 
             health_not_good + 
             white,
           data=df_scaled)

df_mod0 <- df_scaled_sf |>
  mutate(residuals = mod0$residuals)
temprange <- range(min(df_mod0$residuals),max(df_mod0$residuals))

p_simp_corr <- ggplot() +
  geom_sf(data=df_mod0, colour=NA, aes(fill=residuals)) +
  geom_sf(data=uk_outline, fill=NA, colour="black") + 
  geom_sf(data=regions, fill=NA, colour="black") + 
  geom_sf(data=speaker_seats, fill="black", colour="black") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0) +
  labs(fill = "residuals") +
  coord_sf(datum=NA) +
  theme_bw() +
  theme(legend.position = c(0.16, 0.8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.5, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))



# Map of residuals from a simple linear model which does not take geography into account. 
# Regions such as the South West and Merseyside appear to be almost completely red (overprediction of swing), 
# the North East show a block of red alongside a block of blue, 
# while a blue pattern of underprediction spreads across the boundary between the East Midlands and Yorkshire and The Humber.

p_simp_corr


#######
### Model variations
#######


# This section takes a long time to run as it fits many models.
# It has been commented out to save time running it
# Its results are shown in a table afterwards

#######
### Model type 1: ICAR varying coefficients
#######
#
# 
# ## mod1 - hv
# 
# test_k_numbers <- c(53,63,73,83,93,103)
# 
# performance_mod1 <- data.frame(
#   k = rep(NA,length(test_k_numbers)),
#   RMSE = rep(NA,length(test_k_numbers)),
#   adjR2 = rep(NA,length(test_k_numbers)),
#   aic = rep(NA,length(test_k_numbers)),
#   loglik = rep(NA,length(test_k_numbers))
# )
# 
# temp_mod_list <- list()
# j=1
# 
# for (i in test_k_numbers){
#   
#   print(test_k_numbers[j])
#   
#   temp_mod_list[[j]] <- 
#     gam(con_swing ~
#           degree_educated +
#           health_not_good +
#           white +
#           s(region, bs="re") +
#           s(degree_educated,region, bs="re") +
#           s(health_not_good,region, bs="re") +
#           s(white,region, bs="re") +
#           s(county, bs="re") +
#           s(degree_educated,county, bs="re") +
#           s(health_not_good,county, bs="re") +
#           s(white,county, bs="re") +
#           s(constituency_name, by=degree_educated, bs='mrf', xt=list(nb=nlistconst), k=i) +
#           s(constituency_name, by=health_not_good, bs='mrf', xt=list(nb=nlistconst), k=i) +
#           s(constituency_name, by=white, bs='mrf', xt=list(nb=nlistconst), k=i),
#         data=df_scaled, method="REML")
#   
#   temp_mod_summary <- summary(temp_mod_list[[j]])
#   
#   performance_mod1[j,] <- c(i, 
#                             sum((predict.gam(temp_mod_list[[j]],df_scaled) - df_scaled_sf$con_swing)^2)/nrow(df_scaled),
#                             temp_mod_summary$r.sq,
#                             AIC(temp_mod_list[[j]]),
#                             as.numeric(glance(temp_mod_list[[j]])[2]))
#   
#   j = j+1
#   
# }
# 
# mod1_kopt <- performance_mod1$k[which.min(performance_mod1$aic)]
# mod1 <- temp_mod_list[[which.min(performance_mod1$aic)]]
# mod1_opt_performance <- performance_mod1[which.min(performance_mod1$aic),]
# 
#
#######
### Model type 2: ICAR constituency component
#######
# 
# ## mod2 - he
# 
# test_k_numbers <- c(282,288,290,292,294,296,300,305,311)
# 
# performance_mod2 <- data.frame(
#   k = rep(NA,length(test_k_numbers)),
#   RMSE = rep(NA,length(test_k_numbers)),
#   adjR2 = rep(NA,length(test_k_numbers)),
#   aic = rep(NA,length(test_k_numbers)),
#   loglik = rep(NA,length(test_k_numbers))
# )
# 
# temp_mod_list2 <- list()
# j=1
# 
# for (i in test_k_numbers){
#   
#   print(test_k_numbers[j])
#   
#   temp_mod_list2[[j]] <- 
#     gam(con_swing ~
#           degree_educated +
#           health_not_good +
#           white +
#           s(region, bs="re") +
#           s(degree_educated,region, bs="re") +
#           s(health_not_good,region, bs="re") +
#           s(white,region, bs="re") +
#           s(county, bs="re") +
#           s(degree_educated,county, bs="re") +
#           s(health_not_good,county, bs="re") +
#           s(white,county, bs="re") +
#           s(constituency_name, bs='mrf', xt=list(nb=nlistconst), k=i),
#         data=df_scaled, method="REML")
#   
#   temp_mod_summary2 <- summary(temp_mod_list2[[j]])
#   
#   performance_mod2[j,] <- c(i, 
#                             sum((predict.gam(temp_mod_list2[[j]],df_scaled) - df_scaled_sf$con_swing)^2)/nrow(df_scaled),
#                             temp_mod_summary2$r.sq,
#                             AIC(temp_mod_list2[[j]]),
#                             as.numeric(glance(temp_mod_list2[[j]])[2]))
#   
#   j = j+1
#   
# }
# 
# mod2_kopt <- performance_mod2$k[which.min(performance_mod2$aic)]
# mod2 <- temp_mod_list2[[which.min(performance_mod2$aic)]]
# mod2_opt_performance <- performance_mod2[which.min(performance_mod2$aic),]
# 
#
#######
### Model type 3: ICAR constituency component and varying coefficients
#######
# 
# ## mod3 - hev
# 
# test_k_numbers <- c(40,45,50,55,60,65,70)
# 
# performance_mod3 <- data.frame(
#   k = rep(NA,length(test_k_numbers)),
#   RMSE = rep(NA,length(test_k_numbers)),
#   adjR2 = rep(NA,length(test_k_numbers)),
#   aic = rep(NA,length(test_k_numbers)),
#   loglik = rep(NA,length(test_k_numbers))
# )
# 
# temp_mod_list3 <- list()
# j=1
# 
# for (i in test_k_numbers){
#   
#   print(test_k_numbers[j])
#   
#   temp_mod_list3[[j]] <- 
#     gam(con_swing ~
#           degree_educated +
#           health_not_good +
#           white +
#           s(region, bs="re") +
#           s(degree_educated,region, bs="re") +
#           s(health_not_good,region, bs="re") +
#           s(white,region, bs="re") +
#           s(county, bs="re") +
#           s(degree_educated,county, bs="re") +
#           s(health_not_good,county, bs="re") +
#           s(white,county, bs="re") +
#           s(constituency_name, bs='mrf', xt=list(nb=nlistconst), k=i) +
#           s(constituency_name, by=degree_educated, bs='mrf', xt=list(nb=nlistconst), k=i) +
#           s(constituency_name, by=health_not_good, bs='mrf', xt=list(nb=nlistconst), k=i) +
#           s(constituency_name, by=white, bs='mrf', xt=list(nb=nlistconst), k=i),
#         data=df_scaled, method="REML")
#   
#   temp_mod_summary3 <- summary(temp_mod_list3[[j]])
#   
#   performance_mod3[j,] <- c(i, 
#                             sum((predict.gam(temp_mod_list3[[j]],df_scaled) - df_scaled_sf$con_swing)^2)/nrow(df_scaled),
#                             temp_mod_summary3$r.sq,
#                             AIC(temp_mod_list3[[j]]),
#                             as.numeric(glance(temp_mod_list3[[j]])[2]))
#   
#   j = j+1
#   
# }
# 
# mod3_kopt <- performance_mod3$k[which.min(performance_mod3$aic)]
# mod3 <- temp_mod_list3[[which.min(performance_mod3$aic)]]
# mod3_opt_performance <- performance_mod3[which.min(performance_mod3$aic),]



###############################################################################
##### Model comparison
###############################################################################



# performance metrics for each of these model combinations, named models **1-3**. 
# Of these three potential structures, model **2**, which we have been discussing, 
# has the best performance metrics and was deemed the most suitable structure for modelling this particular dataset. 
# Such a process can be used to find the most suitable structure for any potential dataset.

# temp_mod_summary1 <- summary(mod1)
# performance_mod1 <- c(sqrt(sum((predict.gam(mod1,df_scaled) - df_scaled_sf$con_swing)^2)/nrow(df_scaled)),
#                       temp_mod_summary1$r.sq,
#                       AIC(mod1),
#                       logLik.gam(mod1)[1])
# 
# temp_mod_summary2 <- summary(mod2)
# performance_mod2 <- c(sqrt(sum((predict.gam(mod2,df_scaled) - df_scaled_sf$con_swing)^2)/nrow(df_scaled)),
#                       temp_mod_summary2$r.sq,
#                       AIC(mod2),
#                       logLik.gam(mod2)[1])
# 
# temp_mod_summary3 <- summary(mod3)
# performance_mod3 <- c(sqrt(sum((predict.gam(mod3,df_scaled) - df_scaled_sf$con_swing)^2)/nrow(df_scaled)),
#                       temp_mod_summary3$r.sq,
#                       AIC(mod3),
#                       logLik.gam(mod3)[1])
# 
# mod_all_opt_df <- rbind(
#   performance_mod1,
#   performance_mod2,
#   performance_mod3) |>
#   data.frame() |>
#   mutate(model = 1:3,
#          `autoregressive spatial process(es)` = c("varying coefficients",
#                                                   "constituency component",
#                                                   "constituency component + varying coefficients"))
# rownames(mod_all_opt_df) <- 1:3
# names(mod_all_opt_df) <- c("RMSE",
#                            "adjR2",
#                            "AIC",
#                            "loglik",
#                            "model",
#                            "autoregressive spatial process(es)")
# mod_all_opt_df <- mod_all_opt_df |>
#   select(model, `autoregressive spatial process(es)`, AIC, everything()) |>
#   mutate(AIC = round(AIC),
#          RMSE = round(RMSE,2),
#          adjR2 = round(adjR2,2),
#          loglik = round(loglik)) |>
#   arrange(AIC)
# 
# kbl(mod_all_opt_df,
#     caption = "Performances of a hierarchical structure with different combinations of spatial autocorrelation processes at lowest constituency level. Models 1 and 2 each incorporate one type of process while Model 3 includes both. Model 2 performs best on all metrics.",
#     align = "l") |>
#   kable_classic(full_width = F, html_font = "Cambria") |>
#   row_spec(0, bold=TRUE)



# These models take a long time to run, but when they do, they produce the following table:
  
  # | Model | Autoregressive spatial process(es)            | AIC  | RMSE | adjR2 | Loglik |
  # |-------|-----------------------------------------------|------|------|-------|--------|
  # | 1     | constituency component                        | 2336 | 1.43 | 0.76  | -1015  |
  # | 2     | varying coefficients                          | 2373 | 1.62 | 0.73  | -1086  |
  # | 3     | constituency component + varying coefficients | 2381 | 1.64 | 0.73  | -1094  |
  

#######
### Optimal model
#######


# mod2 is the optimal model from the above table (labelled as 1 in the table)
mod2 <- 
  gam(con_swing ~
        degree_educated +
        health_not_good +
        white +
        s(region, bs="re") +
        s(degree_educated,region, bs="re") +
        s(health_not_good,region, bs="re") +
        s(white,region, bs="re") +
        s(county, bs="re") +
        s(degree_educated,county, bs="re") +
        s(health_not_good,county, bs="re") +
        s(white,county, bs="re") +
        s(constituency_name, bs='mrf', xt=list(nb=nlistconst), k=311),
      data=df_scaled, method="REML")



###############################################################################
##### Results
###############################################################################



#######
### Fixed effects
#######


# extract the fixed effects from the model, and clean them
mod2_fix <- extract_fixed(mod2, ci_level = 0.95, digits = 3)
mod2_fix$term[mod2_fix$term=="Intercept"] <- "intercept"
mod2_fix$term <- factor(mod2_fix$term, levels = c(
  "white",
  "health_not_good",
  "degree_educated",
  "intercept"))

# set up a column to set colour scheme
mod2_fix$swingto <- case_when(mod2_fix$value>0 ~ "Conservative",
                              TRUE ~ "Labour")

# plot the fixed effects
ggplot() + 
  geom_point(data=mod2_fix,aes(x=value, y=term, fill=swingto, colour=swingto), shape=21, size=3) + 
  geom_errorbar(data=mod2_fix,aes(x=value, y=term,xmin=lower_2.5, xmax=upper_97.5, colour=swingto)) + 
  scale_fill_manual(breaks = c("Conservative", "Labour"),
                    values = c("#073763", "firebrick4")) + 
  scale_colour_manual(breaks = c("Conservative", "Labour"),
                      values = c("#073763", "firebrick4")) + 
  geom_vline(xintercept = 0, colour="red") + 
  # theme_bw() + 
  labs(fill = "direction of swing",
       colour = "direction of swing",
       x = "fixed effect",
       y = "") + 
  xlim(-4,6) + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size=6),
        legend.key.size = unit(0.4, "cm"),
        axis.title = element_text(size=6),
        axis.text = element_text(size=6),
        panel.background = element_rect(fill = "ivory2", color = "black")) + 
  scale_y_discrete("term",
                   labels = c(TeX(paste0("(white)    ", "$\\hat{\\beta}_3$")),
                              TeX(paste0("(health_not_good)    ", "$\\hat{\\beta}_2$")),
                              TeX(paste0("(degree_educated)    ", "$\\hat{\\beta}_1$")),
                              TeX(paste0("(intercept)    ", "$\\hat{\\beta}_0$"))))


#######
### Random effects
#######


# for extracting random effects from `mgcv`...
# set up a temp dataframe featuring all of the variables in the model
# set the covariates which will be estimated equal to 1
temp <- tibble(
  degree_educated=rep(1,nrow(df_scaled)),
  health_not_good=rep(1,nrow(df_scaled)),
  white=rep(1,nrow(df_scaled)),
  region=df$region,
  county=df_scaled$county,
  constituency_name=df_scaled$constituency_name)

# then predict from this model using type="terms"
mod2_df <- predict(mod2,newdata = temp,type = "terms", se.fit = TRUE) |> 
  as.data.frame() |> 
  cbind(df_scaled) |> 
  as.data.frame() |> 
  mutate(geometry = df$geometry) |> 
  st_as_sf()


#######
### Region random effects
#######


# make a non-contiguous cartogram of regions weighted by population
# to emphasise that a separate fitting is estimated for each region
# vary k until the regions are suitably deflated
carto_region_noncontig <- df |>
  st_drop_geometry() |>
  select(population,region) |>
  group_by(region) |>
  summarise(regionpop = sum(population)) |>
  inner_join(carto_region, by="region") |>
  st_as_sf() |>
  cartogram_ncont(weight="regionpop", k=0.7)

# abbreviate region names for the map
region_abb <- temp_pie_region2 |> 
  select(region,region_abb) |> 
  st_drop_geometry()

# join the non-contiguous cartogram geometries to the predicted values
# pick the region random effects and tidy the names
# as there are multiple entries for each region (there are 571 rows), remove the variables which identify constituency or county and then extract only the unique values
m_ran_region <- mod2_df |>
  st_drop_geometry() |> 
  left_join(carto_region_noncontig, by="region") |>
  st_as_sf() |> 
  select(region,
         fit.s.region.,
         fit.s.degree_educated.region.,
         fit.s.health_not_good.region.,
         fit.s.white.region.) |> 
  rename(intercept = fit.s.region.,
         degree_educated = fit.s.degree_educated.region.,
         health_not_good = fit.s.health_not_good.region.,
         white = fit.s.white.region.) |> 
  st_drop_geometry() |> 
  unique() |> 
  left_join(carto_region_noncontig |> select(region,geometry), by="region") |> 
  st_as_sf() |> 
  left_join(region_abb, by="region")

# define each plot separately, rather than in a function
# because otherwise not possible to add individual latex subscripts in subtitles

region_random_map_degree_educated <- ggplot(m_ran_region) + 
  geom_sf(aes(fill=degree_educated), colour="black", linewidth=0.3) +
  geom_sf_label(data=m_ran_region,aes(label=region_abb, fill=degree_educated), size=2.5, alpha=0.1, label.size=NA, colour="black") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, limits=c(-2,2.5)) +
  coord_sf(datum=NA) + 
  labs(title = TeX(paste0("degree educated (", "$\\hat{b}_{1i}$",")")),
       subtitle = TeX(paste0("with global fixed effect: ", "$\\hat{\\beta}_1$", " = ", mod2_fix[mod2_fix$term=="degree_educated",2])),
       fill = "region\neffect") + 
  theme_bw() +
  theme(legend.position = c(0.15, 0.75),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        axis.title = element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        legend.title = element_text(size = 6),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        panel.background = element_rect(fill = "ivory2", color = "black"))

region_random_map_health_not_good <- ggplot(m_ran_region) + 
  geom_sf(aes(fill=health_not_good), colour="black", linewidth=0.3) +
  geom_sf_label(data=m_ran_region,aes(label=region_abb, fill=health_not_good), size=2.5, alpha=0.1, label.size=NA, colour="black") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, limits=c(-2,2.5)) +
  coord_sf(datum=NA) + 
  labs(title = TeX(paste0("health not good (", "$\\hat{b}_{2i}$",")")),
       subtitle = TeX(paste0("with global fixed effect: ", "$\\hat{\\beta}_2$", " = ", mod2_fix[mod2_fix$term=="health_not_good",2])),
       fill = "region\neffect") + 
  theme_bw() +
  theme(legend.position = c(0.15, 0.75),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        axis.title = element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        legend.title = element_text(size = 6),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        panel.background = element_rect(fill = "ivory2", color = "black"))

region_random_map_white <- ggplot(m_ran_region) + 
  geom_sf(aes(fill=white), colour="black", linewidth=0.3) +
  geom_sf_label(data=m_ran_region,aes(label=region_abb, fill=white), size=2.5, alpha=0.1, label.size=NA, colour="black") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, limits=c(-2,2.5)) +
  coord_sf(datum=NA) + 
  labs(title = TeX(paste0("white (", "$\\hat{b}_{3i}$",")")),
       subtitle = TeX(paste0("with global fixed effect: ", "$\\hat{\\beta}_3$", " = ", mod2_fix[mod2_fix$term=="white",2])),
       fill = "region\neffect") + 
  theme_bw() +
  theme(legend.position = c(0.15, 0.75),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        axis.title = element_blank(),
        plot.title = element_text(size=10),
        plot.subtitle = element_text(size=8),
        legend.title = element_text(size = 6),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        panel.background = element_rect(fill = "ivory2", color = "black"))

pre_squarer <- ggarrange(region_random_map_degree_educated,
                         region_random_map_health_not_good,
                         region_random_map_white,
                         nrow = 1, ncol = 3)

pre_squarer


#######
### County random effects
#######


# use a similar process to regions now for counties
# remove constituency-identifying columns then extract unique rows
m_ran_county <- mod2_df |> 
  select(region,
         county,
         fit.s.county.,
         fit.s.degree_educated.county.,
         fit.s.white.county.) |> 
  rename(intercept = fit.s.county.) |> 
  st_drop_geometry() |> 
  left_join(carto_county, by="county") |> 
  unique() |> 
  st_as_sf()

# put them in alphabetical order so that they can be assigned their place in a grid of maps which is made to represent the shape of England and Wales below
m_ran_unique <- m_ran_county |> 
  st_drop_geometry() |> 
  unique() |> 
  arrange(region)


####
### County random effects - degree_eduacted
####


# loop through all of the regions and map their counties
p_list <- list()
for (i in 1:nrow(regions)){
  temp <- regions$region[i]
  p_list[[i]] <- ggplot(m_ran_county |> filter(region==regions$region[i])) +
    geom_sf(aes(fill=fit.s.degree_educated.county.), linewidth=0.3, colour="black") +
    geom_sf_label_repel(aes(label=county), size=1.5, fontface="bold", alpha=0.7) + 
    coord_sf(datum=NA) + 
    scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, 
                         limits = c(min(m_ran_county$fit.s.degree_educated.county.), max(m_ran_county$fit.s.degree_educated.county.))) + 
    labs(title = temp,
         subtitle = paste0("net ",round(mod2_fix[2,2] + (m_ran_unique[m_ran_unique$region==temp,3]),2)," +")) + 
    theme_bw() + 
    guides(fill="none") + 
    # theme(legend.position = "bottom") + 
    theme(plot.title = element_text(size=8),
          plot.subtitle = element_text(size=6)) +
    theme(axis.title.x = element_blank()) + 
    theme(axis.title.y = element_blank(),
          panel.background = element_rect(fill = "ivory2", color = "black"))
}

# redefine one of the plots above so that it has a legend
p_list[[5]]<- ggplot(m_ran_county |> filter(region==regions$region[5])) +
  geom_sf(aes(fill=fit.s.degree_educated.county.), linewidth=0.3, colour="black") +
  geom_sf_label_repel(aes(label=county), size=1.5, fontface="bold", alpha=0.7) + 
  coord_sf(datum=NA) + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, 
                       limits = c(min(m_ran_county$fit.s.degree_educated.county.), max(m_ran_county$fit.s.degree_educated.county.))) + 
  labs(title = regions$region[5],
       subtitle = paste0("net ",round(mod2_fix[2,2] + m_ran_unique[5,3],2)," +"),
       fill = "") + 
  theme_bw() + 
  theme(legend.position = "left") + 
  theme(plot.title = element_text(size=8),
        plot.subtitle = element_text(size=6)) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        panel.background = element_rect(fill = "ivory2", color = "black"))

# put maps together in the desired grid shape
ranef_county_degree_plot <- (plot_spacer()|plot_spacer()|p_list[[5]]|plot_spacer()) /
  (plot_spacer()|p_list[[4]]|p_list[[6]]|p_list[[11]]|plot_spacer()) /
  (p_list[[9]]|p_list[[10]]|p_list[[2]]|p_list[[1]]) /
  (plot_spacer()|p_list[[8]]|p_list[[3]]|p_list[[7]]) +
  plot_annotation(
    title = "County (within region)",
    subtitle = TeX(paste0("degree_educated (", "$\\hat{b}_{1ij}$",")")),
    theme = theme(plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8))
  )


####
### County random effects - white
####


# same process of creating grid of regions as above
p_list <- list()
for (i in 1:nrow(regions)){
  temp <- regions$region[i]
  p_list[[i]] <- ggplot(m_ran_county |> filter(region==regions$region[i])) +
    geom_sf(aes(fill=fit.s.white.county.), linewidth=0.3, colour="black") +
    geom_sf_label_repel(aes(label=county), size=1.5, fontface="bold", alpha=0.7) + 
    coord_sf(datum=NA) + 
    scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, 
                         limits = c(min(m_ran_county$fit.s.white.county.), max(m_ran_county$fit.s.white.county.))) + 
    labs(title = temp,
         subtitle = paste0("net ",round(mod2_fix[4,2] + (m_ran_unique[m_ran_unique$region==temp,5]),2)," +")) + 
    theme_bw() + 
    guides(fill="none") + 
    theme(plot.title = element_text(size=8),
          plot.subtitle = element_text(size=6)) +
    theme(axis.title.x = element_blank()) + 
    theme(axis.title.y = element_blank(),
          panel.background = element_rect(fill = "ivory2", color = "black"))
}

# make one with a legend
p_list[[5]]<- ggplot(m_ran_county |> filter(region==regions$region[5])) +
  geom_sf(aes(fill=fit.s.white.county.), linewidth=0.3, colour="black") +
  geom_sf_label_repel(aes(label=county), size=1.5, fontface="bold", alpha=0.7) + 
  coord_sf(datum=NA) + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, 
                       limits = c(min(m_ran_county$fit.s.white.county.), max(m_ran_county$fit.s.white.county.))) + 
  labs(title = regions$region[5],
       subtitle = paste0("net ",round(mod2_fix[4,2] + m_ran_unique[m_ran_unique$region==temp,5],2)," +"),
       fill = "") + 
  theme_bw() + 
  theme(legend.position = "left") + 
  theme(plot.title = element_text(size=8),
        plot.subtitle = element_text(size=6)) +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title.y = element_blank()) + 
  theme(legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        panel.background = element_rect(fill = "ivory2", color = "black"))

ranef_county_white_plot <- (plot_spacer()|plot_spacer()|p_list[[5]]|plot_spacer()) /
  (plot_spacer()|p_list[[4]]|p_list[[6]]|p_list[[11]]|plot_spacer()) /
  (p_list[[9]]|p_list[[10]]|p_list[[2]]|p_list[[1]]) /
  (plot_spacer()|p_list[[8]]|p_list[[3]]|p_list[[7]]) +
  plot_annotation(
    title = "County (within region)",
    subtitle = TeX(paste0("white (", "$\\hat{b}_{3ij}$",")")),
    theme = theme(plot.title = element_text(size = 10),
                  plot.subtitle = element_text(size = 8))
  )

ranef_county_degree_plot

ranef_county_white_plot


#######
### Table of variances
#######


# make a table to show the variances estimated by the model

# extract variances, tidy names
m_variance <- gratia::variance_comp(mod2, coverage=0.95) |> 
  mutate(component = str_remove(component, "^s\\("),
         component = str_remove(component, "\\)$"))

# design table
varsum <- sum(m_variance$variance)

m_variance$variable <- 0 # inititalise column
m_variance$variable[1] <- "$\\sigma^{2}_{region,intercept}$"
m_variance$variable[2] <- "$\\sigma^{2}_{region,degree}$"
m_variance$variable[3] <- "$\\sigma^{2}_{region,health}$"
m_variance$variable[4] <- "$\\sigma^{2}_{region,white}$"
m_variance$variable[5] <- "$\\sigma^{2}_{county,intercept}$"
m_variance$variable[6] <- "$\\sigma^{2}_{county,degree}$"
m_variance$variable[7] <- "$\\sigma^{2}_{county,health}$"
m_variance$variable[8] <- "$\\sigma^{2}_{county,white}$"
m_variance$variable[9] <- "$\\sigma^{2}_{constituency,ICAR}$"
m_variance$variable[10] <- "$\\sigma^{2}$"

m_variance$variable <- c("$\\sigma^{2}_{region,int}$",
                         "$\\sigma^{2}_{region,degree}$",
                         "$\\sigma^{2}_{region,health}$",
                         "$\\sigma^{2}_{region,white}$",
                         "$\\sigma^{2}_{county,int}$",
                         "$\\sigma^{2}_{county,degree}$",
                         "$\\sigma^{2}_{county,health}$",
                         "$\\sigma^{2}_{county,white}$",
                         "$\\sigma^{2}_{constituency,ICAR}$",
                         "$\\sigma^{2}$")

m_variance$level <- c("region",
                      "region",
                      "region",
                      "region",
                      "county",
                      "county",
                      "county",
                      "county",
                      "constituency",
                      "constituency")

mod2_pvals <- summary(mod2)[8] |> 
  unlist() |> 
  round(3)

m_variance$level <- factor(m_variance$level, levels = c("region","county","constituency"))

m_variance <- m_variance |> 
  arrange(level) |> 
  mutate(`var %` = 100 * variance / varsum,
         `cum.var` = cumsum(variance),
         `cum.var %` = 100 * `cum.var` / varsum,
         pval = c(mod2_pvals,""),
         sig = case_when(pval>0.1~"",
                         pval>0.05~".",
                         pval>0.01~"*",
                         pval>0.001~"**",
                         TRUE~"***"),
         variance = round(variance,2),
         std_dev = round(std_dev,2),
         `var %` = round(`var %`,1),
         `cum.var` = round(`cum.var`,1),
         `cum.var %` = round(`cum.var %`,1)) |> 
  mutate(variance = ifelse(variance > 0.01, variance, "<0.01"),
         std_dev = ifelse(std_dev > 0.01, variance, "<0.01"),
         `cum.var %` = ifelse(`cum.var %` > 0.01, `cum.var %`, "<0.01"),
         `var %` = ifelse(`var %` > 0.01, `var %`, "<0.01")) |> 
  select(-c(level,component,lower_ci,upper_ci,`cum.var`,std_dev)) |> 
  select(variable,everything())

# this helps to tidy the title row and allow two lines in that row
colnames(m_variance) <-c("","","","","","","")

var_table <- kbl(m_variance, escape = FALSE, 
                 caption = "Variance explained by model, with associated measures of significance, at different levels and for different spatial processes. There is significant variation in the association with 'health' and 'white' at region level, while at county level 'degree' and 'white' show significant divergence.",
                 booktabs = T,
                 align = "lrrrll") |> 
  kable_classic(full_width = F, html_font = "Cambria") |> 
  group_rows("region", 1, 4) |>
  group_rows("county", 5, 8) |>
  group_rows("constituency (ICAR)", 9, 9) |>
  group_rows("residuals", 10, 10) |> 
  add_header_above(c("level",
                     "variance",
                     "variance\n%",
                     "cumulative\nvariance %",
                     "F test\np-val",
                     ""),
                   line = FALSE,
                   bold = TRUE) |> 
  footnote(general="Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1")

var_table


#######
### Net hierarchical effects
#######


# to show net effects on a map, create a dataframe which adds them appropriately
# net effects: fixed + region (+ county)
randoms <- mod2_df |> 
  mutate(degree_educated_region = fit.degree_educated + fit.s.degree_educated.region.,
         health_not_good_region = fit.health_not_good + fit.s.health_not_good.region.,
         white_region = fit.white + fit.s.white.region.,
         degree_educated_county = degree_educated_region + fit.s.degree_educated.county.,
         health_not_good_county = health_not_good_region + fit.s.health_not_good.county.,
         white_county = white_region + fit.s.white.county.) |> 
  select(region,
         county,
         constituency_name,
         degree_educated_region,
         health_not_good_region,
         white_region,
         degree_educated_county,
         health_not_good_county,
         white_county) |> 
  st_drop_geometry()

# make a region net effects df with region cartograms
net_region_effect_df <- randoms |> 
  select(region,
         degree_educated_region,
         health_not_good_region,
         white_region) |> 
  left_join(carto_region, by="region") |> 
  unique() |> 
  st_as_sf()

# make a county net effects df with county cartograms
net_county_effect_df <- randoms |> 
  select(county,
         degree_educated_county,
         health_not_good_county,
         white_county) |> 
  left_join(carto_county, by="county") |> 
  unique() |> 
  st_as_sf()

# each plot is made separately rather than with a function
# because the TeX labelling needs to be individually set...

cou_degree <- ggplot() + 
  geom_sf(data=net_county_effect_df, aes(fill=degree_educated_county), colour=NA) + 
  geom_sf(data=carto_region, fill=NA, colour="black", linewidth=0.3) + 
  # geom_sf(data=speaker_carto, fill="black") + 
  geom_sf_label(data=m_ran_region,aes(label=region_abb), fill=NA, size=2.5, alpha=0.1, label.size=NA, colour="white", fontface="bold") + 
  # scale_fill_gradient2(low="darkred",high="white", limits=c(-2.9,-1)) +
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, limits=c(-2.9,0)) + 
  coord_sf(datum=NA) + 
  theme_bw() +
  theme(legend.position = c(0.15, 0.72),
        legend.title = element_text(size = 6),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.text = element_text(size=6),
        legend.key.size = unit(0.4, "cm"),
        panel.border=element_rect(colour="black", linewidth=1),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"),
        axis.title = element_blank()) + 
  labs(fill=TeX("$\\hat{\\beta}_1+\\hat{b}_{1i}+\\hat{b}_{1ij}$"),
       subtitle="degree educated")

cou_health <- ggplot() + 
  geom_sf(data=net_county_effect_df, aes(fill=health_not_good_county), colour=NA) + 
  geom_sf(data=carto_region, fill=NA, colour="black", linewidth=0.3) + 
  geom_sf_label(data=m_ran_region,aes(label=region_abb), fill=NA, size=2.5, alpha=0.1, label.size=NA, colour="black", fontface="bold") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, limits=c(-1,3)) + 
  coord_sf(datum=NA) + 
  theme_bw() +
  theme(legend.position = c(0.15, 0.72),
        legend.title = element_text(size = 6),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.text = element_text(size=6),
        legend.key.size = unit(0.4, "cm"),
        panel.border=element_rect(colour="black", linewidth=1),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"),
        axis.title = element_blank()) + 
  labs(fill=TeX("$\\hat{\\beta}_2+\\hat{b}_{2i}+\\hat{b}_{2ij}$"),
       subtitle="health not good")

cou_white <- ggplot() +
  geom_sf(data=net_county_effect_df, aes(fill=white_county), colour=NA) +
  geom_sf(data=carto_region, fill=NA, colour="black", linewidth=0.3) + 
  geom_sf_label(data=m_ran_region,aes(label=region_abb), fill=NA, size=2.5, alpha=0.1, label.size=NA, colour="black", fontface="bold") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0, limits=c(-1,3)) +
  coord_sf(datum=NA) +
  theme_bw() +
  theme(legend.position = c(0.15, 0.72),
        legend.title = element_text(size = 6),
        plot.title = element_text(size = 12),
        plot.subtitle = element_text(size = 8),
        legend.text = element_text(size=6),
        legend.key.size = unit(0.4, "cm"),
        panel.border=element_rect(colour="black", linewidth=1),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"),
        axis.title = element_blank()) +
  labs(fill=TeX("$\\hat{\\beta}_3+\\hat{b}_{3i}+\\hat{b}_{3ij}$"),
       subtitle="white")

ggarrange(cou_degree,cou_health,cou_white,
          nrow = 1, ncol = 3)


#######
### Constituency level ICAR
#######


# in order to plot the MRF component with two chosen circles
# pick desired constituencies to centre these circles on
circlecentres <- mod2_df |> 
  filter(constituency_name %in% c("Don Valley","Nuneaton")) |> 
  st_centroid(circlecentres$geometry)
circlecentres[1,] <- circlecentres[1,] |> 
  st_buffer(dist=60000)
circlecentres[2,] <- circlecentres[2,] |> 
  st_buffer(dist=40000)

# then plot and fill by the MRF column in the prediction
ggplot() +
  geom_sf(data=mod2_df,aes(fill=fit.s.constituency_name.), colour=NA) +
  geom_sf(data=counties, fill=NA, linewidth=0.05, colour="gray50") +
  geom_sf(data=regions, fill=NA, linewidth=0.5, colour="black") +
  geom_sf(data=circlecentres, colour="red", fill=NA, linewidth=0.5) + 
  geom_sf(data=speaker_seats, fill="black",colour="black") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0) +
  labs(fill=TeX("ICAR ($\\hat{\\gamma}_{l|m})")) +
  coord_sf(datum=NA) +
  theme_bw() + 
  theme(legend.position = c(0.16, 0.8),
        legend.title = element_text(size = 10),
        legend.text = element_text(size=8),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.5, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))


#######
### Spatial diagnostics of model
#######


# create a spatial weight matrix from the contiguities list
# this is necessary for the moran.test function
df_wts <- nb2listw(nlistconst, zero.policy = TRUE)

# make a df with the residuals of the model and a constituency cartogram geometry column
res_df <- df_scaled_sf |> 
  select(constituency_name,county,region,geometry) |> 
  mutate(resids = resid(mod2))

# calculate Moran's I
mp <- moran.test(res_df$resids, df_wts, zero.policy = TRUE)

# map the residuals
resmap <- ggplot() +
  geom_sf(data=res_df, aes(fill=resids), colour=NA) +
  geom_sf(data=speaker_seats, fill="black", colour="black") + 
  scale_fill_gradient2(low="darkred",mid="white",high="#073763",midpoint = 0) +
  geom_sf(data=counties, fill=NA, linewidth=0.1, colour="gray50") +
  geom_sf(data=regions, fill=NA, linewidth=0.5, colour="black") +
  coord_sf(datum=NA) +
  labs(fill = "Residuals") + 
  theme_bw() + 
  theme(legend.position = c(0.16, 0.8),
        legend.title = element_text(size = 8),
        legend.text = element_text(size=5),
        legend.key.size = unit(0.4, "cm"),
        legend.background = element_rect(linewidth = 0.25, colour = 1),
        panel.background = element_rect(fill = "ivory2", color = "black"))

# create a scatter plot of residuals versus lagged residuals using moran.plot
mplot <- moran.plot(res_df$resids, df_wts, zero.policy = TRUE, labels = res_df$constituency_name, plot=FALSE) |>
  mutate(region=df$region)
# do a simple regression to show the slope
temp_reg <- lm(wx~x,mplot)

# combine the scatter plot and the regression line
xname <- attr(mplot, "xname")
reslag <- ggplot(mplot, aes(x=x, y=wx)) +
  geom_point(shape=1, colour="darkred", alpha=0.9) +
  # geom_smooth(formula=y ~ x, method="lm", colour="darkolivegreen", size=1, se=FALSE) +
  geom_hline(yintercept=mean(mplot$wx), lty=2, colour="black") +
  geom_vline(xintercept=mean(mplot$x), lty=2, colour="black") +
  # theme_bw() +
  geom_point(data=mplot[mplot$is_inf,], aes(x=x, y=wx), shape=1, colour="darkred") +
  xlab("Residuals") +
  ylab(paste0("Spatially lagged Residuals")) +
  # uncomment the following limits to exclude outliers
  # which disrupt the scale of the plot
  # ylim(-4,4) +
  # xlim(-4,4) +
  theme(plot.title = element_text(size=8),
        plot.subtitle = element_text(size=6),
        axis.title = element_text(size=8),
        panel.background = element_rect(fill = "ivory2", color = "black"))

empty <- ggplot() +
  theme_void()

ggarrange(resmap, empty, reslag, nrow = 1, ncol = 3, widths = c(0.6,0.05,0.4))


