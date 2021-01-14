# Load libraries, data -----------------------------------------------

library(tidyverse)
library(readxl)
library(stringr)
library(shiny)
library(shinydashboard)

# Load 19 20 Season Stats
Leaders <- read_xlsx("19.20Season.xlsx", skip = 1)

# Split up the name column 
Leaders <- Leaders %>% 
  separate(col = Player, into = c("Name", "String"), sep = "\\\\")

# Jared
Jared <- Leaders %>% 
  filter(Name %in% c("Adam Henrique", "Phil Kessel", "David Pastrnak", "Rasmus Dahlin",
                     "Matthew Tkachuk", "Dougie Hamilton", "Dominik Kubalík", "Mikko Rantanen",
                     "Zach Werenski", "John Klingberg", "Tyler Bertuzzi", "Connor McDavid",
                     "Keith Yandle", "Anze Kopitar", "Zach Parise", "Tomas Tatar",
                     "Ryan Johansen", "Jack Hughes", "Anders Lee", "Artemi Panarin",
                     "Evgenii Dadonov", "Claude Giroux", "Jake Guentzel", "Erik Karlsson",
                     "Vladimir Tarasenko", "Brayden Point", "John Tavares", "Bo Horvat",
                     "Max Pacioretty", "Nicklas Backstrom", "Patrik Laine", "Gabriel Landeskog")) %>% 
  mutate(Player = "Jared")

Dan <- Leaders %>% 
  filter(Name %in% c("Ryan Getzlaf", "Nick Schmaltz", "Patrice Bergeron", "Jack Eichel",
                     "Elias Lindholm", "Evgeny Svechnikov", "Alex DeBrincat", "Cale Makar",
                     "Pierre-Luc Dubois", "Denis Gurianov", "Dylan Larkin", "Kailer Yamamoto",
                     "Jonathan Huberdeau", "Adrian Kempe", "Ryan Suter", "Phillip Danault",
                     "Roman Josi", "Nico Hischier", "Brock Nelson", "Alexis Lafreniere", "Thomas Chabot",
                     "Jakub Voracek", "Sidney Crosby", "Timo Meier", "Brayden Schenn", "Steven Stamkos",
                     "Mitch Marner", "Elias Pettersson", "Alex Pietrangelo", "Alex Ovechkin",
                     "Mark Scheifele", "Jakub Vrana")) %>% 
  mutate(Player = "Dan")

# Devin
Devin <- Leaders %>% 
  filter(Name %in% c("Rickard Rakell", "Conor Garland", "Brad Marchand", "Taylor Hall",
                     "Johnny Gaudreau", "Teuvo Teravainen", "Patrick Kane", "Andre Burakovsky",
                     "Oliver Bjorkstrand", "Jamie Benn", "Anthony Mantha", "Ryan Nugent-Hopkins",
                     "Aleksander Barkov", "Alex Iafallo", "Kevin Fiala", "Nick Suzuki",
                     "Filip Forsberg", "Nikita Gusev", "Mathew Barzal", "Anthony DeAngelo",
                     "Brady Tkachuk", "Sean Couturier", "Bryan Rust", "Brent Burns", "Ryan O'Reilly",
                     "Victor Hedman", "William Nylander", "J.T. Miller", "Shea Theodore",
                     "John Carlson", "Blake Wheeler", "Jonathan Marchessault")) %>% 
  mutate(Player = "Devin")

# John
John <- Leaders %>% 
  filter(Name %in% c("Jakub Silfverberg", "Clayton Keller", "Charlie McAvoy", "Victor Olofsson",
                     "Sean Monahan", "Sebastian Aho", "Dylan Strome", "Nathan MacKinnon",
                     "Seth Jones", "Miro Heiskanen", "Robby Fabbri", "Leon Draisaitl", "Patric Hornqvist",
                     "Drew Doughty", "Kirill Kaprizov", "Brendan Gallagher", "Ryan Ellis",
                     "Kyle Palmieri", "Jordan Eberle", "Mika Zibanejad", "Connor Brown",
                     "Travis Konecny", "Evgeni Malkin", "Logan Couture", "David Perron", "Anthony Cirelli",
                     "Auston Matthews", "Quinn Hughes", "Mark Stone", "Evgeny Kuznetsov",
                     "Kyle Connor", "Mike Hoffman")) %>% 
  mutate(Player = "John")

# Bind the four sets into one

ALL_FANTASY_PLAYERS <- bind_rows(Jared, Devin, Dan, John)


ALL_FANTASY_PLAYERS <- ALL_FANTASY_PLAYERS %>% 
  rename(Points = `PTS▼`)

Teams <- ALL_FANTASY_PLAYERS %>% 
  group_by(Player) %>% 
  summarise(Points = sum(Points),
            A = sum(A),
            G = sum(G),
            GP = sum(GP))


# Page 1 - Introduction ----------------------------------------------
intro_panel <- tabPanel(
  "Fantasy Hockey",
  
  titlePanel("Fantasy Hockey 2021"),
  
  p("This is an R Shiny Dashboard for the 2021 Fantasy Hockey League")
)

# Page 2 - Visualization -------------------------------------------
select_values <- colnames(Teams)
select_values <- select_values[select_values %in% c('Points', 'GP', 'G', 'A')]

sidebar_content <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Statistics",
    choices = select_values,
    selected = 'Points'
  )
)

main_content <- mainPanel(
  plotOutput("plot")
)

second_panel <- tabPanel(
  "Standings",
  titlePanel("Fantasy Hockey 2021"),
  p("Select Inputs Below"),
  sidebarLayout(
    sidebar_content, main_content
  )
)
# Create Third Panel
select_values3 <- colnames(ALL_FANTASY_PLAYERS)
select_values3 <- select_values3[!select_values3 %in% c('String', 'Rk', 'Age', 'Pos')]

sidebar_content3 <- sidebarPanel(
  selectInput(
    "y_var",
    label = "Team Owner",
    choices = select_values3,
    selected = 'Jared'
  )
)
main_content3 <- mainPanel(
  dataTableOutput('table')
)

third_panel <- tabPanel(
  "Player Performance",
  titlePanel("Fantasy Hockey 2021"),
  p("Player Performance for each Team"),
  sidebarLayout(
    sidebar_content3, main_content3
  )
)

# User Interface -----------------------------------------------------
ui <- navbarPage(
  "Fantasy Hockey 2021",
  intro_panel,
  second_panel,
  third_panel
)

