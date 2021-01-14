library(tidyverse)
library(readxl)
library(stringr)

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

Table_1_Output <- ALL_FANTASY_PLAYERS %>% 
  select(Name, Tm, Points, Player) %>% 
  mutate(Points = as.integer(Points)) %>% 
  arrange(desc(Points))

Teams <- ALL_FANTASY_PLAYERS %>% 
  group_by(Player) %>% 
  summarise(Points = sum(Points),
            A = sum(A),
            G = sum(G),
            GP = sum(GP))

# Create the server -------------------------------------------------------


server <- function(input, output) {
  output$plot <- renderPlot({
    ggplot(data = Teams, aes_string(x = 'Player', y = input$y_var, fill = "Player"))+
      geom_bar(stat = "identity")+
      labs(x = "Owners", y = input$y_var) +
      ggtitle("2019-2020 Season") + coord_flip()
  })
  output$table <- renderDataTable(Table_1_Output, options = list)
  }

