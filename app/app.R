# packages
library(shiny)
library(shinydashboard)
library(ShinyRatingInput)
library(shinyjs)
library(recommenderlab)
library(tidyverse)
library(data.table)
library(rsconnect)

# load helper function
source('BR/functions/helpers.R')


########### System I ##########

# read in data
myurl = "https://liangfgithub.github.io/MovieData/"
ratings = read.csv(paste0(myurl, 'ratings.dat?raw=true'), 
                   sep = ':',
                   colClasses = c('integer', 'NULL'), 
                   header = FALSE)
colnames(ratings) = c('UserID', 'MovieID', 'Rating', 'Timestamp')
movies = readLines(paste0(myurl, 'movies.dat?raw=true'))
movies = strsplit(movies, split = "::", fixed = TRUE, useBytes = TRUE)
movies = matrix(unlist(movies), ncol = 3, byrow = TRUE)
movies = data.frame(movies, stringsAsFactors = FALSE)
colnames(movies) = c('MovieID', 'Title', 'Genres')
movies$MovieID = as.integer(movies$MovieID)

genres = as.data.frame(movies$Genres, stringsAsFactors=FALSE)
genres1 = as.data.frame(tstrsplit(genres[,1], '[|]',
                                  type.convert=TRUE),
                        stringsAsFactors=FALSE)
genre_list = c("Action", "Adventure", "Animation", 
               "Children's", "Comedy", "Crime",
               "Documentary", "Drama", "Fantasy",
               "Film-Noir", "Horror", "Musical", 
               "Mystery", "Romance", "Sci-Fi", 
               "Thriller", "War", "Western")
genre_matrix = matrix(0, nrow(movies), length(genre_list))

for(i in 1:nrow(genres1)){
    genre_matrix[i,genre_list %in% genres1[i,]]=1
}
colnames(genre_matrix) = genre_list

jointdata = ratings %>% 
    left_join(data.frame(MovieID = movies$MovieID, genre_matrix), 
              by = "MovieID") %>%
    select(-c("UserID", "Timestamp"))

# movie images
small_image_url = "https://liangfgithub.github.io/MovieImages/"
movies$image_url = sapply(movies$MovieID, 
                          function(x) paste0(small_image_url, x, '.jpg?raw=true'))


############### System II ################
# define functions
get_user_ratings = function(value_list) {
    dat = data.table(MovieID = sapply(strsplit(names(value_list), "_"), 
                                      function(x) ifelse(length(x) > 1, x[[2]], NA)),
                     Rating = unlist(as.character(value_list)))
    dat = dat[!is.null(Rating) & !is.na(MovieID)]
    dat[Rating == " ", Rating := 0]
    dat[, ':=' (MovieID = as.numeric(MovieID), Rating = as.numeric(Rating))]
    dat = dat[Rating > 0]
    
    # get the indices of the ratings
    # add the user ratings to the existing rating matrix
    user_ratings <- sparseMatrix(i = rep(1,nrow(dat)),
                                 j = dat$MovieID,
                                 x = dat$Rating, 
                                 dims = c(1,ncol(Rmat)))
}

i = paste0('u', ratings$UserID)
j = paste0('m', ratings$MovieID)
x = ratings$Rating
tmp = data.frame(i, j, x, stringsAsFactors = T)
Rmat = sparseMatrix(as.integer(tmp$i), as.integer(tmp$j), x = tmp$x)
rownames(Rmat) = levels(tmp$i)
colnames(Rmat) = levels(tmp$j)


Sidebar <- dashboardSidebar(
    sidebarMenu(
    menuItem("Recommender by Genre", tabName="Genre"),
    menuItem("Recommender by Rating", tabName="Rating")
    )
)

Body <- dashboardBody(
    tabItems(
        tabItem(tabName = "Genre",
                includeCSS("BR/css/movies.css"),
                fluidRow(
                    box(width = 12, title = "Step 1: Select Your Favoriate Genres", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        selectInput("genre",
                                    label = "select a single genre from the menu:",
                                    choices = c("Use the dropdown list" = "", "Action", "Adventure", "Animation", 
                                                "Children.s", "Comedy", "Crime",
                                                "Documentary", "Drama", "Fantasy",
                                                "Film.Noir", "Horror", "Musical", 
                                                "Mystery", "Romance", "Sci.Fi", 
                                                "Thriller", "War", "Western")),
                        div(class = "genreitems"
                        )
                    )
                ),
                fluidRow(
                    useShinyjs(),
                    box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover Movies you might like",
                        br(),
                        withBusyIndicatorUI(
                            actionButton("btn1", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results1")
                    )
                )),
        tabItem(tabName = "Rating",
                includeCSS("BR/css/movies.css"),
                fluidRow(
                    box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
                        div(class = "rateitems",
                            uiOutput('ratings')
                        )
                    )
                ),
                fluidRow(
                    useShinyjs(),
                    box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Step 2: Discover movies you might like",
                        br(),
                        withBusyIndicatorUI(
                            actionButton("btn2", "Click here to get your recommendations", class = "btn-warning")
                        ),
                        br(),
                        tableOutput("results2")
                    )
                ))
    )
)

UI <- dashboardPage(
    dashboardHeader(title = "Movie Recommender"),
    sidebar = Sidebar,
    body = Body
)

Server = function(input, output){
    
    output$ratings <- renderUI({
        num_rows <- 10
        num_movies <- 5 # movies per row
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                list(box(width = 2,
                         div(style = "text-align:center", img(src = movies$image_url[(i - 1) * num_movies + j], height = 150)),
                         #div(style = "text-align:center; color: #999999; font-size: 80%", books$authors[(i - 1) * num_books + j]),
                         div(style = "text-align:center", strong(movies$Title[(i - 1) * num_movies + j])),
                         div(style = "text-align:center; font-size: 150%; color: #f0ad4e;", ratingInput(paste0("select_", movies$MovieID[(i - 1) * num_movies + j]), label = "", dataStop = 5)))) #00c0ef
            })))
        })
    })
    
    df2 <- eventReactive(input$btn2, {
        withBusyIndicatorServer("btn2", { # showing the busy indicator
            # hide the rating container
            useShinyjs()
            jsCode <- "document.querySelector('[data-widget=collapse]').click();"
            runjs(jsCode)
            
            # get the user's rating data
            value_list <- reactiveValuesToList(input)
            user_ratings <- get_user_ratings(value_list)
            
            # add user's ratings as first column to rating matrix
            rmat_user <- rbind(user_ratings, Rmat)
            rmat_user = new('realRatingMatrix', data = rmat_user)
            
            # get the indices of which cells in the matrix should be predicted
            # predict all books the current user has not yet rated
            
            rec_UBCF = Recommender(rmat_user, method = 'UBCF',
                                   parameter = list(normalize = 'Z-score', 
                                                    method = 'Cosine', weighted = FALSE,
                                                    nn = 30))
            res = predict(rec_UBCF, rmat_user[1], type="ratings")
            ids = order(as(res[1,], "matrix"), decreasing = TRUE)[1:10]
            user_ids = colnames(as(res[1,], "matrix"))[ids]
            user_ids = substring(user_ids, 2)
            user_ids = as.integer(user_ids)
            title = NULL
            for (i in 1:10){
                title[i] = movies$Title[which(movies$MovieID == user_ids[i])]
            }
            recom_results <- data.table(Rank = 1:10, 
                                        MovieID = user_ids, 
                                        Title = title)
            
        }) # still busy
        
    }) # clicked on button

        df1 = eventReactive(input$btn1, {
        id = which(jointdata[,which(colnames(jointdata)== input$genre)] == 1)
        joint1 = jointdata[id,]
        joint1 = joint1 %>% group_by(MovieID)%>%  
            summarise(ratings_per_movie = n(), ave_ratings = mean(Rating))%>% 
            arrange(desc(ratings_per_movie))
        movie_results = data.table(
            Rank = 1:10,
            MovieID = joint1$MovieID[1:10],
            Ratings_per_movie = joint1$ratings_per_movie[1:10])
        
    })
    
    output$results1 = renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df1()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])])
                    )
                    
                )        
            }))) # columns
        }) # rows
    })
    
    output$results2 <- renderUI({
        num_rows <- 2
        num_movies <- 5
        recom_result <- df2()
        
        lapply(1:num_rows, function(i) {
            list(fluidRow(lapply(1:num_movies, function(j) {
                box(width = 2, status = "success", solidHeader = TRUE, title = paste0("Rank ", (i - 1) * num_movies + j),
                    
                    div(style = "text-align:center", 
                        a(img(src = movies$image_url[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])], height = 150))
                    ),
                    div(style="text-align:center; font-size: 100%", 
                        strong(movies$Title[which(movies$MovieID == recom_result$MovieID[(i - 1) * num_movies + j])])
                    )
                    
                )        
            }))) # columns
        }) # rows
        
    }) # renderUI function
    
}

# Run the application 
shinyApp(ui = UI, server = Server)
