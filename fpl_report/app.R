#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(fplr)
library(tidyverse)
library(gridExtra)


gameweeks <- fpl_get_gameweeks()%>%
    select(id, average_entry_score)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("FPL 19/20 compare-inator"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput(inputId = "usr_id_1",
                        "FPL ID Mina (replace with any ID):", 
                          h3("User Id"), 
                          value = 20180),
            numericInput(inputId = "usr_id_2",
                         "FPL ID Ole Martin (replace with any ID):", 
                         h3("User Id"), 
                         value = 1615090),
            h5("Made in R by @thomastallaksen")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("rankPlot"),
           plotOutput("pointPlots"),
           plotOutput("meanPoints"),
           plotOutput("weekRank"),
           plotOutput("value"),
           plotOutput("transfers"),
           plotOutput("hits")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
 
    
    
    output$rankPlot <- renderPlot({

        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)
        
        rank <- user_1%>%
            left_join(user_2, by = "event")%>%
            select(event, overall_rank.x, overall_rank.y)%>%
            rename("Mina" = overall_rank.x, "Ole Martin" = overall_rank.y)%>%
            gather(key = "user", value = "rank", -event)
        
        # draw the plots
        ggplot(rank, aes(x=event, y=rank, colour = user))+
            geom_line(size = 2)+
            ggtitle("Overall rank")+
            scale_y_reverse()+
            scale_colour_manual(values=c("#75fb92", "#32043a"))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            ylab("Rank")+
            xlab("Gameweek")
        
    })
       
    output$pointPlots <- renderPlot({
        
        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)
        
        points <- user_1%>%
            left_join(user_2, by = "event")%>%
            select(event, points.x, points.y)%>%
            rename("Mina" = points.x, "Ole Martin" = points.y)%>%
            gather(key = "user", value = "points", -event)
        
        ggplot(points, aes(x=event, y=points, fill = user))+
            geom_bar(stat = "identity", position = "dodge")+
            ggtitle("Points per round", subtitle = NULL)+
            scale_fill_manual(values=c("#75fb92", "#32043a"))+
            geom_text(aes(label=points), vjust=2, colour="white", size=4, position = position_dodge(width = 0.9))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            xlab("Gameweek")+
            ylab("Points")

        
    })
    
    output$meanPoints <- renderPlot({
        
        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)
        
        mean_diff <- user_1%>%
            left_join(user_2, by = "event")%>%
            left_join(gameweeks, by = c("event" = "id"))%>%
            mutate("Mina" = points.x - average_entry_score)%>%
            mutate("Ole Martin" = points.y - average_entry_score)%>%
            select(event, "Mina", "Ole Martin")%>%
            gather(key = "user", value = "diff", -event)
            
        
        ggplot(mean_diff, aes(x=event, y=diff, fill = user))+
            geom_bar(stat = "identity", position = "dodge")+
            ggtitle("Points compared to average", subtitle = NULL)+
            geom_text(aes(y = diff - 2 * sign(diff), label=diff), colour="white", size=4, position = position_dodge(width = 0.9))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            scale_fill_manual(values=c("#75fb92", "#32043a"))+
            xlab("Gameweek")+
            ylab("Point difference")
        
        
        
    })

    output$weekRank <- renderPlot({
        
        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)
        
        rank <- user_1%>%
            left_join(user_2, by = "event")%>%
            select(event, rank.x, rank.y)%>%
            rename("Mina" = rank.x, "Ole Martin" = rank.y)%>%
            gather(key = "user", value = "rank", -event)
        
        # draw the plots
        ggplot(rank, aes(x=event, y=rank, colour = user))+
            geom_line(size = 2)+
            ggtitle("Gameweek rank")+
            scale_y_reverse()+
            scale_colour_manual(values=c("#75fb92", "#32043a"))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            ylab("Rank")+
            xlab("Gameweek")
        
    })
    
    
    output$hits <- renderPlot({
        
        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)
        
        points <- user_1%>%
            left_join(user_2, by = "event")%>%
            select(event, event_transfers_cost.x, event_transfers_cost.y)%>%
            rename("Mina" = event_transfers_cost.x, "Ole Martin" = event_transfers_cost.y)%>%
            gather(key = "user", value = "points", -event)%>%
            mutate(points = points/4)
        
        ggplot(points, aes(x=event, y=points, fill = user))+
            geom_bar(stat = "identity", position = "dodge")+
            ggtitle("Hits", subtitle = NULL)+
            scale_fill_manual(values=c("#75fb92", "#32043a"))+
            geom_text(aes(label=points), vjust=2, colour="white", size=4, position = position_dodge(width = 0.9))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            xlab("Gameweek")+
            ylab("Number of hits")
        
        
    })
    
    output$value <- renderPlot({
        
        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)
        
        rank <- user_1%>%
            left_join(user_2, by = "event")%>%
            select(event, value.x, value.y)%>%
            rename("Mina" = value.x, "Ole Martin" = value.y)%>%
            gather(key = "user", value = "value", -event)
        
        # draw the plots
        ggplot(rank, aes(x=event, y=value, colour = user))+
            geom_line(size = 2)+
            ggtitle("Team value")+
            scale_colour_manual(values=c("#75fb92", "#32043a"))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            ylab("Value")+
            xlab("Gameweek")
        
    })
    
    output$transfers <- renderPlot({
        
        user_1 <- fpl_get_user_current(user_id = input$usr_id_1)
        user_2 <- fpl_get_user_current(user_id = input$usr_id_2)

        transfers <- user_1%>%
            left_join(user_2, by = "event")%>%
            select(event, event_transfers.x, event_transfers.y)%>%
            rename("Mina" = event_transfers.x, "Ole Martin" = event_transfers.y)%>%
            gather(key = "user", value = "transfers", -event)
        
        ggplot(transfers, aes(x = event, y = transfers, fill = user))+
            geom_bar(stat = "identity", position = "dodge")+
            ggtitle("Number of transfers", subtitle = NULL)+
            scale_fill_manual(values=c("#75fb92", "#32043a"))+
            geom_text(aes(label=transfers), vjust=2, colour="white", size=4, position = position_dodge(width = 0.9))+
            theme(legend.title = element_blank(), legend.position = "bottom", legend.text=element_text(size=12))+
            xlab("Gameweek")+
            ylab("Number of transfers")
        
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
