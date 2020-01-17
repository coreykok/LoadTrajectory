##########################################################################################
# 
# Plots a particular consumption trajectory based on where it ranks in a particular statistic indicating "High Usage" 
# 
# User Input:
# Rank: Which statistic will be used to rank each sample
# perc: Which percentile of the statistic should be plotted
#
# Input Files/Folers:
# TC.rds: Simulation of the total consumption by resampling, attaching the domination rank to the sample number
# 
# Output Files:
# savefile1: Plot of the overall consumption in the selected sample
# savefile2: Plot of the selected samples total consumption in each subgroup
# 
##########################################################################################

library(RColorBrewer) # Alternative colour palette
library(shiny) # Interactive plotting
library(shinyWidgets) # Interactive plotting
library(ggplot2) # ggplot plotting package
library(scales) # Scales data (for clustering purposes)

# Loads consumption data and defines statistics that may be used to rank samples
TotalConsumption = readRDS("data/Simulation/TC.rds")
RankList = sort(unique(TotalConsumption$Rank))
TotalConsumption$DomCount = floor(TotalConsumption$DomCount)
RankMethodList = c("Total Consumption", "Peak Consumption", "Minimum Consumption", "Dominance Rank")
SaveFileList = c("tot","peak","min","dom")

ui <- fluidPage(
    titlePanel(title=h4("Plot specific consumption trajectories from simulation", align="center")),
    sidebarPanel(
        helpText("Select ranking method and percentile to plot."),
        radioButtons(inputId = "rank", # Ranking method
                     label = "Rank Consumers by:",
                     choices = list("Total Consumption" = 1,"Peak Consumption" = 2, "Minimum Consumption" = 3, "Dominance Rank" = 4),
                     selected = 1),
        sliderTextInput(inputId = "perc", # Percentile to be plotted
                        label = paste("Percentile:"),
                        choices = 0:100,
                        selected  = 50),
        actionButton("execbut", "Plot Trajectory")
    ),
    mainPanel(
        tabsetPanel(
            tabPanel("Overall Consumption",  plotOutput("PercentilePlot")), 
            tabPanel("Consumption by Cluster", plotOutput("PercentilePlot2"))
        )
    )
)

server <- function(input,output,session){
    session$onSessionEnded(function() {
        stopApp()
    })
    outputPlot <- eventReactive(input$execbut,{
        
        # Processes data, specifies plot titles and save file names based on user input
        TotalConsumption$Date = as.POSIXct(TotalConsumption$Date) 
        TotalConsumption.sum = aggregate(Consumption ~ Date + Rank + DomCount + Iteration, TotalConsumption, FUN = sum)
        Title = paste0(input$perc, "th Percentile According to ", RankMethodList[as.numeric(input$rank)])
        savefile1 = paste0("data/Output/1_perc_",input$perc,"_",SaveFileList[as.numeric(input$rank)],".pdf") # Changed save file name based on statistic and rank method
        savefile2 = paste0("data/Output/2_perccat_",input$perc,"_",SaveFileList[as.numeric(input$rank)],".pdf") 
        
        
        # Assigns ranks based on the user specified statistic 
        if (input$rank == 4) {
            TotalConsumption$Perc = factor(TotalConsumption$Rank)
        } else if (input$rank == 1) {
            TotalConsumption.sum.stat = aggregate(Consumption ~ Iteration, TotalConsumption.sum, FUN = sum)
            TotalConsumption.sum.stat$Perc = rank(TotalConsumption.sum.stat$Consumption)
            TotalConsumption = merge(TotalConsumption, TotalConsumption.sum.stat[,c("Iteration","Perc")], by = "Iteration", all.x = TRUE)
        } else if (input$rank == 2) {
            TotalConsumption.sum.stat = aggregate(Consumption ~ Iteration, TotalConsumption.sum, FUN = max)
            TotalConsumption.sum.stat$Perc = rank(TotalConsumption.sum.stat$Consumption)
            TotalConsumption = merge(TotalConsumption, TotalConsumption.sum.stat[,c("Iteration","Perc")], by = "Iteration", all.x = TRUE)    
        } else if (input$rank == 3) {
            TotalConsumption.sum.stat = aggregate(Consumption ~ Iteration, TotalConsumption.sum, FUN = min)
            TotalConsumption.sum.stat$Perc = rank(TotalConsumption.sum.stat$Consumption)
            TotalConsumption = merge(TotalConsumption, TotalConsumption.sum.stat[,c("Iteration","Perc")], by = "Iteration", all.x = TRUE)    
        }
        
        # Extracts the rank specifed by the user input
        TotalConsumption.sub = TotalConsumption[TotalConsumption$Perc == max(round(input$perc * max(RankList) / 100),1),]
        TotalConsumption.sub.sum = aggregate(Consumption ~ Date + Perc + Iteration, TotalConsumption.sub, FUN = sum)       
        
        # Plot of overall consumption
        dataOutput = matrix(list(), 2)
        dataOutput[[1]] = ggplot(TotalConsumption.sub.sum,aes(x=Date, y=Consumption)) + 
            geom_line(size = 1.1) +
            geom_blank(data = TotalConsumption.sum, aes(x=Date, y = Consumption * 1.15), inherit.aes = FALSE) +
            expand_limits(y = 0) +
            scale_y_continuous("Consumption (kWh)", breaks = pretty_breaks()) +
            scale_x_datetime("Date", breaks =  pretty_breaks()) +
            ggtitle(Title) +
            theme(text=element_text(size=21))
        ggsave(savefile1, plot =  dataOutput[[1]] )
        
        # Plot of total consumption in each cluster 
        dataOutput[[2]] = ggplot(TotalConsumption.sub,aes(x=Date, y=Consumption)) + 
            geom_line(size = 1.1) +
            geom_blank(data = TotalConsumption, aes(x=Date, y = Consumption * 1.15), inherit.aes = FALSE) +
            expand_limits(y = 0) +
            scale_y_continuous("Consumption (kWh)") +
            scale_x_datetime("Date") +
            facet_grid(Cluster~., scales="free_y") + 
            ggtitle(Title) +
            theme(text=element_text(size=21))
        ggsave(savefile2, plot = dataOutput[[2]])
        return(dataOutput)
    })
    output$PercentilePlot<-renderPlot({ outputPlot()[[1]] }, height = 400, width = 700)
    output$PercentilePlot2<-renderPlot({ outputPlot()[[2]] }, height = 800, width = 700)
}
shinyApp(ui = ui, server = server)

