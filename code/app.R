#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

source("model/plant_class.R")
source("model/environment_run.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Recursive Learning in Plants - Some First modelling attempts"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("goal",
                        "Goal Function (koMt + (1-ko)St):",
                        min = 0,
                        max = 1,
                        value = 0.1),
            sliderInput("water",
                        "Starting Water Amount:",
                        min = 250,
                        max = 4000,
                        value = 1000),
            sliderInput("waterAve",
                        "Average Rainfall on wet days",
                        min = 5,
                        max = 100, 
                        value = 10),
            sliderInput("waterSd",
                        "Standard Deviation of rainfall on wet days",
                        min = 0,
                        max = 25,
                        value = 5),
            actionButton("goButton", "Go!")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Biomass", plotOutput("distPlot")),
                        tabPanel("Rainfall", plotOutput("rainfall"))
           
           # 
           # 
        )
    )
    
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    rain <- NULL;
    
    Tend = 150;

    rainfall <- reactive({ 
        
        water_ave <- input$waterAve;
        water_sd <- input$waterSd;
        
        
        MC_water <- matrix(c(0.35, 0.65, 0.75, 0.25), nrow = 2)
        colnames(MC_water) <- c('R', 'D')
        rownames(MC_water) <- c('R', 'D')
        
        generate_rainfall(Tend, MC_water,water_ave, water_sd)
        })
    
    
    
    
    output$distPlot <- renderPlot({
        
        input$goButton
        
        # if (input$goButton == 0)
            # return()
        
        
        tw = 14;
        
        # water_ave <- input$waterAve;
        # water_sd <- input$waterSd;
        
        
        
        rain <- rainfall()
        
        goal <- isolate(input$goal)
        W0 <- isolate(input$water)
        
        
        plant <- new_plant(W0 = W0);
        
        outputs <- run_model(plant,rain, tw, Tend, W0, goal)
        
        simlength = length(outputs$M)
        t_seq = 1:simlength
        
        out_df_1 <- data.frame(carbon_pool_type = as.factor(rep(c("Storage", "Biomass"), each = simlength)),
                               carbon_pool_val = c(outputs$S, outputs$M),
                               water_pool_val = rep(outputs$W, times = 2),
                               water_pool_type = as.factor(rep("Soil Water Available", times = simlength * 2)),
                               time = rep(t_seq, times = 2))
        coeff <- 0.4
        
        p1 <- ggplot(out_df_1, aes(x=time, y=carbon_pool_val)) +
            geom_line(mapping=aes(colour=carbon_pool_type), show.legend=TRUE) + 
            geom_line(mapping=aes(y=water_pool_val/coeff,
                                  fill = water_pool_type
                                  ), size=1.5, alpha=0.5, color = "blue") + 
            
            geom_hline(yintercept=0, color="#000000", size = 0.25,linetype="dashed") + 
            scale_y_continuous(
                limits = c(-625,4000),
                # Features of the first axis
                name = "Pool Size (gC)",
                # Add a second axis and specify its features
                sec.axis = sec_axis(~.*coeff, name = expression(paste("Available Water (kg", H[2], "O)")))
            ) +
            scale_color_hue("Carbon Pool", guide=guide_legend(order=1)) +
            scale_fill_manual(name  ="Water Pool", values = rep(1, 3),
                              guide=guide_legend(
                                  override.aes = list(colour=c("blue")) , order=2),
                              labels=c("Available Soil Water")) +
            
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
        
        p1
    })
    

    output$rainfall <- renderPlot({

        input$goButton
        
        if(!is.null(rain)){
            time = 1:length(rain)
            print(time)

            rain_df <- data.frame(time = time, rainfall = rain)

            p2 <- ggplot(rain_df, aes(x=time, y=rainfall)) +
                geom_bar(stat = "identity", fill="blue") +
                scale_y_continuous(
                    # Features of the first axis
                    name =  "Water Input [kgH20]"
                ) +
                theme_bw()
            p2
        }

    })
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
