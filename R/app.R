#' SCRAD v0.1
#'
#' A shiny app to visualizate the risk of SNPs associated with five types of dementia
#'
#' @return
#' A shiny app to visualization
#'
#' @import
#' shiny
#'
#' @import
#' ggplot2
#'
#' @import
#' dplyr
#'
#' @import
#' DT
#'
#'
#' @examples
#' app_SCRAD()
#'
#' @export app_SCRAD
app_SCRAD <- function() {


  Tipos_SNPs<-Data_dementia$SNPS
  Tipos_demencia<-Data_dementia$Class
  Cells<-Data_dementia$Class
  # Define UI for application that draws a histogram
  ui <- fluidPage(

    # Application title
    titlePanel("SNP-Cell CRAD v0.2"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        # Text instructions
        HTML(paste("Ingresa los SNPs a evaluar, hay un total de: ", nrow(Data_dementia),"SNPs asociados a Demencia")),

        # Break for visual separation
        br(), br(), br(),


        selectInput(inputId = "SNPS",
                    label = "Selecciona los SNPs:",
                    choices = Tipos_SNPs,
                    selected = "rs10792830",
                    selectize=T,
                    multiple=T),

        selectInput(inputId = "population",
                    label = "Poblaciones continentales:",
                    choices=colnames(Riesgo[,8:13]),
                    selected="Probabilidad_Global"),

        selectInput(inputId = "dementia",
                    label = "Tipo de Demencia:",
                    choices=levels(Tipos_demencia),
                    selected="AD"),

        # Select filetype
        radioButtons(inputId = "filetype",
                     label = "Tipo de archivo:",
                     choices = c("csv", "tsv"),
                     selected = "csv"),
        br(),br(),

        HTML(paste("Cell_Score: Valor de probabilidad de asociacion entre gen y tipo celular de la unidad neurovascular,
                 calculado con Machine Learning")),
        br(),br(),
        HTML(paste("Dementia_Score: Valor de probabilidad de asociacion entre SNP y tipo de Demencia,
                 calculado con Machine Learning"))




      ),

      # Show a plot of the generated distribution
      mainPanel(
        shiny::dataTableOutput(outputId = "dementiatable"),
        br(),br(),br(),
        textOutput(outputId = "population"),
        tags$head(tags$style("#population{color: black;
                                 font-size: 20px;
                                 font-style: bold;
                                 }")),
        br(),br(),
        HTML("Selecciona el tipo de archivo y las variables, despues haz clic en 'Download data'."),
        br(), br(), # line break and some visual separation
        downloadButton("download_data", "Download data"),
        br(),br(), br(),br(),
        plotOutput(outputId = "Cell_var"),
        br(),br(), br(),br(),
        plotOutput(outputId = "pop_var")
      )
    )
  )

  # Define server logic required to draw a histogram
  server <- function(input, output) {

    output$dementiatable <- shiny::renderDataTable({
      req(input$SNPS)
      risk_from_selected_SNPs <- Riesgo %>% filter(Class %in%input$dementia)%>%
        filter(SNPS %in% input$SNPS) %>%
        select(SNPS:Dementia_Score,Cell,Cell_Score, input$population)
      DT::datatable(data = risk_from_selected_SNPs,
                    options = list(pageLength = 10),
                    rownames = FALSE)
    })

    output$population<-renderText({
      risk <- Riesgo %>% filter(Class %in%input$dementia)%>%
        filter(SNPS %in% input$SNPS) %>%
        select(input$population)
      risk<-sum(risk)*0.5
      paste0("Probabilidad de desarrollar demencia tipo ", input$dementia," es de: ",risk)
    })

    # Download file
    output$download_data <- downloadHandler(
      filename = function() {
        paste0("SCRAD_v0_2.", input$filetype)
      },
      content = function(file) {
        if(input$filetype == "csv"){
          write.csv(Data_dementia %>% filter(Class %in%input$dementia)%>%
                      filter(SNPS %in% input$SNPS) %>%
                      select(SNPS:Dementia_Score,Cell,Cell_Score, input$population), file)
        }
        if(input$filetype == "tsv"){
          write.table(Data_dementia %>% filter(Class %in%input$dementia)%>%
                        filter(SNPS %in% input$SNPS) %>%
                        select(SNPS:Dementia_Score,Cell,Cell_Score, input$population), file)
        }
      }
    )

    output$Cell_var<-renderPlot({
      risk_from_selected_SNPs <- Data_dementia %>% filter(Class %in%input$dementia)%>%
        filter(SNPS %in% input$SNPS) %>%
        select(SNPS:Dementia_Score,Cell,Cell_Score, input$population)
      ggplot(risk_from_selected_SNPs, aes(x = MAPPED_GENE, fill=Cell)) +
        geom_bar(color="Black") +
        theme_bw()+
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.border = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size =20 , family="Arial"),
              axis.text.x=element_text(colour="black", size = 11),
              axis.text.y=element_text(colour="black", size = 20))+
        ylab("SNPs") +
        xlab("Genes")+ labs(fill="Cell type") +
        #Para girara el rotulo
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })

    output$pop_var<-renderPlot({

      risk <- Riesgo %>% filter(Class %in%input$dementia)%>%
        filter(SNPS %in% input$SNPS) %>% select(Probabilidad_Global:Probabilidad_SAS)

      Africa<-data.frame(Prob=sum(risk$Probabilidad_AFR)*0.5)
      AMR<-data.frame(Prob=sum(risk$Probabilidad_AMR)*0.5)
      EAS<-data.frame(Prob=sum(risk$Probabilidad_EAS)*0.5)
      EUR<-data.frame(Prob=sum(risk$Probabilidad_EUR)*0.5)
      SAS<-data.frame(Prob=sum(risk$Probabilidad_SAS)*0.5)
      Pop<-rbind(Africa,AMR,EAS,EUR,SAS)

      vec<-c("Africa","America","Asia del Este","Europa","Asia del Sur")

      Pop_var<-data.frame(Prob=Pop, Popu=vec)

      ggplot(Pop_var, aes(x = Popu, y=Prob, fill=Popu)) +
        geom_col(color="Black") + geom_text(aes(label=Prob), vjust=-0.5)+
        theme_bw()+
        theme(axis.line.x = element_line(size = 0.5, colour = "black"),
              axis.line.y = element_line(size = 0.5, colour = "black"),
              axis.line = element_line(size=1, colour = "black"),
              panel.border = element_blank(),
              plot.title=element_text(size = 20, family="Arial"),
              text=element_text(size =20 , family="Arial"),
              axis.text.x=element_text(colour="black", size = 15),
              axis.text.y=element_text(colour="black", size = 15))+
        ylab("Prob. Demencia") +
        xlab("Poblacion")+ labs(fill="Poblacion") +
        #Para girara el rotulo
        theme(axis.text.x = element_text(angle = 90, hjust = 1))
    })

  }

  # Run the application
  shinyApp(ui = ui, server = server)

}
