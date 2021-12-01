devtools::install_github("jbergenstrahle/STUtility")

library(shinydashboard)
library(STutility)
library(dashboardthemes)
library(tidyverse)
library(RCurl)
library(EnhancedVolcano)
library(tidyverse)
library(Seurat)
library(DT)

load('app_required.Rdata')


#d = data[[1]]

#text = read.delim('E:/stAMD/Visualisation/AMD.txt')

header <- dashboardHeader(title = "ST-AMD")

sidebar <- dashboardSidebar(sidebarMenu(
    menuItem("Introduction",
             icon = icon("bookmark"),
             tabName = "dashboard"    ),
    menuItem('Experiment setups', 
             icon = icon('flask'), 
             tabName = 'setup'),
    menuItem("Spatial transcriptomic results",
             icon = icon("eye"),
             tabName = "spatial") , 
    
    menuItem('Where are the clusters?', 
             icon = icon('question'), 
             tabName = 'ST'
    ),
    menuItem(text = 'Spatial Differential expression', 
             icon = icon('balance-scale-right'), 
             tabName = 'DE'), 
    menuItem(text = 'Compare Differential expression', 
             icon = icon('dna'), 
             tabName = 'compare') 
    
)


)

body <- dashboardBody(
    #  shinyDashboardThemes( theme = "onenote"),
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #fff;
      }
    '
    ))),
    tabItems(
           tabItem(tabName = "dashboard",
                     box(width = 12, title = h3('Using Spatial transcriptomics to underly the molecular disparity in age-related macular degeneration (AMD) mice eye'), icon = icon("dashboard")) ,
                     fluidRow(box(width = 6, title = h3('What is AMD?'),'Age-related Macular Degeneration (AMD) affects the macular region in the retina, which is responsible for providing humans with controls acuteness and colour visionhigh visual acuity and colour vision (van Lookeren Campagne et al., 2014). The early and intermediate stage of AMD is characterised by 1) inflammation in the outer retina with activated microglia recruitment, 2) Drusen accumulation between retinal pigmented epithelium (RPE) and Brunch’s membrane, and 3) less compressed choriocapillaris layer (Fig. 1). Mild vision impairment shows in the patients with early and intermediate AMD. Some intermediate AMD will develop into two advanced forms, the “dry” atrophic form (geographic atrophy) and thean “wet” neovascular form (choroidal neovascularisation). AThe astrophic AMD coupled with photoreceptor and RPE degeneration is most prevalent, and it is the leading cause for blindness worldwide in the developed world (Schuman et al., 2009). Although both oxidaiitve stress and inflammatory pathways have been associated with AMD, However, thethe specific pathophysiology and driver molecular pathways that controlsdrive AMD progression haves not yet been fully understood yet. Additionally, treatments are currently unavailable for the dry form of atrophic AMD (van Lookeren Campagne et al., 2014).  Characterisinge the specific gene expression patterns in retinal cell populations during degeneration sheds lights into the understanding the molecular mechanisms and may help identify potential therapies (Voigt et al., 2019). '),
                              box(width = 6, img(src="https://openophthalmologyjournal.com/contents/volumes/V13/TOOPHTJ-13-90/TOOPHTJ-13-90_F3.jpg", width = 600))
                     ), 
                     fluidRow(box(width = 6, title = h3('What is Spatial transcriptomics?'), 'Spatial transcriptomics: The innovative ST method has been awarded Method of Year 2020 by Nature Method (Marx, 2021). While scRNAseq gives a cell specific transcriptome profile (Marx, 2021), the spatial information of cell origin within the highly structured retina is lost. The ST method profiles the transcriptome of fixed tissues in situ, utilising barcoded spots on a microscope slide (Fig. 2). Therefore, the technology enables the comparison of transcriptional profiles retaining the spatial information (Asp et.al., 2020). We aim to gain a morphological understanding of transcriptomic variations between the normal and AMD retina. We hypothesise that the ST analysis can not only elucidate the transcriptomic variation of cellular responses to light damage, but also reveals the morphological distinction of cell layers which are responsible for the pathophysiology of AMD. '),
                              box(width = 6, img(src = 'https://ngisweden.scilifelab.se/wp-content/uploads/2021/02/Visium-gene-expression-slide-768x292.png', width = 600))
                     ) ),
             
             tabItem(tabName = "spatial",
                     column(width = 12, sidebarPanel(selectInput( 'Sample', 
                                                                  label = 'Please Enter your sample ID of interest:', 
                                                                  choices = c(1:8)))), 
                     fluidRow(
                         column(width = 6, plotOutput("UMAP", width = '800px', height = '600px')),
                         column(width =6, plotOutput("Spatial", width = '800px', height = '600px'))
                         
                     )
             ),
             tabItem(tabName = 'ST', 
                     column(width = 12, selectInput('clusters',  
                                                    label = 'Please select your cluster of interest:', 
                                                    choices = levels(se) )), 
                     column(width = 12, plotOutput('ST', width = '1500px', height = '600px'))
                     
             ),
             tabItem(tabName = 'DE', 
                     fluidRow(column(width = 6,  selectInput(
                         'Sample', 
                         label = 'Please Enter your sample ID of interest:', 
                         choices = c(1:8)
                     ) ), 
                     column(width = 6,                      
                            # 这里是定义一个slider 作为输入
                            textInput(inputId = "GeneID",
                                      label = "Please enter your gene symbol of interest:",
                                      value = 'Rho')) , 
                     #                    fluidRow( splitLayout(cellWidths = c("600px", "800px"), 
                     #                                          plotOutput('distPlot_DE', width = '600px', height = '600px'),
                     #                                          plotOutput('UMAP_DE', width = '800px', height = '600px'))), 
                     fluidRow(  column(width = 12, plotOutput('VlnPlot_DE', width = '100%', height = '500px'))), 
                     fluidRow( column(width = 6,  plotOutput('distPlot_DE', width = '500px', height = '500px')),
                               column(width = 6, plotOutput('UMAP_DE', width = '700px', height = '500px')) )
                     ), 
                     
             )
             , 
             tabItem(tabName = 'compare', 
                     fluidRow(column(width = 3,       selectInput('Celltypes',
                                                                  label = 'Celltypes',
                                                                  selected  = cell_type[1],
                                                                  choices = unique(cell_type)) ) ,
                              column(width = 3,  selectInput('Conditions', 
                                                             label = 'condition',
                                                             selected = conditions[1],
                                                             choices = unique(conditions)) ),
                              column(width = 3, sliderInput(inputId = "fc_cutoff",
                                                            label = "Fold-change cutoffs:",
                                                            min = 0,
                                                            max = 3,
                                                            value = 0.8, 
                                                            step = 0.1) ),
                              column(width = 3, sliderInput(inputId = "p_cutoff",
                                                            label = "P_value cutoffs:",
                                                            min = 0,
                                                            max = 0.2,
                                                            value = 0.05, 
                                                            step = 0.01))), 
                     fluidRow(
                         splitLayout(cellWidths = c("600px", "500px"), 
                                     plotOutput(outputId = "Volc",    width = '600px',
                                                height = "600px", click = "plot_click"), 
                                     plotOutput("Vln", width = '500px', height = '600px')
                         )
                     )
                     # Output: Histogram ----
                     ,
                     tableOutput("print"),
                     
                     fluidRow(
                         column(12,
                                DTOutput('table')
                         ))
                     
                     
             )
    )
)

ui <-  dashboardPage(skin = 'green', header, sidebar, body)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$Spatial <- renderPlot({
        
        FeatureOverlay(se, features = 'labels', 
                       sampleids = input$Sample, 
                       ncols = 1, pt.size = 1.25, cols = colset)
    })
    
    output$UMAP <- renderPlot({
        DimPlot(se, reduction = "umap", label = F, pt.size = 1, cols = colset) 
    })
    output$distPlot_DE <- renderPlot({
        
        FeatureOverlay(se, features = input$GeneID, 
                       sampleids = input$Sample, 
                       ncols = 1, pt.size = 1.25)
    })
    output$VlnPlot_DE <- renderPlot({
        
        plot = VlnPlot(se[, se$section == paste0('section_', input$Sample)], features = input$GeneID, cols = colset)
        plot + theme(axis.text.x = element_text(angle = 90))
    })
    output$UMAP_DE <- renderPlot({
        
        FeatureOverlay(se, features = 'labels', 
                       sampleids = input$Sample, 
                       ncols = 1, pt.size = 1.25, cols = colset)
    })
    
    output$Volc <- renderPlot({
        fc_cutoff  = input$fc_cutoff
        p_cutoff = input$p_cutoff
        dname = paste0(input$Celltypes, ".", input$Conditions)
        index = as.integer(which(names == dname))
        d = data[[index]]
        colnames(d) <- c( "log2FoldChange", "AveExpr", "t", "pvalue" ,"padj", "B" )
        margin(t = 3, r = 3, b = 3, l = 3, unit = "pt")
        EnhancedVolcano(d,
                        lab = rownames(d),
                        x = 'log2FoldChange',
                        y = 'pvalue',
                        xlim = c(-3, 3),
                        #NAMES:   ==================
                        pCutoff = p_cutoff*10,
                        FCcutoff = fc_cutoff,
                        pointSize = 3.0,
                        col=c('black', 'black', 'blue', 'red3'), 
                        labSize = 4.5, 
                        title = dname, 
                        legendPosition = "none", 
                        border = "full" 
        )  +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
        
    })
    
    output$print = renderTable({
        nearPoints(
            d,             # the plotting data
            input$plot_click,   # input variable to get the x/y coordinates from
            xvar = 'logFC', 
            yvar = 'log10p',
            maxpoints = 1,      # only show the single nearest point 
            threshold =  1000  # basically a search radius. set this big enough 
            # to show at least one point per click
        )
    })
    
    output$Vln = renderPlot({
        pt = 
            nearPoints(
                d,             # the plotting data
                input$plot_click,   # input variable to get the x/y coordinates from
                xvar = 'logFC', 
                yvar = 'log10p',
                maxpoints = 1,      # only show the single nearest point 
                threshold =  1000  # basically a search radius. set this big enough 
                # to show at least one point per click
            )
        id =as.character(pt$GeneID)
        VlnPlot(se, features = id , group.by = 'setup')
    })
    
    output$table <- renderDT(d,
                             filter = "top",
                             options = list(
                                 pageLength = 5
                             ))
    #PAGE ST
    output$ST <- renderPlot({
        se$selected_clusters = ifelse(se$labels == input$clusters, 'selected', 'unselected' )
        cols_up =c('white', 'red')
        names(cols_up) = c('unselected', 'selected')
        FeatureOverlay(se, features = "selected_clusters", sampleids = 1:8, ncols = 4, pt.size = 1.25, cols= cols_up )
        
    })
    
}

shinyApp(ui, server)
