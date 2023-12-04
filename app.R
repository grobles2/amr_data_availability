#App for visualising pathogen and breakdown in public datasets 
#library(shiny)
library(ggplot2)
library(ggforce)
#library(plotly)
library(scales)
library(dplyr)
library(DT)

resdata <- read.csv("table.csv", stringsAsFactors = FALSE) 
resdata$combo <- paste0(resdata$pathogen,"-",resdata$abx_class)
#rrdata <- read.csv("rrtable.csv", stringsAsFactors = FALSE) 

#UI is the end user interface
ui <- fluidPage(
  tabsetPanel(
   tabPanel("Prev_resistance: 1) covariates", 
             # Combo input
             selectInput("combo", "Bug-drug combos by pathogen:",
                         # c('escherichia_coli','klebsiella_pneumoniae','streptococcus_pneumoniae','staphylococcus_aureus',
                         # 'acinetobacter_baumanii','pseudomonas_aeruginosa','neisseria_gonorrheae','shigella_spp','salmonella_typhi','salmonella_paratyphi',
                         # 'serratia_spp','morganella_spp','haemophilus_influenzae','proteus_spp','citrobacter_spp','enterococcus_faecalis',
                         # 'enterococcus_faecium','enterococcus_spp','group_a_strep','group_b_strep','campylobacter'
                         # #                         c('acinetobacter_baumanii-anti_pseudomonal_penicillin',	'acinetobacter_baumanii-beta_lactamase_inhibitor',	'acinetobacter_baumanii-carbapenem',	'acinetobacter_baumanii-fourth_gen_ceph',	'acinetobacter_baumanii-third_gen_ceph',	'citrobacter_spp-aminoglycoside',	'citrobacter_spp-anti_pseudomonal_penicillin',	'citrobacter_spp-carbapenem',	'citrobacter_spp-fluoroquinolone',	'citrobacter_spp-fourth_gen_ceph',	'citrobacter_spp-third_gen_ceph',	'enterobacter_spp-aminoglycoside',	'enterobacter_spp-anti_pseudomonal_penicillin',	'enterobacter_spp-carbapenem',	'enterobacter_spp-fluoroquinolone',	'enterobacter_spp-fourth_gen_ceph',	'enterobacter_spp-sulfa',	'enterococcus_faecalis-fluoroquinolone',	'enterococcus_faecalis-vancomycin',	'enterococcus_faecium-fluoroquinolone',	'enterococcus_faecium-vancomycin',	'enterococcus_spp-fluoroquinolone',	'enterococcus_spp-vancomycin',	'escherichia_coli-aminoglycoside',	'escherichia_coli-aminopenicillin',	'escherichia_coli-beta_lactamase_inhibitor',	'escherichia_coli-carbapenem',	'escherichia_coli-fluoroquinolone',	'escherichia_coli-sulfa',	'escherichia_coli-third_gen_ceph',	'group_a_strep-macrolide',	'group_b_strep-fluoroquinolone',	'group_b_strep-macrolide',	'group_b_strep-penicillin',	'haemophilus_influenzae-aminopenicillin',	'haemophilus_influenzae-third_gen_ceph',	'klebsiella_pneumoniae-aminoglycoside',	'klebsiella_pneumoniae-beta_lactamase_inhibitor',	'klebsiella_pneumoniae-carbapenem',	'klebsiella_pneumoniae-fluoroquinolone',	'klebsiella_pneumoniae-sulfa',	'klebsiella_pneumoniae-third_gen_ceph',	'morganella_spp-fluoroquinolone',	'morganella_spp-fourth_gen_ceph',	'morganella_spp-third_gen_ceph',	'mycobacterium_tuberculosis-isoniazid_new',	'mycobacterium_tuberculosis-isoniazid_retreated',	'mycobacterium_tuberculosis-rifampicin_new',	'mycobacterium_tuberculosis-rifampicin_retreated',	'neisseria_gonorrheae-fluoroquinolone',	'neisseria_gonorrheae-third_gen_ceph',	'non_typhoidal_salmonellae-fluoroquinolone',	'proteus_spp-aminoglycoside',	'proteus_spp-aminopenicillin',	'proteus_spp-fluoroquinolone',	'proteus_spp-sulfa',	'proteus_spp-third_gen_ceph',	'pseudomonas_aeruginosa-aminoglycoside',	'pseudomonas_aeruginosa-anti_pseudomonal_penicillin',	'pseudomonas_aeruginosa-carbapenem',	'pseudomonas_aeruginosa-fluoroquinolone',	'pseudomonas_aeruginosa-fourth_gen_ceph',	'pseudomonas_aeruginosa-third_gen_ceph',	'salmonella_paratyphi-fluoroquinolone',	'salmonella_paratyphi-mdr',	'salmonella_typhi-fluoroquinolone',	'salmonella_typhi-mdr',	'serratia_spp-aminoglycoside',	'serratia_spp-anti_pseudomonal_penicillin',	'serratia_spp-carbapenem',	'serratia_spp-fluoroquinolone',	'serratia_spp-fourth_gen_ceph',	'serratia_spp-third_gen_ceph',	'shigella_spp-fluoroquinolone',	'staphylococcus_aureus-fluoroquinolone',	'staphylococcus_aureus-macrolide',	'staphylococcus_aureus-methicillin',	'staphylococcus_aureus-sulfa',	'staphylococcus_aureus-vancomycin',	'streptococcus_pneumoniae-beta_lactamase_inhibitor',	'streptococcus_pneumoniae-carbapenem',	'streptococcus_pneumoniae-fluoroquinolone',	'streptococcus_pneumoniae-macrolide',	'streptococcus_pneumoniae-penicillin',	'streptococcus_pneumoniae-sulfa',	'streptococcus_pneumoniae-third_gen_ceph'
c('acinetobacter_baumannii-beta_lactamase_inhibitor',
  'acinetobacter_baumannii-carbapenem',
  'acinetobacter_baumannii-fourth_gen_ceph',
  'acinetobacter_baumannii-third_gen_ceph',
  'acinetobacter_baumannii-aminoglycoside',
  'citrobacter_spp-aminoglycoside',
  'citrobacter_spp-anti_pseudomonal_penicillin',
  'citrobacter_spp-carbapenem',
  'citrobacter_spp-fourth_gen_ceph',
  'citrobacter_spp-third_gen_ceph',
  'enterobacter_spp-aminoglycoside',
  'enterobacter_spp-anti_pseudomonal_penicillin',
  'enterobacter_spp-carbapenem',
  'enterobacter_spp-fourth_gen_ceph',
  'enterococcus_faecalis-fluoroquinolone',
  'enterococcus_faecalis-vancomycin',
  'enterococcus_faecium-fluoroquinolone',
  'enterococcus_faecium-vancomycin',
  'escherichia_coli-aminoglycoside',
  'escherichia_coli-aminopenicillin',
  'escherichia_coli-beta_lactamase_inhibitor',
  'escherichia_coli-carbapenem',
  'escherichia_coli-sulfa',
  'group_a_strep-macrolide',
  'group_b_strep-fluoroquinolone',
  'group_b_strep-macrolide',
  'group_b_strep-penicillin',
  'haemophilus_influenzae-aminopenicillin',
  'haemophilus_influenzae-third_gen_ceph',
  'klebsiella_pneumoniae-aminoglycoside',
  'klebsiella_pneumoniae-beta_lactamase_inhibitor',
  'klebsiella_pneumoniae-fluoroquinolone',
  'klebsiella_pneumoniae-sulfa',
  'proteus_spp-aminoglycoside',
  'proteus_spp-third_gen_ceph',
  'pseudomonas_aeruginosa-aminoglycoside',
  'pseudomonas_aeruginosa-anti_pseudomonal_penicillin',
  'pseudomonas_aeruginosa-carbapenem',
  'pseudomonas_aeruginosa-fluoroquinolone',
  'pseudomonas_aeruginosa-fourth_gen_ceph',
  'pseudomonas_aeruginosa-third_gen_ceph',
  'serratia_spp-aminoglycoside',
  'serratia_spp-anti_pseudomonal_penicillin',
  'serratia_spp-carbapenem',
  'serratia_spp-fourth_gen_ceph',
  'serratia_spp-third_gen_ceph',
  'staphylococcus_aureus-fluoroquinolone',
  'staphylococcus_aureus-macrolide',
  'staphylococcus_aureus-sulfa',
  'streptococcus_pneumoniae-beta_lactamase_inhibitor',
  'streptococcus_pneumoniae-carbapenem',
  'streptococcus_pneumoniae-fluoroquinolone',
  'streptococcus_pneumoniae-macrolide',
  'streptococcus_pneumoniae-sulfa',
  'streptococcus_pneumoniae-third_gen_ceph',
  'escherichia_coli-fluoroquinolone',
  'escherichia_coli-third_gen_ceph',
  'klebsiella_pneumoniae-carbapenem',
  'klebsiella_pneumoniae-third_gen_ceph',
  'salmonella_paratyphi-fluoroquinolone',
  'salmonella_typhi-fluoroquinolone',
  'staphylococcus_aureus-methicillin',
  'streptococcus_pneumoniae-penicillin',
  'staphylococcus_aureus-vancomycin'),
                         selected =   'pseudomonas_aeruginosa-anti_pseudomonal_penicillin'),
            #Output
           # textOutput('summary'),
           #dataTableOutput('table'),
           htmlOutput('resistance_pdfs'),
    ),
   tabPanel("Relative risk: age cascade", 
            # Combo input
            selectInput("abxclass", "Relative risk data by combination:",
                        c('aminoglycoside','aminopenicillin','anti_pseudomonal_penicillin','beta_lactamase_inhibitor',
                          'carbapenem','fluoroquinolone','fourth_gen_ceph','mdr','macrolide','methicillin','penicillin','sulfa','third_gen_ceph','vancomycin'
                        ),
                        selected = "third_gen_ceph"), #"acinetobacter_baumanii-anti_pseudomonal_penicillin"),
            #Output
            htmlOutput('rr_pdfs'),
            dataTableOutput('tablerr'),
   )
  )
)

server <- function(input, output) {
# PLOTS FOR RESISTANCE TAB
  # We first observe and react to a change in the input$synd
   combo <- reactive({
     input$combo
   })
   abxclass <- reactive({
     input$abxclass
     
   })
   #Subset the data to create plots
      output$resistance_pdfs <- renderText({
        y <- combo()
        #y <- substring(combination,1,stringr::str_locate(combination,"-")-1)[1]
        #z <- substring(combination,stringr::str_locate(combination,"-")+1,)[1]
        #y<-ifelse(combination=="acinetobacter_baumannii","acinetobacter_baumanii",combination)
        pdf2 <- paste0(y,'.png')
        pdf1 <- paste0('st1_',y,'.pdf')
        #        pdf1 <- paste0(y,'_year_sr.pdf')
 #       pdf2 <- paste0(y,"_map.pdf")
        return(paste('<iframe style="height:900px; width:60%" src="', pdf1,'"></iframe>','<iframe style="height:700px; width:39%" src="', pdf2,'"></iframe>', sep = "\n"))
      })

      output$rr_pdfs <- renderText({
      y <- abxclass()
      rrcombos <- unique(resdata$combo[resdata$abx_class == y])
      n_rrcombos <- length(rrcombos)
      s <- list()
      for(i in 1:n_rrcombos) {
        s[i] <- paste0('st_',rrcombos[i],'.png') # title = "',rrcombos[i],'"')
      }
      textoreturn<-c()
      for(i in 1:n_rrcombos) {
        textoconcat <- paste0('<iframe style="height:450px; width:47%" src="',s[i],'"></iframe>', sep = "\n")
        textoreturn <- paste0(textoreturn,textoconcat)
      }
      #y <- substring(combination,1,stringr::str_locate(combination,"-")-1)[1]
      #z <- substring(combination,stringr::str_locate(combination,"-")+1,)[1]
      #y<-ifelse(combination=="acinetobacter_baumannii","acinetobacter_baumanii",combination)
  #    pdf1 <- paste0(y,'_ysr.pdf')
   #   pdf2 <- paste0(y,"_map.pdf")
      #return(paste('<iframe style="height:450px; width:95%" src="', pdf1,'"></iframe>','<iframe style="height:450px; width:95%" src="', pdf2,'"></iframe>', sep = "\n"))
      return(textoreturn) 
      }) 
      
     # output$table <- renderDataTable(resdata)
      #output$tablerr <- renderDataTable(rrdata)
      
      }

  shinyApp(ui = ui, server = server)

  