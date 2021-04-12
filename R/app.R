library(shiny)

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "minty"),
  titlePanel("The DNA APP!"),
  textInput("dna", "Input DNA string:"),
  textOutput("dna_complement"),
  numericInput("length", "What's the length of the wanted random DNA sequence?",value = 0, min = 0),
  textOutput("dna_random"),
  textOutput("aa")
)

server <- function(input, output, session) {
  
  lookup <- c("A" = "T", "T" = "A", "G" = "C", "C" = "G") 
  dna_split <- reactive(strsplit(input$dna, "")[[1]])
  output$dna_complement <- renderText({paste0(lookup[dna_split()], collapse = "")
    })

  nucleotides <- reactive(sample(c("A", "T", "G", "C"), size = input$length, replace = TRUE))
  output$dna_random <- renderText({paste0(nucleotides(), collapse = "")
    })
  
  l = reactive(nchar(input$dna))
  codons <- reactive(substring(input$dna,
                             first = seq(from = 1, to = l()-3+1, by = 3),
                             last = seq(from = 3, to = l(), by = 3)))
  
  std_code_table <- c("TTT" = "F", "TCT" = "S", "TAT" = "Y", "TGT" = "C",
                      "TTC" = "F", "TCC" = "S", "TAC" = "Y", "TGC" = "C",
                      "TTA" = "L", "TCA" = "S", "TAA" = "*", "TGA" = "*",
                      "TTG" = "L", "TCG" = "S", "TAG" = "*", "TGG" = "W",
                      "CTT" = "L", "CCT" = "P", "CAT" = "H", "CGT" = "R",
                      "CTC" = "L", "CCC" = "P", "CAC" = "H", "CGC" = "R",
                      "CTA" = "L", "CCA" = "P", "CAA" = "Q", "CGA" = "R",
                      "CTG" = "L", "CCG" = "P", "CAG" = "Q", "CGG" = "R",
                      "ATT" = "I", "ACT" = "T", "AAT" = "N", "AGT" = "S",
                      "ATC" = "I", "ACC" = "T", "AAC" = "N", "AGC" = "S",
                      "ATA" = "I", "ACA" = "T", "AAA" = "K", "AGA" = "R",
                      "ATG" = "M", "ACG" = "T", "AAG" = "K", "AGG" = "R",
                      "GTT" = "V", "GCT" = "A", "GAT" = "D", "GGT" = "G",
                      "GTC" = "V", "GCC" = "A", "GAC" = "D", "GGC" = "G",
                     "GTA" = "V", "GCA" = "A", "GAA" = "E", "GGA" = "G",
                      "GTG" = "V", "GCG" = "A", "GAG" = "E", "GGG" = "G")
  output$aa <- renderText({paste0(std_code_table[codons()], collapse = "")})
 
}

shinyApp(ui, server)

