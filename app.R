library(shiny)

ui <- fluidPage(tags$head(tags$link(rel = "stylesheet",type = "text/css",href = "style.css")),
	tags$header(tags$div(class="titlebox",tags$h1("A computational hypothesis test"))),
	sidebarLayout(
		sidebarPanel(tags$div(class="box",tags$table(style="width:100%",tags$tr(tags$td(" "),tags$td(align="center","x data"),tags$td(" ")),
			tags$tr(tags$td(numericInput("x1",NULL,NA)),tags$td(numericInput("x2",NULL,NA)),tags$td(numericInput("x3",NULL,NA))),
			tags$tr(tags$td(numericInput("x4",NULL,NA)),tags$td(numericInput("x5",NULL,NA)),tags$td(numericInput("x6",NULL,NA))),
			tags$tr(tags$td(numericInput("x7",NULL,NA)),tags$td(numericInput("x8",NULL,NA)),tags$td(numericInput("x9",NULL,NA))),
			tags$tr(tags$td(numericInput("x10",NULL,NA)),tags$td(numericInput("x11",NULL,NA)),tags$td(numericInput("x12",NULL,NA))),
			tags$tr(tags$td(numericInput("x13",NULL,NA)),tags$td(numericInput("x14",NULL,NA)),tags$td(numericInput("x15",NULL,NA))),
			tags$tr(tags$td(numericInput("x16",NULL,NA)),tags$td(numericInput("x17",NULL,NA)),tags$td(numericInput("x18",NULL,NA))),
			tags$tr(tags$td(numericInput("x19",NULL,NA)),tags$td(numericInput("x20",NULL,NA)),tags$td(numericInput("x21",NULL,NA))),
			tags$tr(tags$td(" "),tags$td(align="center","y data"),tags$td(" ")),
			tags$tr(tags$td(numericInput("y1",NULL,NA)),tags$td(numericInput("y2",NULL,NA)),tags$td(numericInput("y3",NULL,NA))),
			tags$tr(tags$td(numericInput("y4",NULL,NA)),tags$td(numericInput("y5",NULL,NA)),tags$td(numericInput("y6",NULL,NA))),
			tags$tr(tags$td(numericInput("y7",NULL,NA)),tags$td(numericInput("y8",NULL,NA)),tags$td(numericInput("y9",NULL,NA))),
			tags$tr(tags$td(numericInput("y10",NULL,NA)),tags$td(numericInput("y11",NULL,NA)),tags$td(numericInput("y12",NULL,NA))),
			tags$tr(tags$td(numericInput("y13",NULL,NA)),tags$td(numericInput("y14",NULL,NA)),tags$td(numericInput("y15",NULL,NA))),
			tags$tr(tags$td(numericInput("y16",NULL,NA)),tags$td(numericInput("y17",NULL,NA)),tags$td(numericInput("y18",NULL,NA))),
			tags$tr(tags$td(numericInput("y19",NULL,NA)),tags$td(numericInput("y20",NULL,NA)),tags$td(numericInput("y21",NULL,NA))),
			tags$tr(tags$td(align="center",actionButton(inputId = "run",label = "Run",class='button')),tags$td(align="center",actionButton(inputId = "clear",label = "Clear",class='button')),tags$td(align="center",actionButton(inputId = "ex",label = "Example",class='button')))
			))),
		mainPanel(tags$div(class="box",
				tabsetPanel(tabPanel("Results",
					tags$table(style="width:100%",tags$tr(tags$td(align="center",imageOutput("hist", inline=TRUE))),
					tags$tr(tags$td(align="center",textOutput("m"))),
					tags$tr(tags$td(align="center",textOutput("pval")))
					)
				),
				tabPanel("Notes",tags$p(style="maintext","This app uses a simple Monte Carlo simulation method to apply a hypothesis 
					test to determine whether the y values come from a distribution with a greater average than that of the x values.  
					Each sample must include at least three values."),tags$p(style="maintext","If the null hypothesis is correct, the 
					values all come from the same distribution, and the assignment of each to x or y is effectively random.  By 
					repeatedly simulating the random reassignment of the data sets to x and y, with the size of x and y remaining 
					fixed, a distribution of simulated differences in medians is generated.  By comparing the observed difference
					in medians to this distribution, the p value for rejecting the null hypothesis is determined.  If the 
					alternative hypothesis is correct, the y values come from a distribution with a greater average.",
					tags$p(style="maintext","The key utility of this test is that it doesn't make any assumptions about the population
					distribution, calculating the p value directly without any intermediate parameters or formulas.")))
		))
))
)

server <- function(input, output, session) {
  png('hist.png')
  hist(c(-1,-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1.0),breaks=10,freq=FALSE,col='#FCFCFC',border='#FCFCFC',main="",xlab="Difference of medians",ylab="",bg='#FCFCFC',yaxt="n")
  dev.off()
  output$hist <- renderImage(list(src='hist.png'))
  deltamed <- function(v,n) { #assume input of a vector, and length of x vector (so first Nx elements are in x vector, remaining in y)
			return((median(v[(n+1):length(v)]) - median(v[1:n])))
	}
	shuffletest <- function(x,y){
		trials <- 50000 #minimum detectable p value is 1/trials
		Nx <- length(x)
		Ny <- length(y)
		z <- c(x,y)
		Nz <- Nx + Ny
		m <- deltamed(z,Nx)
		sims <-replicate(trials, sample(z), simplify=FALSE)
		M <- mapply(deltamed,sims,n=Nx)
		test <- list("pval"=sum(M>=m)/trials, "m"=m)
		png('hist.png')
		hist(M,breaks=50,freq=FALSE,col="#00144d",main="Difference of medians",xlab="",ylab="",bg='#FCFCFC',yaxt="n")
		points(c(m),c(0),pch=19,col="#990000")
		dev.off()
		return(test)
	}
	xIds <- list("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21")
	yIds <- list("y1","y2","y3","y4","y5","y6","y7","y8","y9","y10","y11","y12","y13","y14","y15","y16","y17","y18","y19","y20","y21")
	observeEvent(input$run, {
		x_inputs <- c(input$x1,input$x2,input$x3,input$x4,input$x5,input$x6,input$x7,input$x8,input$x9,input$x10,input$x11,input$x12,input$x13,input$x14,input$x15,input$x16,input$x17,input$x18,input$x19,input$x20,input$x21)
		x_inputs <- x_inputs[!is.na(x_inputs)]
		y_inputs <- c(input$y1,input$y2,input$y3,input$y4,input$y5,input$y6,input$y7,input$y8,input$y9,input$y10,input$y11,input$y12,input$y13,input$y14,input$y15,input$y16,input$y17,input$y18,input$y19,input$y20,input$y21)
		y_inputs <- y_inputs[!is.na(y_inputs)]
		if (length(x_inputs) > 2 & length(y_inputs) > 2) {
			outputs <- shuffletest(x_inputs,y_inputs)
			output$hist <- renderImage(list(src='hist.png'))
			output$m <- renderText(paste('Observed difference of medians: ',round(outputs[['m']],digits=5)))
			output$pval <- renderText(paste('p value: ',outputs[['pval']]))
		}
	})
	observeEvent(input$clear, {
		for (x in xIds) {
			updateNumericInput(session,x,value=NA)
		}
		for (y in yIds) {
			updateNumericInput(session,y,value=NA)
		}
	})
		observeEvent(input$ex, {
		for (x in xIds) {
			updateNumericInput(session,x,value=NA)
		}
		for (y in yIds) {
			updateNumericInput(session,y,value=NA)
		}
		updateNumericInput(session,"x1",value=6)
		updateNumericInput(session,"x2",value=7.5)
		updateNumericInput(session,"x3",value=7)
		updateNumericInput(session,"x4",value=7.25)
		updateNumericInput(session,"y1",value=7.3)
		updateNumericInput(session,"y2",value=7.2)
		updateNumericInput(session,"y3",value=7.15)
		updateNumericInput(session,"y4",value=7.07)
		updateNumericInput(session,"y5",value=7.19)
		updateNumericInput(session,"y6",value=7.22)
		updateNumericInput(session,"y7",value=7.23)
		updateNumericInput(session,"y8",value=7.16)
	})

}

shinyApp(ui = ui, server = server)