#Test version: image binarization

library(shiny)
library(png)
library(shinyjs)
library(jpeg)
library(tools)
library(EBImage)

jsCode <- 'shinyjs.getPixelColor = function(params){
            setTimeout(
                  function(){
                        var img = document.getElementById("binaryImage").childNodes[0];
                        var canvas = document.createElement("canvas");
                        canvas.setAttribute("id", "canv");
                        canvas.width = img.width;
                        canvas.height = img.height;
                        canvas.getContext("2d").drawImage(img, 0, 0, img.width, img.height);
                        img.remove();
                        var i = 0;
                        document.getElementById("binaryImage").appendChild(canvas);
                        var imgData = canvas.getContext("2d").getImageData(0,0,canvas.width, canvas.height).data;
                        canvas.onclick = function(e){
                              var pixel = canvas.getContext("2d").getImageData(e.offsetX, e.offsetY, 1, 1).data;
                              //Picking skin color
                              if(i == 0){
                                    Shiny.onInputChange("skinR", pixel[0]);
                                    Shiny.onInputChange("skinG", pixel[1]);
                                    Shiny.onInputChange("skinB", pixel[2]);
                              }
                              //Picking background color
                              if(i == 1){
                                    Shiny.onInputChange("bgR", pixel[0]);
                                    Shiny.onInputChange("bgG", pixel[1]);
                                    Shiny.onInputChange("bgB", pixel[2]);
                              }
                              //Picking coin color and coordinates
                              if(i == 2){
                                    Shiny.onInputChange("coinR", pixel[0]);
                                    Shiny.onInputChange("coinG", pixel[1]);
                                    Shiny.onInputChange("coinB", pixel[2]);
                                    Shiny.onInputChange("coinX", e.offsetX);
                                    Shiny.onInputChange("coinY", e.offsetY);
                              }
                              //Picking disease color and coordinates
                              if(i == 3){
                                    Shiny.onInputChange("disR", pixel[0]);
                                    Shiny.onInputChange("disG", pixel[1]);
                                    Shiny.onInputChange("disB", pixel[2]);
                                    Shiny.onInputChange("disX", e.offsetX);
                                    Shiny.onInputChange("disY", e.offsetY);
                              }               
                              i++;
                        }
                  }, 500);
      }

      shinyjs.deleteCanv = function(params){
            var canvas = document.getElementById("canv");
            canv.remove();
      }'

ui <- fluidPage(
      
      useShinyjs(),
      
      extendShinyjs(text = jsCode, functions = c("getPixelColor", "deleteCanv")),
      
      titlePanel("Skin Disease Area Calculator"),
      
      sidebarLayout(
            
            sidebarPanel(
                  
                  # Allows users to change threshold for different images
                  sliderInput(inputId = "threshold",
                              label = "Threshold for image binarization",
                              min = 1,
                              max = 10000,
                              value = 3000),
                  
                  # Image upload
                  fileInput(inputId = "diseaseImage",
                            label = NULL,
                            width = '100%',
                            buttonLabel = "Upload Image"),
                  
                  actionButton("conversion", 'Convert!')
                  
            ),
            
            mainPanel(
                  
                  imageOutput("binaryImage")
                  
            )
      )
)

server <- function(input, output, session){
      
      output$binaryImage = renderImage({
            
            list(src = input$diseaseImage$datapath)
            
      }, deleteFile = F)
      
      observeEvent(input$diseaseImage,{
            js$getPixelColor()
      })
      
      observeEvent(input$conversion,{
            js$deleteCanv()
            if(is.null(input$skinR)){
                  alert("Select skin color")
            }
            else if(is.null(input$bgR)){
                  alert("Select background color")
            }
            else{
                  newPath = paste("copy_", input$diseaseImage$name, sep = "")
                  file.copy(input$diseaseImage$datapath, newPath)
                  if(file_ext(newPath) == "jpg"){
                        img = readJPEG(newPath)
                  }
                  else if(file_ext(newPath) == "png"){
                        img = readPNG(newPath)
                  }
                  else{
                        alert("Wrong file extension")
                  }
                  R = input$skinR
                  G = input$skinG
                  B = input$skinB
                  R1 = input$bgR
                  G1 = input$bgG
                  B1 = input$bgB
                  rs = img[,,1]*255
                  gs = img[,,2]*255
                  bs = img[,,3]*255
                  rdiff = (rs-R)^2
                  gdiff = (gs-G)^2
                  bdiff = (bs-B)^2
                  metric = rdiff + gdiff + bdiff
                  #Finding skin based on color difference and averaging skin color
                  binary = ifelse(metric < input$threshold, 1, 0)
                  R = mean(rs[binary>0])
                  B = mean(bs[binary>0])
                  G = mean(gs[binary>0])
                  rdiff = (rs-R)^2
                  gdiff = (gs-G)^2
                  bdiff = (bs-B)^2
                  metric = rdiff + gdiff + bdiff
                  #Finding skin based on mean skin color
                  binary = ifelse(metric < input$threshold, 1, 0)
                  rdiff = (rs-R1)^2
                  gdiff = (gs-G1)^2
                  bdiff = (bs-B1)^2
                  metric = rdiff + gdiff + bdiff
                  #Finding background 
                  binary1 = ifelse(metric < input$threshold, 1, 0)
                  res = ifelse(binary1 == 0 & binary == 0, 1, 0)
                  #Getting pixels that are neither skin nor background, smooting and filling thus finidng disease
                  res = medianFilter(res, input$radius)
                  res = floodFill(res, c(input$disY, input$disX), 0.7)
                  metric = (rs - input$coinR)^2 + (bs - input$coinB)^2 + (gs - input$coinG)^2
                  binary = ifelse(metric > input$threshold, 1, 0)
                  coinRes = ifelse(binary1 == 0 & binary == 0 , 1, 0)
                  #Finding coin and getting its area
                  coinRes = floodFill(coinRes, c(input$coinY, input$coinX), 0.5)
                  coinArea = length(coinRes[coinRes == 0.5])
                  pixelCost = coinArea/(2.5*2.5*pi)
                  #Calculating disease area using coin area
                  disArea = length(res[res = 0.7]) * pixelCost
                  output$binaryImage <- renderImage({
                        out = tempfile(fileext = ".jpg")
                        writeJPEG(res, out)
                        list(src = out)
                  }, deleteFile = T)
                  alert(disArea)
            }
      })

}

shinyApp(ui = ui, server = server)