
if (interactive()) {
    library(shiny)
    library(tidyverse)
    library(dplyr)
    library(scales)
    library(shinyaframe)
    library(htmltools)
    data(mtcars)
    araba <- as.tibble(mtcars)
    araba$cyl_cat[araba$cyl==4] <- "V4"
    araba$cyl_cat[araba$cyl==6] <- "V6"
    araba$cyl_cat[araba$cyl==8] <- "V8"
    araba$gear_cat[araba$gear==3] <- "3-Gear"
    araba$gear_cat[araba$gear==4] <- "4-Gear"
    araba$gear_cat[araba$gear==5] <- "5-Gear"
    araba$carb_cat[araba$carb==1] <- "1-carb"
    araba$carb_cat[araba$carb==2] <- "2-carb"
    araba$carb_cat[araba$carb==3] <- "3-carb"
    araba$carb_cat[araba$carb==4] <- "4-carb"
    araba$carb_cat[araba$carb==6] <- "6-carb"
    araba$carb_cat[araba$carb==8] <- "8-carb"
# Define UI for application that draws a histogram
    ui = fluidPage(
        aDataSceneOutput(
            # attributes and child elements provided as arguments
            # server output variable name
            outputId = "mydatascene",
            # add backdrop
            environment = tags$script(
              tags$source(
                src = "https://unpkg.com/aframe-environment-component@1.3.1/dist/aframe-environment-component.min.js"
              )
            ),
            
            atags$entity(
              `environment`= "preset: forest; groundColor: #445"
            ),
        
            # gg-aframe plot syntax
            atags$entity(
                # an empty string sets attributes with no additional properties
                plot = "",
                # sizable scale option uses polyhedra scaled for equivalent volumes
                `scale-shape` = "sizable",
                position = "0 1.6 -1.38",
                atags$entity(
                    `layer-point` = "",
                    `data-binding__drat`="target: layer-point.x",
                    `data-binding__wt`="target: layer-point.y",
                    `data-binding__qsec`="target: layer-point.z",
                    `data-binding__gear_cat`="target: layer-point.shape",
                    `data-binding__carb.size`="target: layer-point.size",
                    `data-binding__cyl_cat.color`="target: layer-point.color"
                ),
                atags$entity(
                    `guide-axis` = "axis: x",
                    `data-binding__xbreaks` = "target: guide-axis.breaks",
                    `data-binding__xlabels` = "target: guide-axis.labels",
                    `data-binding__xtitle` = "target: guide-axis.title"
                ),
                atags$entity(
                    `guide-axis` = "axis: y",
                    `data-binding__ybreaks` = "target: guide-axis.breaks",
                    `data-binding__ylabels` = "target: guide-axis.labels",
                    `data-binding__ytitle` = "target: guide-axis.title"
                ),
                atags$entity(
                    `guide-axis` = "axis: z",
                    `data-binding__zbreaks` = "target: guide-axis.breaks",
                    `data-binding__zlabels` = "target: guide-axis.labels",
                    `data-binding__ztitle` = "target: guide-axis.title"
                ),
                atags$entity(
                    `guide-legend` = "aesthetic: shape",
                    `data-binding__shapetitle` = "target: guide-legend.title"
                ),
                #atags$entity(
                #    `guide-legend` = "aesthetic: size",
                #    `data-binding__sizebreaks` = "target: guide-legend.breaks",
                #    `data-binding__sizelabels` = "target: guide-legend.labels",
                #    `data-binding__sizetitle` = "target: guide-legend.title"
                #),
                atags$entity(
                    `guide-legend` = "aesthetic: color",
                    `data-binding__colorbreaks` = "target: guide-legend.breaks",
                    `data-binding__colorlabels` = "target: guide-legend.labels",
                    `data-binding__colortitle` = "target: guide-legend.title"
                )
                # animate the plot rotation
                #atags$other('animation', attribute = "rotation",
                #            from = "0 45 0", to = "0 405 0",
                #            dur = "10000", `repeat` = "indefinite")
            )
        )
    )
    
    server = function(input, output, session) {
        output$mydatascene <- renderADataScene({
            names(araba) <- tolower(names(araba))
            # Margin in (0,1) scale keeps polyhedra from sticking out of plot area
            positional_to <- c(0.01, 0.99)
            # convert to #RRGGBB color
            color_scale = setNames(rainbow(3, 0.75, 0.5, alpha = NULL),
                                   unique(araba$cyl_cat))
            araba %>%
                # scale positional data
                mutate_if(is.numeric, rescale, to = positional_to) %>%
                # scale size data to relative percentage, using cube root to correct
                # for radius->volume perception bias
                mutate(#carb.size = rescale(carb^(1/3), to = c(0.5, 2)),
                       cyl_cat.color = color_scale[cyl_cat]) ->
                araba_scaled
            
            # provide guide info
            make_guide <- function (var, aes, breaks = c(0.01, 0.5, 0.99)) {
                guide = list()
                domain = range(araba[[var]])
                guide[[paste0(aes, "breaks")]] <- breaks
                guide[[paste0(aes, "labels")]] <- c(domain[1],
                                                    round(mean(domain), 2),
                                                    domain[2])
                guide[[paste0(aes, "title")]] <- var
                guide
            }
            Map(make_guide,
                var = c("drat", "wt", "qsec"),
                aes = c("x", "y", "z")) %>%
                # repeat radius adjustment in the guide
                #c(list(make_guide("carb", "size", c(1,2,3,4,6,8)))) %>%
                Reduce(f = c) ->
                guides
            guides$shapetitle = "gear_cat"
            guides$colortitle = "cyl_cat"
            guides$colorbreaks = color_scale
            guides$colorlabels = names(color_scale)
            
            # convert data frame to list and combine with guides list
            aDataScene(c(araba_scaled, guides))
        })
    }
# Run the application 
shinyApp(ui = ui, server = server)
}