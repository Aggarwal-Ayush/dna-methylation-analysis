#Version 2
# Added in ability to read 450K and V2 chips and use newly trained models
# 1/26/24
# Will Chen, william.chen@ucsf.edu

library(shiny)
library(shinyFiles)
library(sesame)
library(sesameData)
library(BiocManager)
library(caret)
library(kernlab)
library(DNAcopy)
library(ggplot2)
library(GenomicRanges)
library(IRanges)

#CUSTOMIZED cnSegmentation
cnSegmentation_EPICv2 = function (sdf, sdfs.normal = NULL, genomeInfo = NULL, probeCoords = NULL, 
          tilewidth = 50000, verbose = FALSE) 
{
  stopifnot(is(sdf, "SigDF"))
  platform <- sdfPlatform(sdf, verbose = verbose)
  if (is.null(sdfs.normal)) {
    if (platform == "EPIC") {
      sdfs.normal <- sesameDataGet("EPIC.5.SigDF.normal")
    }
    else {
      stop(sprintf("Please provide sdfs.normal=. No default for %s", 
                   platform))
    }
  }
  if (is.null(genomeInfo)) {
    genome <- sesameData_check_genome(NULL, platform)
    genomeInfo <- sesameData_getGenomeInfo(genome)
  }
  if (is.null(probeCoords)) {
    genome <- sesameData_check_genome(NULL, platform)
    probeCoords <- sesameData_getManifestGRanges(platform, 
                                                 genome = genome)
  }
  seqLength <- genomeInfo$seqLength
  gapInfo <- genomeInfo$gapInfo
  target.intens <- totalIntensities(sdf)
  normal.intens <- do.call(cbind, lapply(sdfs.normal, function(sdf) {
    totalIntensities(sdf)
  }))
  target.intens <- na.omit(target.intens)
  pb_suffixincluded = names(target.intens)
  pb_suffixremoved = sapply(pb_suffixincluded,function(x) gsub("_.*","",as.character(x)))
  pb <- intersect(rownames(normal.intens), pb_suffixremoved)
  pb_i = match(rownames(normal.intens), pb_suffixremoved)
  pb_suffixincluded = pb_suffixincluded[pb_i[!is.na(pb_i)]]
  pb <- intersect(names(probeCoords), pb_suffixincluded)
  pb_suffixremoved_2 = sapply(pb,function(x) gsub("_.*","",as.character(x)))
  target.intens <- target.intens[pb]
  normal.intens <- normal.intens[as.character(pb_suffixremoved_2), ]
  probeCoords <- probeCoords[pb]
  fit <- lm(y ~ ., data = data.frame(y = target.intens, X = normal.intens))
  probe.signals <- setNames(log2(target.intens/pmax(predict(fit), 
                                                    1)), pb)
  bin.coords <- getBinCoordinates(seqLength, gapInfo, tilewidth = tilewidth, 
                                  probeCoords)
  bin.signals <- binSignals(probe.signals, bin.coords, probeCoords)
  structure(list(seg.signals = segmentBins(bin.signals, bin.coords), 
                 bin.coords = bin.coords, bin.signals = bin.signals), 
            class = "CNSegment")
}
#' Get bin coordinates
#'
#' requires GenomicRanges, IRanges
#' 
#' @param seqLength chromosome information object
#' @param gapInfo chromosome gap information
#' @param probeCoords probe coordinates
#' @param tilewidth tile width for smoothing
#' @return bin.coords
getBinCoordinates <- function(
    seqLength, gapInfo, tilewidth=50000, probeCoords) {
  
  tiles <- sort(GenomicRanges::tileGenome(
    seqLength, tilewidth=tilewidth, cut.last.tile.in.chrom = TRUE))
  
  tiles <- sort(c(
    GenomicRanges::setdiff(tiles[seq(1, length(tiles), 2)], gapInfo), 
    GenomicRanges::setdiff(tiles[seq(2, length(tiles), 2)], gapInfo)))
  
  GenomicRanges::values(tiles)$probes <-
    GenomicRanges::countOverlaps(tiles, probeCoords)
  
  bin.coords <- do.call(rbind, lapply(
    split(tiles, as.vector(GenomicRanges::seqnames(tiles))),
    function(chrom.tiles)
      leftRightMerge1(GenomicRanges::as.data.frame(
        GenomicRanges::sort(chrom.tiles)))))
  
  bin.coords <- GenomicRanges::sort(
    GenomicRanges::GRanges(
      seqnames = bin.coords$seqnames,
      IRanges::IRanges(start = bin.coords$start, end = bin.coords$end),
      seqinfo = GenomicRanges::seqinfo(tiles)))
  
  chr.cnts <- table(as.vector(GenomicRanges::seqnames(bin.coords)))
  chr.names <- as.vector(GenomicRanges::seqnames(bin.coords))
  
  names(bin.coords) <- paste(
    as.vector(GenomicRanges::seqnames(bin.coords)),
    formatC(unlist(lapply(
      GenomicRanges::seqnames(bin.coords)@lengths, seq_len)),
      width=nchar(max(chr.cnts)), format='d', flag='0'), sep='-')
  
  bin.coords
}

#' Bin signals from probe signals
#'
#' require GenomicRanges
#' 
#' @param probe.signals probe signals
#' @param bin.coords bin coordinates
#' @param probeCoords probe coordinates
#' @importFrom methods .hasSlot
#' @return bin signals
binSignals <- function(probe.signals, bin.coords, probeCoords) {
  ov <- GenomicRanges::findOverlaps(probeCoords, bin.coords)
  if (.hasSlot(ov, 'queryHits')) {
    .bins <- names(bin.coords)[ov@subjectHits]
    .probe.signals <- probe.signals[names(probeCoords)[ov@queryHits]]
  } else {
    .bins <- names(bin.coords)[ov@to]
    .probe.signals <- probe.signals[names(probeCoords)[ov@from]]
  }
  
  bin.signals <- vapply(split(.probe.signals, .bins), median, 1, na.rm=TRUE)
  bin.signals
}

#' Segment bins using DNAcopy
#'
#' @param bin.signals bin signals (input)
#' @param bin.coords bin coordinates
#' @return segment signal data frame
segmentBins <- function(bin.signals, bin.coords) {
  
  bin.coords <- bin.coords[names(bin.signals)]
  
  ## make input data frame
  maplocs <- as.integer((
    GenomicRanges::start(bin.coords) + GenomicRanges::end(bin.coords))/2)
  
  cna <- DNAcopy::CNA(
    genomdat = bin.signals,
    chrom = as.character(GenomicRanges::seqnames(bin.coords)),
    maploc = maplocs,
    data.type = 'logratio')
  
  seg <- DNAcopy::segment(
    x = cna, min.width = 5,
    nperm = 10000, alpha = 0.001, undo.splits = 'sdundo',
    undo.SD = 2.2, verbose=0)
  
  summary <- DNAcopy::segments.summary(seg)
  pval <- DNAcopy::segments.p(seg)
  seg.signals <- cbind(summary, pval[,c('pval','lcl','ucl')])
  seg.signals$chrom <- as.character(seg.signals$chrom)
  seg.signals
}

#' Visualize segments
#'
#' The function takes a \code{CNSegment} object obtained from cnSegmentation
#' and plot the bin signals and segments (as horizontal lines).
#'
#' require ggplot2, scales
#' @param seg a \code{CNSegment} object
#' @param to.plot chromosome to plot (by default plot all chromosomes)
#' @importFrom GenomicRanges start
#' @importFrom GenomicRanges end
#' @importFrom GenomicRanges seqnames
#' @importFrom GenomicRanges seqinfo
#' @return plot graphics
#' @examples
#'
#' sesameDataCache()
#' ## sdf <- sesameDataGet('EPIC.1.SigDF')
#' ## sdfs.normal <- sesameDataGet('EPIC.5.SigDF.normal')
#' ## seg <- cnSegmentation(sdf, sdfs.normal)
#' ## visualizeSegments(seg)
#'
#' sesameDataGet_resetEnv()
#' 
#' @export
visualizeSegments <- function(seg, to.plot=NULL) {
  
  stopifnot(is(seg, "CNSegment"))
  bin.coords <- seg$bin.coords
  bin.seqinfo <- seqinfo(bin.coords)
  bin.signals <- seg$bin.signals
  sigs <- seg$seg.signals
  total.length <- sum(as.numeric(bin.seqinfo@seqlengths), na.rm=TRUE)
  
  ## skip chromosome too small (e.g, chrM)
  if (is.null(to.plot)) {
    to.plot <- (bin.seqinfo@seqlengths > total.length*0.01) }
  
  seqlen <- as.numeric(bin.seqinfo@seqlengths[to.plot])
  seq.names <- bin.seqinfo@seqnames[to.plot]
  totlen <- sum(seqlen, na.rm=TRUE)
  seqcumlen <- cumsum(seqlen)
  seqstart <- setNames(c(0,seqcumlen[-length(seqcumlen)]), seq.names)
  bin.coords <- bin.coords[as.vector(seqnames(bin.coords)) %in% seq.names]
  bin.signals <- bin.signals[names(bin.coords)]
  
  GenomicRanges::values(bin.coords)$bin.mids <-
    (start(bin.coords) + end(bin.coords)) / 2
  GenomicRanges::values(bin.coords)$bin.x <-
    seqstart[as.character(seqnames(bin.coords))] + bin.coords$bin.mids
  
  ## plot bin
  p <- ggplot2::qplot(bin.coords$bin.x / totlen,
                      bin.signals, color=bin.signals, alpha=I(0.8))
  
  ## plot segment
  seg.beg <- (seqstart[sigs$chrom] + sigs$loc.start) / totlen
  seg.end <- (seqstart[sigs$chrom] + sigs$loc.end) / totlen
  p <- p + ggplot2::geom_segment(ggplot2::aes(x = seg.beg, xend = seg.end,
                                              y = sigs$seg.mean, yend=sigs$seg.mean), size=1.0, color='blue')
  
  ## chromosome boundary
  p <- p + ggplot2::geom_vline(xintercept=seqstart[-1]/totlen,
                               linetype="dotted", alpha=I(0.5))
  
  ## chromosome label
  p <- p + ggplot2::scale_x_continuous(
    labels=seq.names, breaks=(seqstart+seqlen/2)/totlen) +
    ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90, hjust=0.5))
  
  p <- p + ggplot2::scale_colour_gradient2(
    limits=c(-0.3,0.3), low='red', mid='grey', high='green',
    oob=scales::squish) + ggplot2::xlab('') + ggplot2::ylab('') +
    ggplot2::theme(legend.position="none")
  p
}

## Left-right merge bins
## THIS IS VERY SLOW, SHOULD OPTIMIZE
leftRightMerge1 <- function(chrom.windows, min.probes.per.bin=20) {
  
  while (
    dim(chrom.windows)[1] > 0 &&
    min(chrom.windows[,'probes']) < min.probes.per.bin) {
    
    min.window <- which.min(chrom.windows$probes)
    merge.left <- FALSE
    merge.right <- FALSE
    if (min.window > 1 && 
        chrom.windows[min.window,'start']-1 ==
        chrom.windows[min.window-1, 'end']) {
      merge.left <- TRUE
    }
    if (min.window < dim(chrom.windows)[1] &&
        chrom.windows[min.window, 'end']+1 ==
        chrom.windows[min.window+1, 'start']) {
      merge.right <- TRUE
    }
    
    if (merge.left && merge.right) {
      if (chrom.windows[min.window-1,'probes'] <
          chrom.windows[min.window+1,'probes']) {
        merge.right <- FALSE
      }
    }
    
    if (merge.left) {
      chrom.windows[min.window-1, 'end'] <-
        chrom.windows[min.window, 'end']
      
      chrom.windows[min.window-1, 'probes'] <-
        chrom.windows[min.window-1, 'probes'] +
        chrom.windows[min.window, 'probes']
      
      chrom.windows <- chrom.windows[-min.window,]
    } else if (merge.right) {
      chrom.windows[min.window+1, 'start'] <-
        chrom.windows[min.window, 'start']
      chrom.windows[min.window+1, 'probes'] <-
        chrom.windows[min.window+1, 'probes'] +
        chrom.windows[min.window, 'probes']
      chrom.windows <- chrom.windows[-min.window,]
    } else {
      chrom.windows <- chrom.windows[-min.window,]
    }
  }
  chrom.windows
}

### CUSTOMIZED sesameDataCache() to use less RAM
sesameDataCacheAll_custom = function(wanted_eh) {
  setExperimentHubOption(arg="MAX_DOWNLOADS", 100)
  
  eh_ids <- wanted_eh
  eh_ids <- eh_ids[eh_ids != "TBD"]
  print(eh_ids)
  suppressMessages(try({
    eh_ids <- eh_ids[!(eh_ids %in% names(ExperimentHub(localHub=TRUE)))]
  }, silent = TRUE))
  if (length(eh_ids) == 0) return(invisible(TRUE));
  tryCatch({
    sesameDataCache0_custom(eh_ids)
  }, error = function(cond) {
    message("ExperimentHub Caching fails:")
    message(cond)
    return(invisible(FALSE))
  })
  invisible(TRUE)
}

### Customized sesameDataCache0()
sesameDataCache0_custom <- function(eh_ids) {
  ## load meta data
  message(sprintf("Metadata (N=%d):\n", length(eh_ids)))
  suppressMessages(log <- capture.output(
    eh <- query(ExperimentHub(), "sesameData")[eh_ids]))
  
  ## load actual data
  tmp2 <- lapply(seq_along(eh), function(i) {
    message(sprintf(
      "(%d/%d) %s:\n", i, length(eh_ids), eh_ids[i]))
    suppressMessages(log <- capture.output(cache(eh[i])))
  })
}

######### START APP CODE ############

options(repos = BiocManager::repositories())
#rsconnect::configureApp("MeninMethylClass_V2_450K_added", size="xxlarge")
options(shiny.maxRequestSize = 30 * 1024^2)
#options(EXPERIMENT_HUB_CACHE="ExperimentHub")
probes = scan("probes.txt", character(), quote = "")
probes_450k = scan("probes_450K.txt", character(), quote = "")
probes_v2 = scan("probes_EPICv2.txt", character(), quote = "")
sesameDataCache()
wanted_eh_csv = read.csv('wanted_eh_smaller.csv')
wanted_eh = wanted_eh_csv$eh
#sesameDataCacheAll_custom(wanted_eh)
print('Cached')
sdfs.normal <- sesameDataGet('EPIC.5.SigDF.normal')
svm_Linear = readRDS("sesame_classifier.rds")
svm_Linear_4groups = readRDS("sesame_classifier_four.rds") #1 = immune, 2 = Hypermetabolic, 3 = Merlin intact, 4 = Proliferative
svm_Linear_450K = readRDS("svm_Linear_450K.rds")
svm_Linear_EPICV2 = readRDS("svm_linear_EPICv2.rds")
svm_Linear_450K_4groups = readRDS("svm_Linear_450K_4groups.rds")
svm_Linear_EPICV2_4groups = readRDS("svm_Linear_EPICv2_4groups.rds")
ui <- pageWithSidebar(
  
  # App title ----
  headerPanel("Choudhury et al Meningioma Methylation Classifier - version 2, 1/26/24"),
  
  # Sidebar panel for inputs ----
  fluidPage(
    fileInput("idat_grn", "Choose IDAT green file", accept = ".idat"),
    fileInput("idat_red", "Choose IDAT red file", accept = ".idat"),
    #textInput('path', "Enter path and prefix, eg ***_Grn.idat"),
    actionButton("Go","Predict"),
    tableOutput("Observe_Out_V"),
    tableOutput("Observe_Out_E"),
    plotOutput("Out_P"),
    uiOutput(outputId = "my_ui"),
    titlePanel(p("PLEASE READ THE DISCLAIMER BEFORE USING THE CLASS PREDICTION OR THIS WEBSITE: By using the website or the methylation class prediction, you accept this disclaimer. If you do not accept this disclaimer, do not use the methylation class predictor or this website. The methylation class predictor is based on research conducted by researchers at the University of California, San Francisco [Choudhury et al, Meningioma epigenetic grouping reveals biologic drivers and therapeutic vulnerabilities. Nature Genetics, January 2022, doi:10/1101/2020.11.23.20237495]. This methylation class predictor is distributed under the terms of the Creative Commons Attribution-Non-Commercial 3.0 International License (CC BY-NC 3.0) which permits the user to copy and redistribute the material in any medium or format and to remix, transform, and build upon the material, provided you give appropriate credit to the original author(s) and the source, provide a link to the Creative Commons License, and indicate if changes were made. Data should only be used for non-commercial research purposes by research professionals. The methylation class predictor is based on N=565 meningioma samples from 2 retrospective cohorts from independent international academic centers. Meningioma classification using methylation profiling is a research tool under development, it is not verified and has not been prospectively clinically validated. Implementation of the results in a clinical setting is in the sole responsiblity of the user and/or treating physician. This tool/website is not HIPAA compliant. Version 2 notes: (V2, 1/26/24) - added in version detection to detect the methylation chip version. Classifier can handle 450K chip, EPIC chip, and EPIC v2 chip. Please note, ONLY the EPIC CHIP was reported in the Choudhury et al, Nature Genetics 2022 publication. To develop compatibility with 450K and V2 chips, new SVM models were re-trained using overlapping probes existing in the 450K (1242 probes out of 2000) and V2 chips (1184 out of 2000). Models were re-trained using N=200 samples from UCSF, and tested in N=365 samples from UHK. Concordance was >98% using the EPIC V2 chip compared with the original EPIC model, and concordance was 100% using the 450K chip compared to the original EPIC model. These results have not been published. Please use this model at your own discretion and risk.", style = "font-family:'arial'; font-si6pt; font-size: 14px"))
  ),
  
  # Main panel for displaying outputs ----
  mainPanel()
)


server <- function(input, output) {
  
  observeEvent(input$Go, {
    withProgress(message='Processing', value=0, {
      file.copy(input$idat_grn$datapath, file.path(tempdir(),input$idat_grn$name), recursive = TRUE)
      file.copy(input$idat_red$datapath, file.path(tempdir(),input$idat_red$name), recursive = TRUE)
      path = file.path(tempdir(),sub("_Grn.idat","",input$idat_grn$name))
      idat=path
      print('Loaded files')
      incProgress(0.25, detail = 'Loading IDAT pair')
      sdf = readIDATpair(idat) %>% pOOBAH %>% noob %>% dyeBiasCorrTypeINorm
      sdf_chip_version = sdfPlatform(sdf)
      betas = getBetas(sdf)
      print('Loaded betas')
      newdat = as.data.frame(t(betas[names(betas) %in% probes])) #double check
      newdat[is.na(newdat)] = 0   #replace NA with 0 for sesame classifier
      incProgress(0.25, detail = 'Predicting class using SVM model')
      
      if (sdf_chip_version == 'EPIC'){
          p = predict(svm_Linear, newdata=newdat)
          p2 = predict(svm_Linear_4groups, newdata=newdat)
          if (p==3){
            subtype = 'Merlin Intact'
          }
          else if (p==1){
            subtype = 'Immune Enriched'
          }
          else if (p==2){
            subtype = 'Hypermitotic'
          }
          else {
            subtype = 'Unclassified'
          }
          df = c(subtype)
          
          if (p2==3){
            subtype_4group = 'Merlin Intact'
          }
          else if (p2==1){
            subtype_4group = 'Immune Enriched'
          }
          else if (p2==2){
            subtype_4group = 'Hypermetabolic'
          }
          else if (p2==4){
            subtype_4group = 'Proliferative'
          }
          else {
            subtype_4group = 'Unclassified'
          }
          df = c(subtype_4group)
      }
      
      if (sdf_chip_version == 'HM450'){
        newdat.450k = as.data.frame(t(betas[names(betas)  %in% probes_450k])) #double check
        newdat.450k[is.na(newdat.450k)] = 0   #replace NA with 0 for sesame classifier
        p = predict(svm_Linear_450K, newdata = newdat.450k)
        p2 = predict(svm_Linear_450K_4groups, newdata = newdat.450k)
        subtype = as.character(p)
        subtype_4group = as.character(p2)
        df = c(subtype)
      }
      
      if (sdf_chip_version == 'EPICv2'){
        newdat.v2 = as.data.frame(t(betas[names(betas)  %in% probes_v2])) #double check
        newdat.v2[is.na(newdat.v2)] = 0   #replace NA with 0 for sesame classifier
        names(newdat.v2) = sapply(names(newdat.v2),function(x) gsub("_TC21","",as.character(x)))
        names(newdat.v2) = sapply(names(newdat.v2),function(x) gsub("_BC21","",as.character(x)))
        p = predict(svm_Linear_EPICV2, newdata = newdat.v2)
        p2 = predict(svm_Linear_EPICV2_4groups, newdata = newdat.v2)
        subtype = as.character(p)
        subtype_4group = as.character(p2)
        df = c(subtype)
      }
      
      output$Observe_Out_V<-renderText({c("DETECTED CHIP VERSION: ", sdf_chip_version)})
      output$Observe_Out_E<-renderText({c("PREDICTED 3-GROUP CLASS: ", subtype, ", PREDICTED 4-GROUP CLASS: ", subtype_4group)})
      incProgress(0.25, detail = 'Creating methylation CNV plot')
      if (sdf_chip_version == 'EPICv2'){
        segs = cnSegmentation_EPICv2(sdf, sdfs.normal)
      }
      if (sdf_chip_version != 'EPICv2'){
        segs = cnSegmentation(sdf, sdfs.normal)
      }
      output$Out_P = renderPlot(print(visualizeSegments(segs, to.plot=1:22)))
      output$my_ui<-renderUI({
        img(src='nomogram.png',height='300px')
      })
    })
  })
  
}
shinyApp(ui, server)