# Revised create_zproject and create_spp functions from zonator package:
#   1) Use existing project directory rather than creating new one
#   2) Include full absolute path names in .bat and .spp files 

create_zproject_rev<-function (name, dir, variants, dat_template_file = NULL, spp_template_file = NULL, 
          spp_template_dir = NULL, overwrite = FALSE, debug = FALSE, 
          ...) 
{
     if (!file.exists(dir)) {
          stop("Directory ", dir, " provided does not exist.")
     }
    ###Define project directory as existing directory
     project_dir <- file.path(dir, name)
     if (!file.exists(project_dir)) {
               if (debug) 
                    message("The specified directory does not exist: ", project_dir)
     }
     if (debug) 
          message("Creating an empty README file")
     file.create(file.path(project_dir, "README.md"), showWarnings = FALSE)
     for (variant in variants) {
          variant_dir <- file.path(project_dir, variant)
          if (debug) 
               message("Creating a variant directory ", variant_dir)
          dir.create(variant_dir)
          if (is.null(dat_template_file)) {
               dat_template_file <- system.file("extdata", "template.dat", 
                                                package = "zonator")
          }
          dat_to <- file.path(variant_dir, paste0(variant, ".dat"))
          if (is.null(spp_template_file) & is.null(spp_template_dir)) {
               spp_template_file <- system.file("extdata", "template.spp", 
                                                package = "zonator")
          }
          spp_to <- file.path(variant_dir, paste0(variant, ".spp"))
          if (debug) 
               message("Copying template dat-file ", dat_template_file, 
                       " to variant directory ", variant_dir)
          if (file.exists(dat_template_file)) {
               file.copy(from = dat_template_file, to = dat_to, 
                         overwrite = TRUE)
          }
          else {
               stop("dat-file template ", dat_template_file, " not found")
          }
          if (!is.null(spp_template_dir)) {
               if (all(sapply(spp_template_dir, function(x) file.exists(x)))) {
                    if (debug) {
                         if (length(spp_template_dir) > 1) {
                              dir_msg <- paste("Creating a spp file from rasters in directories ", 
                                               paste(spp_template_dir, collapse = ", "))
                         }
                         else {
                              dir_msg <- paste("Creating a spp file from rasters in directory ", 
                                               spp_template_dir)
                         }
                         message(dir_msg)
                    }
                    create_spp_rev(filename = spp_to, spp_file_dir = spp_template_dir, 
                               ...)
               }
               else {
                    stop("Spp template dir ", spp_template_dir, " not found.")
               }
          }
          else if (!is.null(spp_template_file)) {
               if (file.exists(spp_template_file)) {
                    if (debug) {
                         message("Copying template spp-file  ", spp_template_file, 
                                 " to variant directory ", variant_dir)
                    }
                    file.copy(from = spp_template_file, to = spp_to, 
                              overwrite = TRUE)
               }
               else {
                    stop("Input template spp-file ", spp_template_file, 
                         " not found!")
               }
          }
          output_dir <- file.path(variant_dir, paste0(variant, 
                                                      "_out"))
          if (debug) 
               message("Creating an output directory ", output_dir)
          dir.create(output_dir, recursive = TRUE)
          bat_from <- system.file("extdata", "template.bat", package = "zonator")
          cmd_sequence <- scan(file = bat_from, "character", sep = " ",   ###this is vector of statements in .bat file
                               quiet = TRUE)
          ###Change relative to full path names in .bat
          dat_absolute<-paste0(getwd(),.Platform$file.sep,name,.Platform$file.sep,variant,.Platform$file.sep,variant,".dat")
          spp_absolute<-paste0(getwd(),.Platform$file.sep,name,.Platform$file.sep,variant,.Platform$file.sep,variant,".spp")

          output_dir_absolute <- paste0(getwd(),.Platform$file.sep,name,.Platform$file.sep,variant,.Platform$file.sep,variant,"_out")
          cmd_sequence <- gsub("INPUT_DAT", dat_absolute, cmd_sequence)
          cmd_sequence <- gsub("INPUT_SPP", spp_absolute, cmd_sequence)
          cmd_sequence <- gsub("OUTPUT", file.path(output_dir_absolute, 
                                                   paste0(variant, ".txt")), cmd_sequence)
          bat_to <- file.path(project_dir, paste0(variant, ".bat"))
          if (debug) 
               message("Writing bat file ", bat_to)
          cat(paste0(paste(cmd_sequence, collapse = " "), "\n"), 
              file = bat_to)
     }
     return(invisible(NULL))
}

create_spp_rev <- function (filename = "filelist.spp", weight = 1, alpha = 1, bqp = 1, 
          bqp_p = 1, cellrem = 0.25, spp_file_dir, recursive = FALSE, 
          spp_file_pattern = ".+\\.(tif|img)$", override_path = NULL) 
{
  target_rasters <- list()
  if (length(spp_file_dir) > 1) {
    for (item in spp_file_dir) {
      ###Change relative to full path names in .spp
      target_rasters[[item]] <- paste0(getwd(),.Platform$file.sep,list.files(path = item, 
                                           pattern = spp_file_pattern, full.names = TRUE, 
                                           recursive = recursive))
    }
  }
  else {
    target_rasters[[spp_file_dir]] <- paste0(getwd(),.Platform$file.sep,list.files(path = spp_file_dir, 
                                                 pattern = spp_file_pattern, full.names = TRUE, 
                                                 recursive = recursive))
  }
  if (length(target_rasters) == 0) {
    stop("No raster(s) matching the spp_file_pattern ", spp_file_pattern, 
         " found in ", spp_file_dir)
  }
  spp_content <- data.frame(weight = weight, alpha = alpha, 
                            bqp = bqp, bqp_p = bqp_p, cellrem = cellrem, sppfiles = as.vector(unlist(target_rasters)))
  if (!is.null(override_path)) {
    if (recursive) {
      if (length(spp_file_dir) == 1) {
        path_components <- strsplit(target_rasters[[1]], 
                                    paste0(spp_file_dir, .Platform$file.sep))
        target_rasters <- sapply(path_components, function(x) x[length(x)])
      }
      else {
        temp_target_rasters <- c()
        for (target_dir in names(target_rasters)) {
          path_components <- strsplit(target_rasters[[target_dir]], 
                                      paste0(target_dir, .Platform$file.sep))
          temp_target_rasters <- c(temp_target_rasters, 
                                   sapply(path_components, function(x) x[length(x)]))
        }
        target_rasters <- temp_target_rasters
      }
    }
    else {
      target_rasters <- sapply(target_rasters, function(x) basename(x))
    }
    spp_content$sppfiles <- file.path(override_path, target_rasters)
  }
  write.table(spp_content, file = filename, row.names = FALSE, 
              quote = FALSE, col.names = FALSE)
  return(invisible(TRUE))
}