## create batch processing commands for CLIP (to detect fire)

library(data.table)
library(stringr)


## NWR_2021 ----------------------------------------------

folder_list <- fread('/Users/caraappel/Documents/CV4E/megadetector_runs/nwr2021_subfolders.txt', header = FALSE)

  n_batches <- 3 #how many gpus
  (per_batch <- length(folder_list$V1) / n_batches)
  
  cmd_list <- NULL
  for (ff in folder_list$V1){
    #site <- sapply(strsplit(ff, '\\/'), '[', 8)
    index_ff <- which(folder_list$V1 == ff)
    gpu_ff <- ifelse(index_ff < per_batch, '0',  #if 3 GPUs
                     ifelse(index_ff < per_batch*2, '1', '2'))
    cmd_ff <- paste('CUDA_VISIBLE_DEVICES=', gpu_ff,
                    ' python',
                    ' process_clip_fire_batch.py ', ff, ' ',
                    'NWR_2021_fire', sep = '')
    cmd_list <- c(cmd_list, cmd_ff)
  }
  
  head(cmd_list)
  
    #save
    write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=0', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
                file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2021_gpu0.txt')
    write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=1', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
                file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2021_gpu1.txt')
    write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=2', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
                file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2021_gpu2.txt')
  
    
## NWR_2022 ----------------------------------------------
    
  folder_list <- fread('/Users/caraappel/Documents/CV4E/megadetector_runs/nwr2022_subfolders.txt', header = FALSE)

  n_batches <- 3 #how many gpus
  (per_batch <- length(folder_list$V1) / n_batches)
  
  cmd_list <- NULL
  for (ff in folder_list$V1){
    #site <- sapply(strsplit(ff, '\\/'), '[', 8)
    index_ff <- which(folder_list$V1 == ff)
    gpu_ff <- ifelse(index_ff < per_batch, '0',  #if 3 GPUs
                     ifelse(index_ff < per_batch*2, '1', '2'))
    cmd_ff <- paste('CUDA_VISIBLE_DEVICES=', gpu_ff,
                    ' python',
                    ' process_clip_fire_batch.py ', ff, ' ',
                    'NWR_2022_fire', sep = '')
    cmd_list <- c(cmd_list, cmd_ff)
  }
  
  head(cmd_list)
  
  #save
  write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=0', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
              file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2022_gpu0.txt')
  write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=1', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
              file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2022_gpu1.txt')
  write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=2', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
              file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2022_gpu2.txt')

  
  
## NWR_2023 ----------------------------------------------
  
  folder_list <- fread('/Users/caraappel/Documents/CV4E/megadetector_runs/nwr2023_subfolders.txt', header = FALSE)
  
  n_batches <- 3 #how many gpus
  (per_batch <- length(folder_list$V1) / n_batches)
  
  cmd_list <- NULL
  for (ff in folder_list$V1){
    #site <- sapply(strsplit(ff, '\\/'), '[', 8)
    index_ff <- which(folder_list$V1 == ff)
    gpu_ff <- ifelse(index_ff < per_batch, '0',  #if 3 GPUs
                     ifelse(index_ff < per_batch*2, '1', '2'))
    cmd_ff <- paste('CUDA_VISIBLE_DEVICES=', gpu_ff,
                    ' python',
                    ' process_clip_fire_batch.py ', ff, ' ',
                    'NWR_2023_fire', sep = '')
    cmd_list <- c(cmd_list, cmd_ff)
  }
  
  head(cmd_list)
  
  #save
  write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=0', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
              file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2023_gpu0.txt')
  write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=1', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
              file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2023_gpu1.txt')
  write.table(cmd_list[grepl('CUDA_VISIBLE_DEVICES=2', cmd_list)], col.names = FALSE, row.names = FALSE, quote = FALSE,
              file = '/Users/caraappel/Documents/CV4E/megadetector_runs/batch_commands_clip_fire_nwr2023_gpu2.txt')
  