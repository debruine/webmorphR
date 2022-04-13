reticulate::source_python("stuff/facetrain.py")
#reticulate::source_python("inst/python/facedetect.py")

london <- demo_stim("london")
f_3dsk <- read_stim("~/Desktop/facetrain/3dsk_female")
m_3dsk <- read_stim("~/Desktop/facetrain/3dsk_male")
ea <- read_stim("~/Desktop/facetrain/ea")

# bind and resize
stim <- c(london, f_3dsk, m_3dsk, ea) %>% resize(0.5)

# make rotations
rot <- stim %>% rotate(-45:45) %>% setnames(prefix = "rot_")

# mirror reverse
mirror <- c(stim, rot) %>% mirror("FRL") %>% setnames(prefix = "mirror_")


stimuli <- c(stim, mirror, rot)
tem_to_xml(stimuli, "~/Desktop/img")

# xml <- readLines("~/Desktop/img/images.xml")
# xml[1:10]

facetrain("/Users/lisad/Desktop/img/images.xml", 
          "inst/python/frl.dat", 
          tree_depth = 5L, 
          nu = 0.5, 
          cascade_depth = 15L)

file.size("inst/python/frl.dat")/1024/1024

devtools::load_all(".")
stimuli <- c(demo_stim("lisa"), demo_stim("zoom"))
s2 <- auto_delin(stimuli, "frl", TRUE)
draw_tem(s2) %>% to_size(400, 600) %>% plot(nrow = 2)
