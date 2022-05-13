# https://stackoverflow.com/questions/54719496/how-to-install-dlib-for-python-on-mac

# Error in py_run_file_impl(file, local, convert) : 
# ModuleNotFoundError: No module named 'PIL'

#Sys.setenv(RETICULATE_PYTHON = "~/.virtualenvs/webmorph/bin/python")
# virtualenv_create("~/.virtualenvs/webmorph")
# # /Users/lisad/.virtualenvs/webmorph/bin/python -m pip install --upgrade pip
# use_virtualenv("~/.virtualenvs/webmorph/", required = TRUE)
# py_install("numpy")
# py_install("image")
# py_install("scipy")
# py_install("dlib")

Sys.getenv("RETICULATE_PYTHON")

# lots of build stuff

frl <- demo_stim("tem_examples", "frl") 
s <- frl |> remove_tem()
d7 <- auto_delin(s, "dlib7")
d70 <- auto_delin(s, "dlib70")
d81 <- auto_delin(s, dlib_path = "stuff/shape_predictor_81_face_landmarks.dat")
d194 <- auto_delin(s, dlib_path = "stuff/shape_predictor_194_face_landmarks.dat")
helen <- auto_delin(s, dlib_path = "stuff/helen-dataset.dat")

c(frl, d7, d70, d81, d194, helen) |>
  label(c("189-point manual",
          "5-point + 2",
          "68-point + 2",
          "81-point",
          "194-point",
          "194-point helen")) |>
  draw_tem(pt.size = 10) |>
  plot(nrow = 1) |>
  write_stim("~/Desktop", "example")
