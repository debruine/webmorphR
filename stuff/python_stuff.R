# dlib <<- reticulate::import("dlib", convert = FALSE, delay_load = TRUE)
# PIL <<- reticulate::import("PIL", convert = FALSE, delay_load = TRUE)
# np <<- reticulate::import("numpy", convert = FALSE, delay_load = TRUE)
# 
# # load pose predictor (get directly and include in pkg)
# path = normalizePath("stuff/shape_predictor_68_face_landmarks.dat")
# pose_predictor_68_point = dlib$shape_predictor(path)
# 
# # load image 
# im = PIL$Image$open("inst/extdata/test/f_multi.jpg")$convert("RGB")
# image = np$array(im)
# 
# # detect face
# face_detector = dlib$get_frontal_face_detector()
# face_locations = face_detector(image, 1)
# 
# # get landmarks


reticulate::source_python("stuff/facedetect.py")
pt = get_points("inst/extdata/test/f_multi.jpg")

matrix(unlist(pt), nrow = 2, dimnames = list(c("x", "y"), c()))


