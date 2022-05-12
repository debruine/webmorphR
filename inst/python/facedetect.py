import PIL.Image
import dlib
import numpy as np

# modified from:
# https://github.com/ageitgey/face_recognition/blob/master/face_recognition/api.py

def py_get_points(file, pred_file):
  # load pose predictor
  # https://github.com/davisking/dlib-models
  pose_predictor = dlib.shape_predictor(pred_file)
  
  # load image 
  im = PIL.Image.open(file).convert("RGB")
  face_image = np.array(im)
  
  # detect face
  face_detector = dlib.get_frontal_face_detector()
  face_locations = face_detector(face_image, 1)
  
  # get landmarks
  landmarks = [pose_predictor(face_image, face_location) for face_location in face_locations]
  landmarks_as_tuples = [[(p.x, p.y) for p in landmark.parts()] for landmark in landmarks]
  
  return landmarks_as_tuples

def py_get_location(file):
  # load image 
  im = PIL.Image.open(file).convert("RGB")
  face_image = np.array(im)
  
  # detect face
  face_detector = dlib.get_frontal_face_detector()
  face_locations = face_detector(face_image, 1)
  
  if (len(face_locations) == 0):
    return None
  
  rect = [[(rect.top(), rect.right(), rect.bottom(), rect.left())] for rect in face_locations]
  
  return rect[0]
