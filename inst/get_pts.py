import face_recognition as fr

def get_pts(file, face = 1):
  face_im = fr.load_image_file(file)
  # Detect the landmarks, extract just the first face
  return fr.face_landmarks(face_im)[face-1]
