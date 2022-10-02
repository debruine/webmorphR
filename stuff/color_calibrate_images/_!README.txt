NOTE!

Not all the CP2 images used for shape SD seem to have been calibrated. The following images are missing

WOMEN (5/50)
cp_171
cp_234
cp_259
cp_272
cp_332
cp_139 is there, but the delineation template doesn't fit the face

MEN
cp_197
cp_237
cp_323
cp_326
cp_341
cp_361
cp_366
cp_379
cp_382
cp_406
cp_411
cp_422


---- Decided with Ben that for analysing SD of face images, we will only use the 38 men's faces that are available, and cut the female sample down to the same number


---- CODE FOR PULLING colour-calibrated images ----

cd /Volumes/facelab/Stimuli/attractive_shape/colour/_iris_sexualDimorphism/pngs

dir=/Volumes/facelab/Imagesets/ESRC_CP2/color_calibrated/psychomorph
outputdir=/Volumes/facelab/Stimuli/attractive_shape/colour/_iris_sexualDimorphism/pngs/men
while IFS= read -r fname; do
    find "$dir" -name "$fname" -exec cp {} "$outputdir" \;
done < MEN.txt

dir=/Volumes/facelab/Imagesets/ESRC_CP2/color_calibrated/psychomorph
outputdir=/Volumes/facelab/Stimuli/attractive_shape/colour/_iris_sexualDimorphism/pngs/women
while IFS= read -r fname; do
    find "$dir" -name "$fname" -exec cp {} "$outputdir" \;
done < WOMEN.txt

