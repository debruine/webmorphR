function RGBImage = Lab2RGB(LabImage)
% Applies makecform('srgb2lab'); and applycform(RgbImage,Conversion);
WP = whitepoint('d65')
Conversion = makecform('lab2srgb','adaptedwhitepoint',WP);
RGBImage = applycform(LabImage,Conversion);