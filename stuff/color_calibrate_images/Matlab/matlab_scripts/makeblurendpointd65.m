function makeblurendpoint(imagename,L,a,b,PlusL,Plusa,Plusb,outimagename)

% makeendpoint(imagename, L,a,b, outimagename)
% imagename is name of masked image in current directory
% L, a and b are the values of L*a*b* you want in the final image
% outimagename is the name of the file you want to write

image=imread(imagename);

logimage=double(~~image(:,:,1));

gblur = fspecial('gaussian',15,3);

logimageblur=imfilter(logimage,gblur);
logimageblur(140:190,60:190,:) = logimage(140:190,60:190,:);

labimage(:,:,1)=(logimage.*L)+(logimageblur.*PlusL);
labimage(:,:,2)=(logimage.*a)+(logimageblur.*Plusa);
labimage(:,:,3)=(logimage.*b)+(logimageblur.*Plusb);

rgbimage=Lab2RGBd65(labimage);

imwrite(rgbimage,outimagename);

