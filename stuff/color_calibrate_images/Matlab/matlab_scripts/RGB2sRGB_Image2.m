function imageout = RGB2sRGB_Image2(imageout, R, DV_Type)

if ~isa(imageout, 'double')
    imageout = im2double(imageout);
end

[m,n,p] = size(imageout);

for i=1:m
    for j=1:n
        imageout(i,j,:) = rgb2rgbpoly(imageout(i,j,:),11)*R;
    end
end

if strcmp(lower(DV_Type), 'xyz')
   imageout = xyz2srgb(imageout); 
elseif strcmp(lower(DV_Type), 'lab')
   imageout = Lab2sRGBd65(imageout); 
end