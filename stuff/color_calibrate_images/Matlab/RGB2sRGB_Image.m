function imageout = RGB2sRGB_Image(imagename, R, DVType)

pack;

imageout = imread(imagename);

imageout = im2double(imageout);

[m,n,p]=size(imageout);

splitx = floor(m/1000);
splity = floor(n/1000);

no_chunks = (splitx+1)*(splity+1);

display(['Processing: ', imagename, ' Split into ',num2str(no_chunks),' chunks']);
chunk=0;

for j=1:splitx
    for k=1:splity
        chunk=chunk+1;
        display(['Processing chunk ',int2str(chunk),' of ',int2str(no_chunks)]);
        imageout(((j-1)*1000)+1:((j)*1000),((k-1)*1000)+1:(k*1000),:) = RGB2sRGB_Image2(imageout(((j-1)*1000)+1:((j)*1000),((k-1)*1000)+1:(k*1000),:),R,DVType);
    end
    chunk=chunk+1;
    display(['Processing chunk ',int2str(chunk),' of ',int2str(no_chunks)]);   
    imageout(((j-1)*1000)+1:((j)*1000),(k*1000)+1:end,:) = RGB2sRGB_Image2(imageout(((j-1)*1000)+1:((j)*1000),(k*1000)+1:end,:),R,DVType);
end

for k=1:splity
    chunk=chunk+1;
    display(['Processing chunk ',int2str(chunk),' of ',int2str(no_chunks)]);
    imageout((splitx*1000)+1:end,((k-1)*1000)+1:(k*1000),:) = RGB2sRGB_Image2(imageout((splitx*1000)+1:end,((k-1)*1000)+1:(k*1000),:),R,DVType);
end

chunk=chunk+1;

display(['Processing chunk ',int2str(chunk),' of ',int2str(no_chunks)]);
imageout((splitx*1000)+1:end,(splity*1000)+1:end,:) = RGB2sRGB_Image2(imageout((splitx*1000)+1:end,(splity*1000)+1:end,:),R,DVType);


imageout = im2uint8(imageout);

imwrite(imageout,strcat(['sRGB_',imagename]));