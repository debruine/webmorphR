% averageColour:
%    function [R,G,B,L,a,b,avLUM,avCRG,avCBY] = averageColour(imagelist,outputfilename);
%    function [R,G,B,L,a,b,avLUM,avCRG,avCBY] = averageColour(imagelist,outputfilename,Mask);
%    function [R,G,B,L,a,b,avLUM,avCRG,avCBY] = averageColour(imagelist,outputfilename,TopLeft,BottomRight);
%
% Required Parameters: imagelist,outputfilename
% This function requires the filename of a text image list and returns
% average colour values for each image (calculated ignoring black).
% These statistics are written to outputfilename.xls Excel document in the current directory.
%
% Optional Parameters: Mask
% This can be either an image in the matlab workspace or the filename of an
% image. All zero value pixel positions in this image will be deleted from 
% all the images in imagelist before calculating the average colours. All 
% images in imagelist must be of the same size as this image.
%
% Optional Parameters: TopLeft,BottomRight
% The TopLeft, BottomRight parameters can be left off for calculating the
% whole image or specified to return values for a pixel box.
% Updated Final Version 19/4/5 - Michael Stirrat

function [R,G,B,L,a,b,avLUM,avCRG,avCBY] = averageColour(imagelist,outputfilename,TopLeft,BottomRight);

    % Get list of images to manipulate
    imagelist = importdata(imagelist);
    [m,n] = size(imagelist);
    % make cellarray of images
    images = cell(m,1);
    R = zeros(m,1);
    G = zeros(m,1);
    B = zeros(m,1);
    L = zeros(m,1);
    a = zeros(m,1);
    b = zeros(m,1);
    avLUM = zeros(m,1);
    avCRG = zeros(m,1);
    avCBY = zeros(m,1);
    cut=0;
    
    %output = fopen(outputfilename,'w');
    %fprintf(output, 'Image Red Green Blue L a* b* LUM CRG CBY\n');
    %fclose(output);
    
    if nargin == 4
        %Cut box out of image
        display(['Images are being cut to box.']);
        cut=1;
    elseif nargin == 3
        % should rewrite this to allow mask...
        display(['Images are being cut to mask.']);
        if class(TopLeft) == 'char'
            display(['Masking to ', TopLeft]);
            mask = imread(TopLeft);
            mask = ~~mask;
        else
            display(['Masking']);
            mask = ~~TopLeft;
        end
    end
    
WP = whitepoint('d65');
Conversion = makecform('srgb2lab', 'adaptedwhitepoint', WP);


    for x = 1:m
        display(['Processing Image: ',int2str(x), ' of ', int2str(m), ' Images.']);
        % Read image
        currentimage = imread(cell2mat(imagelist(x,1)));
        if cut == 1
            currentimage = currentimage(TopLeft(1):BottomRight(1), TopLeft(2):BottomRight(2), :);
        end
        if nargin == 3
            currentimage = immultiply(currentimage,mask);
        end
        % count non-zero pixels
        nonZero = currentimage(:,:,1) > 0|currentimage(:,:,2) > 0|currentimage(:,:,3) > 0;
%       nonZero = double(nonZero);
        denom = sum(sum(nonZero));
        % Calculate LumCRGCBY and L*a*b*
        [LUM,CRG,CBY] = lms2lumchro(double(currentimage),0);      
        currentimageLab = lab2double(applycform(currentimage,Conversion));
        % Average non-zero colour.
        R(x) = sum(sum(currentimage(:,:,1)))/denom;
        G(x) = sum(sum(currentimage(:,:,2)))/denom;
        B(x) = sum(sum(currentimage(:,:,3)))/denom;
        
        avLUM(x) = sum(sum(nonZero.*LUM))/denom;
        avCRG(x) = sum(sum(nonZero.*CRG))/denom;        
        avCBY(x) = sum(sum(nonZero.*CBY))/denom;        
            % Zero L*a*b* values for black pixels
            currentimageLab(:,:,1) = nonZero .* currentimageLab(:,:,1);
            currentimageLab(:,:,2) = nonZero .* currentimageLab(:,:,2);
            currentimageLab(:,:,3) = nonZero .* currentimageLab(:,:,3);
        L(x) = sum(sum(currentimageLab(:,:,1)))/denom;
        a(x) = sum(sum(currentimageLab(:,:,2)))/denom;
        b(x) = sum(sum(currentimageLab(:,:,3)))/denom;
        %output = fopen(outputfilename,'a');
        %fprintf(output, '%s %i %i %i %i %i %i %i %i %i\n', cell2mat(imagelist(x,1)),R(x,1),G(x,1),B(x,1),L(x,1),a(x,1),b(x,1),avLUM(x,1),avCRG(x,1),avCBY(x,1));
        %fclose(output);        
    end

display(['Writing to: ',outputfilename]);
output = [[{'Image'};imagelist],[{'Red'};num2cell(R)],[{'Green'};num2cell(G)], [{'Blue'};num2cell(B)], [{'L'};num2cell(L)], [{'a*'};num2cell(a)], [{'b*'};num2cell(b)], [{'LUM'};num2cell(avLUM)], [{'CRG'};num2cell(avCRG)], [{'CBY'};num2cell(avCBY)]];
csvwrite_cell(outputfilename,output);
end
