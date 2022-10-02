%-----------------------------------------------------------------------
% Luminance and Chromatic images
% Based on Parraga and Troscianko paper
%
% Usage:          
%       [LUM,CRG,CBY] = lms2lumchro(LMS,disp)
% 
% Input:
%       LMS  = LMS Image to be analysed
%       disp = Option to display the images (1=Yes / 0=NO)
%
% Output:
%       LUM = Luminance image
%       CRG = Chromatic image = Red-Green
%       CBY = Chromatic image = Blue-Yellow
%
% Author: 
%    Adriana Olmos 10/Feb/2003
% Last modification:
%   09/Apr/2003 - CBY formula corrected
%   19/Mar/2003 - Added the gamma correction function just for display!?
%   20/Feb/2003 - Review function / renamed.
%                 Originals from lumchro.m
%   10/Feb/2003 - First version 
%-----------------------------------------------------------------------


function [LUM,CRG,CBY] = lms2lumchro(LMS,disp)


L=LMS(:,:,1);
M=LMS(:,:,2);
S=LMS(:,:,3);


%Luminance
LUM=L + M;


%Chromatic
%CRG = (M-L)./(LUM); 
CRG = (L-M)./(LUM); %%For first.m / theone.m
CBY = (S-(LUM./2))./(S+(LUM./2));


CBY=double(CBY)+1;
CRG=double(CRG)+1;


%Removing NANS for CRG
temp = isnan(CRG);
%M = max(max(CRG));
M=1;
[X,Y] = find(temp==1);
for i=1:length(X)
    CRG(X(i),Y(i))=M;
end


%Removing NANS for CBY
temp = isnan(CBY);
%M = min(min(CBY));
M=1;
[X,Y] = find(temp==1);
for i=1:length(X)
    CBY(X(i),Y(i))=M;
end


%For displaying images only
if (disp==1)
    subplot(2,2,1)
    I=scale0to1(LMS);
    imshow(I)
    title('LMS image')
    
    subplot(2,2,2)
    I(:,:,1)=LUM; I(:,:,2)=LUM; I(:,:,3)=LUM; 
    I=scale0to1(I); %Scale image for display
    imshow(I)
    title('Luminance image')
    
    subplot(2,2,3)
    I(:,:,1)=CRG; I(:,:,2)=abs(CRG-max(max(CRG))); I(:,:,3)=0;  
    I=scale0to1(I); %Scale image for display
    imshow(I); %imagesc(I)
    title('Chromatic red-green image')
     
    subplot(2,2,4)
    I(:,:,1)=abs(CBY-max(max(CBY))); I(:,:,2)=abs(CBY-max(max(CBY))); I(:,:,3)=CBY;
    I=scale0to1(I); %Scale image for display
    imshow(I)
    title('Chromatic blue-yellow image')
end
 