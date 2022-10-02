function rgb_poly = rgb2rgbpoly(rgb, number)
% rgb_poly = rgb2rgbpoly(rgb, number)
% takes single pixel value and returns function of rgb values depending on 
% the value of number:
%
% 3 : r g b
% 5 : r g b rgb 1
% 6 : r g b rg rb gb
% 8 : r g b rg rb gb rgb 1
% 9 : r g b rg rb gb r2 g2 b2
% 11: r g b rg rb gb r2 g2 b2 rgb 1

if (number == 3)
    rgb_poly = [rgb(1), rgb(2), rgb(3)];
end
% r g b rgb 1
if (number == 5)
    rgb_poly = [rgb(1), rgb(2), rgb(3), rgb(1)*rgb(2)*rgb(3), 1];
end
% r g b rg rb gb
if (number == 6)
    rgb_poly = [rgb(1), rgb(2), rgb(3), rgb(1)*rgb(2), rgb(1)*rgb(3), rgb(2)*rgb(3)];
end
% r g b rg rb gb rgb 1
if (number == 8)
    rgb_poly = [rgb(1), rgb(2), rgb(3), rgb(1)*rgb(2), rgb(1)*rgb(3), rgb(2)*rgb(3), rgb(1)*rgb(2)*rgb(3), 1];
end
% r g b rg rb gb r2 g2 b2
if (number == 9)
    rgb_poly = [rgb(1), rgb(2), rgb(3), rgb(1)*rgb(2), rgb(1)*rgb(3), rgb(2)*rgb(3), rgb(1)^2, rgb(2)^2, rgb(3)^2];
end
% r g b rg rb gb r2 g2 b2 rgb 1
if (number == 11)
    rgb_poly = [rgb(1), rgb(2), rgb(3), rgb(1)*rgb(2), rgb(1)*rgb(3), rgb(2)*rgb(3), rgb(1)^2, rgb(2)^2, rgb(3)^2, rgb(1)*rgb(2)*rgb(3), 1];
end