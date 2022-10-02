function csvwrite_cell(fname,C)
%
% csvwrite_cell(fname,C)
%
% ogbg 2012
%

fid = fopen(fname,'w');
if fid == -1, error(['csvwrite_cell: can''t open file ' fname ' for writing']); end

[r,c] = size(C);

for i = 1:r
    for j = 1:c
        if isnumeric(C{i,j})
            out = num2str(C{i,j});
        elseif ischar(C{i,j})
            out = C{i,j};
        else
            out = '';
        end
        fprintf(fid,'%s',out);
        if j < c, fprintf(fid,','); end
    end
    fprintf(fid,'\r\n');
end
fclose(fid);
