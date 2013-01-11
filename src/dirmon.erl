-module(dirmon).
-export([]).

-include_lib("kernel/include/file.hrl").

-record(file, {name, mtime}).
-record(directory, {name, mtime, sub_files, sub_directories}).


new(FileName) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}} ->
            SubFileNames = lists:usort(file:list_dir(FileName)),
            SubFileRecs = [new(X) || X <- SubFileNames],
            %% file or symlink.
            #directory{name = FileName,
                       mtime = MTime, 
                       sub_files = [X || X=#file{} <- SubFileRecs],
                       sub_directories = [X || X=#directory{} <- SubFileRecs]};
        {ok, #file_info{mtime = MTime}} ->
           #file{name = FileName, mtime = MTime}
    end.


check(X=#directory{name = FileName, mtime = MTime, sub_files = SubFiles, 
                   sub_directories = SubDirs}, Time, Events) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}} ->
            %% Same, check sub-directories.
            
        {ok, #file_info{type = directory, mtime = NewMTime}} ->
            %% Directory content was changed.
            SubFileNames = lists:usort(file:list_dir(FileName)),
            {SubFiles2, SubDirs2, Events2} = sub_check(SubFileNames, SubFiles, SubDirs, Events),
            {X#directory{sub_files = SubFiles2, sub_directories = SubDirs2}, Events2};



        {ok, #file_info{mtime = NewMTime}} ->
            %% Replaced with a file.

        {error, enoent} ->
            %% the directory was deleted.

    
sub_check(SubFileNames, SubFiles, SubDirs, Events) 
