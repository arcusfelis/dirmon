-module(dirmon).
-export([new/1, check/3]).

-include_lib("kernel/include/file.hrl").

-record(file, {basename, fullname, mtime}).
-record(directory, {basename, fullname, mtime, sub_files, sub_directories}).


new(FileName) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}} ->
            SubFileNames = lists:usort(file:list_dir(FileName)),
            SubFileRecs = [new(X) || X <- SubFileNames],
            %% file or symlink.
            D = #directory{basename = filename:basename(FileName),
                           fullname = FileName,
                           mtime = MTime, 
                           sub_files = [X || X=#file{} <- SubFileRecs],
                           sub_directories = [X || X=#directory{} <- SubFileRecs]},
            {ok, D};
        {ok, #file_info{mtime = MTime}} ->
            F = #file{basename = filename:basename(FileName), 
                      fullname = FileName, 
                      mtime = MTime},
            {ok, F};
        {error, _Reason} = E ->
            E
    end.


check(X=#directory{fullname = FileName, mtime = MTime, sub_files = SubFiles, 
                   sub_directories = SubDirs}, Time, Events) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}} ->
            %% Same, check sub-directories.
            {SubDirs2, Events2} = sub_dir_check(SubDirs, Time, Events),
            {ok, X#directory{sub_directories = SubDirs2}, Events2};
            
        {ok, #file_info{type = directory, mtime = NewMTime}} ->
            %% Directory content was changed.
            {SubFiles2, SubDirs2, Events2} = sub_check(FileName, SubFiles, SubDirs, Time, Events),
            {ok, X#directory{sub_files = SubFiles2, sub_directories = SubDirs2, mtime = NewMTime}, Events2};

        {ok, #file_info{mtime = NewMTime}} ->
            %% Replaced with a file.
            error(fixme);

        {error, _Reason} = E ->
            %% the directory was deleted.
            E
    end;
check(X=#file{fullname = FileName, mtime = MTime}, Time, Events) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{mtime = directory}} ->
            %% Replaced with a directory.
            error(fixme);

        {ok, #file_info{mtime = MTime}} ->
            {ok, X, Events};
            
        {ok, #file_info{mtime = NewMTime}} ->
            %% File content was changed.
            {ok, X#file{mtime = NewMTime}, [{changed, X}|Events]};

        {error, _Reason} = E ->
            %% the directory was deleted.
            E
    end.


sub_dir_check(SubDirs, Time, Events) ->
    sub_dir_check(SubDirs, Time, Events, []).

sub_dir_check([D|Ds], Time, Es, ADs) ->
   case check(D, Time, Es) of
     {ok, D1, Es1}   -> sub_dir_check(Ds, Time, Es1, [D1|ADs]);
     {error, enoent} -> sub_dir_check(Ds, Time, Es, ADs)
   end;
sub_dir_check([], _Time, Es, ADs) ->
    {lists:reverse(ADs), Es}.
        


    
sub_check(DirName, SubFiles, SubDirs, Time, Events) ->
    SubFileNames = lists:usort(file:list_dir(DirName)),
    New = fun(BaseName) -> 
            case new(filename:join(DirName, BaseName)) of
                {ok, Rec} -> Rec;
                %% Ignore the fact, that file was deleted.
                {error, enoent} -> undefined
            end
         end,
    Chk = fun(Rec, Es) ->
            case check(Rec, Time, Es) of
                {ok, Rec1, Es1} -> {Rec1, Es1};
                %% The file was suddenly deleted.
                %% Wait the next check to handle this is a good strategy.
                {error, enoent} -> {Rec, Es}
            end
        end,
    sub_check(New, Chk, SubFileNames, SubFiles, SubDirs, Events, [], []).

sub_check(New, Chk, [N|Ns], Fs, [D=#directory{basename = N}|Ds], Es, AFs, ADs) ->
    %% The directory is still here.
    {D1, Es1} = Chk(D, Es),
    sub_check(New, Chk, Ns, Fs, Ds, Es1, AFs, [D1|ADs]);
sub_check(New, Chk, [N|Ns], [F=#file{basename = N}|Fs], Ds, Es, AFs, ADs) ->
    %% The file is still here.
    {F1, Es1} = Chk(F, Es),
    sub_check(New, Chk, Ns, Fs, Ds, Es1, [F1|AFs], ADs);
sub_check(New, Chk, [N|_]=Ns, [F=#file{basename = FN}|Fs], Ds, Es, AFs, ADs)
    when N > FN ->
    %% The file was deleted.
    sub_check(New, Chk, Ns, Fs, Ds, [{removed, F}|Es], AFs, ADs);
sub_check(New, Chk, [N|_]=Ns, Fs, [D=#directory{basename = DN}|Ds], Es, AFs, ADs)
    when N > DN ->
    %% The directory was deleted.
    sub_check(New, Chk, Ns, Fs, Ds, [{removed, D}|Es], AFs, ADs);
sub_check(New, Chk, [N|Ns], Fs, Ds, Es, AFs, ADs) ->
    %% N is a new file or directory.
    case New(N) of
        F=#file{}      -> sub_check(New, Chk, Ns, Fs, Ds, [{new, F}|Es], [F|AFs], ADs);
        D=#directory{} -> sub_check(New, Chk, Ns, Fs, Ds, [{new, D}|Es], AFs, [D|ADs]);
        undefined      -> sub_check(New, Chk, Ns, Fs, Ds, Es, AFs, ADs)
    end;
sub_check(_New, _Chk, [], Fs, Ds, Es, AFs, ADs) ->
    %% Fs and Ds are deleted.
    Es1 = [{deleted, F} || F <- Fs],
    Es2 = [{deleted, D} || D <- Ds],
    {lists:reverse(AFs), lists:reverse(ADs), Es1 ++ Es2 ++ Es}.
    



