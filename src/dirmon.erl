-module(dirmon).
-export([new/1, check/3, match/2]).

-include_lib("kernel/include/file.hrl").

-record(file, {basename, fullname, mtime}).
-record(directory, {basename, fullname, mtime, sub_files, sub_directories}).


new(FileName) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}} ->
            {ok, SubFileNames} = file:list_dir(FileName),
            SubFileRecs = [new(filename:join(FileName, X))
                           || X <- lists:usort(SubFileNames)],
            %% file or symlink.
            D = #directory{basename = filename:basename(FileName),
                           fullname = FileName,
                           mtime = MTime, 
                           %% Ignore errors.
                           sub_files = [X || {ok, X=#file{}} <- SubFileRecs],
                           sub_directories = [X || {ok, X=#directory{}} <- SubFileRecs]},
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
            %% File list is the same, check sub-directories and files.
            {SubDirs2,  Events2} = sub_dir_check(SubDirs, Time, Events),
            {SubFiles2, Events3} = sub_file_check(SubFiles, Time, Events2),
            {ok, X#directory{sub_directories = SubDirs2, sub_files = SubFiles2}, Events3};
            
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



sub_file_check(SubDirs, Time, Events) ->
    sub_file_check(SubDirs, Time, Events, []).

sub_file_check([F|Fs], Time, Es, AFs) ->
    case check(F, Time, Es) of
      {ok, F1, Es1}   -> sub_file_check(Fs, Time, Es1, [F1|AFs]);
      {error, enoent} -> sub_file_check(Fs, Time, Es, AFs)
    end;
sub_file_check([], _Time, Es, AFs) ->
    {lists:reverse(AFs), Es}.
        


    
sub_check(DirName, SubFiles, SubDirs, Time, Events) ->
    {ok, SubFileNames} = file:list_dir(DirName),
    SubFileNames1 = lists:usort(SubFileNames),
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
    sub_check(New, Chk, SubFileNames1, SubFiles, SubDirs, Events, [], []).


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
    



% match(Events, Pattern) 
match([{Type, #file{fullname = N}}|Es], Re) ->
    case re:run(N, Re, [{capture, none}]) of
        match   -> [N|match(Es, Re)];
        nomatch -> match(Es, Re)
    end;
match([{Type, #directory{fullname = N, sub_files = Fs, sub_directories = Ds}}|Es], Re) ->
    [match([{Type, X}], Re) || X <- Ds] ++
    [match([{Type, X}], Re) || X <- Fs] ++
    case re:run(N, Re, [{capture, none}]) of
        match   -> [N|match(Es, Re)];
        nomatch -> match(Es, Re)
    end;
match([], _) -> [].

