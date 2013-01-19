%% A directory is a list of sub-filenames.
%% Events are `modified', `added', `deleted'.
-module(dirmon_lib).
-export([new/1,
         check/3,
         match/2,
         match_tree/2]).

-include_lib("kernel/include/file.hrl").

-record(file, {basename, fullname, mtime}).
-record(directory, {basename, fullname, mtime, sub_files, sub_directories}).


new(FileName) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}} ->
            {ok, SubFileNames} = file:list_dir(FileName),
            SubFileRecs = [new(filename:join(FileName, X))
                           || X <- lists:usort(SubFileNames)],
            SubFiles = [X || {ok, X=#file{}} <- SubFileRecs],
            SubDirs = [X || {ok, X=#directory{}} <- SubFileRecs],
            %% file or symlink.
            D = #directory{basename = filename:basename(FileName),
                           fullname = FileName,
                           mtime = MTime, 
                           %% Ignore errors.
                           sub_files = SubFiles,
                           sub_directories = SubDirs},
            {ok, D};
        {ok, #file_info{mtime = MTime}} ->
            F = #file{basename = filename:basename(FileName), 
                      fullname = FileName, 
                      mtime = MTime},
            {ok, F};
        {error, _Reason} = E ->
            E
    end.


%% `Time' is a starting time of the previous scanning.
%% Previous scanning is a scanning when X was generated.
check(X=#directory{fullname = FileName, mtime = MTime, sub_files = SubFiles, 
                   sub_directories = SubDirs}, Time, Events) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory, mtime = MTime}}
            when MTime < Time ->
            %% When MTime =:=, > Time, than there can be a situation,
            %% when the file was modified and analyse was occured
            %% at the same perion of time.
            %%
            %% It is cheap to check it again.
            %%
            %% File list is the same, check sub-directories and files.
            {SubDirs2,  Events2} = sub_dir_check(SubDirs, Time, Events),
            {SubFiles2, Events3} = sub_file_check(SubFiles, Time, Events2),
            X2 = X#directory{sub_directories = SubDirs2, sub_files = SubFiles2},
            {ok, X2, Events3};
            
        {ok, #file_info{type = directory, mtime = NewMTime}} ->
            %% Directory content was changed.
            {SubFiles2, SubDirs2, Events2} =
                sub_check(FileName, SubFiles, SubDirs, Time, Events),
            X2 = X#directory{sub_files = SubFiles2,
                             sub_directories = SubDirs2,
                             mtime = NewMTime},
            {ok, X2, Events2};

        {ok, #file_info{mtime = _NewMTime}} ->
            %% Replaced with a file.
            error(fixme);

        {error, _Reason} = E ->
            %% the directory was deleted.
            E
    end;
check(X=#file{fullname = FileName, mtime = MTime}, _Time, Events) ->
    case file:read_file_info(FileName) of
        {ok, #file_info{type = directory}} ->
            %% Replaced with a directory.
            error(fixme);

        {ok, #file_info{mtime = MTime}} ->
            %% If  MTime < Time, than a file CAN be changed.
            %% We can use md5sum of the file, but it will be slow.
            {ok, X, Events};
            
        {ok, #file_info{mtime = NewMTime}} ->
            %% File content was changed.
            {ok, X#file{mtime = NewMTime}, [{modified, X}|Events]};

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
    case X=New(N) of
    #file{}      -> sub_check(New, Chk, Ns, Fs, Ds, [{added,X}|Es], [X|AFs], ADs);
    #directory{} -> sub_check(New, Chk, Ns, Fs, Ds, [{added,X}|Es], AFs, [X|ADs]);
    undefined    -> sub_check(New, Chk, Ns, Fs, Ds, Es, AFs, ADs)
    end;
sub_check(_New, _Chk, [], Fs, Ds, Es, AFs, ADs) ->
    %% Fs and Ds are deleted.
    Es1 = [{deleted, F} || F <- Fs],
    Es2 = [{deleted, D} || D <- Ds],
    {lists:reverse(AFs), lists:reverse(ADs), Es1 ++ Es2 ++ Es}.
    



% match(Events, Pattern) 
match([E|Es], Re) ->
    match1(E, Re) ++ match(Es, Re);
match([], _Re) ->
    [].

match1({Type, #file{fullname = N}}, Re) ->
    case re:run(N, Re, [{capture, none}]) of
        match   -> [{Type, N}];
        nomatch -> []
    end;
match1({Type, D=#directory{}}, Re) ->
    #directory{fullname = N, sub_files = Fs, sub_directories = Ds} = D,
    [Y || X <- Ds, Y <- match1({Type, X}, Re)] ++
    [Y || X <- Fs, Y <- match1({Type, X}, Re)] ++
    case re:run(N, Re, [{capture, none}]) of
        match   -> [{Type, N}];
        nomatch -> []
    end.


%% @doc It is a variant of `match/2'. 
%% It does not uses event types.
%% It returns a list of filenames.
match_tree(#file{fullname = N}, Re) ->
    case re:run(N, Re, [{capture, none}]) of
        match   -> [N];
        nomatch -> []
    end;
match_tree(D=#directory{}, Re) ->
    #directory{fullname = N, sub_files = Fs, sub_directories = Ds} = D,
    [Y || X <- Ds, Y <- match_tree(X, Re)] ++
    [Y || X <- Fs, Y <- match_tree(X, Re)] ++
    case re:run(N, Re, [{capture, none}]) of
        match   -> [N];
        nomatch -> []
    end.
    
