-module(runner).


main([BeamFile, ChunkName]) ->
    { ok, Bin } = file:read_file(BeamFile),
    Id = list_to_atom(ChunkName),

    case beam_lib:chunks(Bin, [Id]) of
        { ok, Chunks } -> io:fwrite("~w", [Chunks]);
        { error, _, Reason } -> io:fwrite("~w", [Reason])
    end;

main(_) ->
    io:fwrite("Usage : escript runner BEAM_FILE CHUNK_NAME"),
    halt(1).
