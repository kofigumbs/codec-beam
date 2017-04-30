-module(runner).


main([BeamFile, ChunkName]) ->
    { ok, Bin } = file:read_file(BeamFile),
    { ok, Chunks } = beam_lib:chunks(Bin, [list_to_atom(ChunkName)]),
    io:fwrite("~w", Chunks);

main(_) ->
    io:fwrite("Usage : escript runner BEAM_FILE CHUNK_NAME"),
    halt(1).
