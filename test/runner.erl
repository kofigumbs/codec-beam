-module(runner).


main([BeamFile, ChunkName]) ->
    { ok, Bin } = file:read_file(BeamFile),
    Id = list_to_atom(ChunkName),

    case catch beam_lib:chunks(Bin, [Id]) of
        { ok, Chunks } ->
            io:fwrite("~w", [Chunks]);

        { error, _, Reason } ->
            fail(beam_lib:format_error(Reason));

        { 'EXIT', Reason } ->
            fail(io_lib:format("~p", [Reason]))
    end;

main(_) ->
    fail("Usage : escript runner BEAM_FILE CHUNK_NAME").


fail(Message) ->
    io:put_chars(standard_error, Message),
    halt(1).
