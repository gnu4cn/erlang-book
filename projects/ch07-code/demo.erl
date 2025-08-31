unpack_image_resource_directory(Dir) ->
    <<Characteristics : ?DWORD,
      TimeDateStamp : ?DWORD,
      MajorVersion : ?WORD,
      MinorVersion : ?WORD,
      NumberOfNamedEntries : ?WORD,
      NumberOfIdEntries : ?WORD, _/binary>> = Dir,
    ..
