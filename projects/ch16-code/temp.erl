-record(file_info,
        {
         size,          % Size of file in bytes.
         type,          % Atom: device, directory, regular,
                        % or other.
         access,        % Atom: read, write, read_write, or none.
         atime,         % The local time the file was last read:
                        % {{Year, Mon, Day}, {Hour, Min, Sec}}.
         mtime,         % The local time the file was last written.
         ctime,         % The interpretation of this time field
                        % is dependent on operating system.
                        % On Unix it is the last time the file or
                        % or the inode was changed. On Windows,
                        % it is the creation time.
         mode,          % Integer: File permissions. On Windows,
                        % the owner permissions will be duplicated
                        % for group and user.
         links,         % Number of links to the file (1 if the
                        % filesystem doesn't support links).
         ...
}).
