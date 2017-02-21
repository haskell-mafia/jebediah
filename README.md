![statue](images/jepediah.png)
```
  “I wish someone would cut his ugly old head off!”
  ―Jimbo
```

Cloudwatch logs tool. Jebediah as a library exposes Conduit sources and sinks for streaming to and from logs,
as well as simple functions for creating and listing groups and streams.

As a cli tool, these functions are exposed with a simple interface.

Usage:
```
jebediah ((-v|--version) | COMMAND)

Available options:
  -v,--version             Version information
  -h,--help                Show this help text

Available commands:
  list-groups               List all log groups
  list-streams              List log streams in a log group
  cat-stream                Cat a stream
  create-group              Create a log group
  create-stream             Create a log stream in a group
  upload-file               Upload a file to a new fresh stream
  upload-file-to-existing   Upload a file to an existing stream
```

Most commands self explanatory, the two upload commands may require some clarification. One should generally
use `upload-file`. This will create a new stream to write the file to, and fail if the stream already exists
or the file does not exist.

One can write to an empty, but previously created stream easily with `upload-file-to-exising`. To append to
an existing stream with contents in it already (which is not really recommended) one must obtain a sequence
token for new writes. This can be done by running the job without the token and reading it from the error
message.
