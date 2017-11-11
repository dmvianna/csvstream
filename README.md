# csvstream

I'm learning how to parse big CSV files in Haskell. This is one attempt. The data I'll be analysing are the [public records of Australian patents](https://ipaustralia.gov.au/about-us/economics-ip/ip-government-open-data) accumulated over more than 100 years. You can get the CSV files from [data.gov.au](https://data.gov.au/dataset/intellectual-property-government-open-live-data). No, I will not include 785 MB of data (compressed) in this repository.

### First task

Reading from a stream.

### Second dask

Doing some basic data analysis, like counting records.

### Third task

Extracting relevant info from unstructured text, such as addresses. That's a big part of what I do for work, and the main motivation for looking beyond Python. I want to move away from regular expressions and do it fast.

### Fourth task

GROUP BY

### At some point

- Encoding results back into an output file.
- Indexing / examining data

## Proposed libraries

There are good tutorials for [cassava](https://hackage.haskell.org/package/cassava) by [Chris Allen](http://howistart.org/posts/haskell/1/) and [stackbuilders](https://www.stackbuilders.com/tutorials/haskell/csv-encoding-decoding/). I worked through these, in that order. So I'll retrace their steps with this new dataset as a starting point. Next, I may move on to [conduit](https://github.com/snoyberg/conduit#readme) because the syntax seems good and I read [some comments](https://www.yesodweb.com/blog/2013/10/core-flaw-pipes-conduit) suggesting it is more resilient to ugly data than [pipes](http://hackage.haskell.org/package/pipes). Or [machines](http://statusfailed.com/blog/2014/09/02/practical-machines-in-60-seconds.html). Or [streaming](https://hackage.haskell.org/package/streaming)...

Finally, I eagerly welcome help to move this forward. Get in touch!
