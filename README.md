
This package contains a test case for a UTF-8 decoding problem I am running into.

Tested with GHC 7.8.3 (Haskell Platform 2014.2.0.0), OSX 10.8.5.

To reproduce:

1. Build the executable `bug`:

        cabal sandbox init
        cabal install --only-dependencies --force-reinstalls
        cabal build

2. Run `bug` (it reads the file `1510-3.html`):

        ./dist/build/bug/bug

The output I get is:

    in file 1510-3.html, number of bytes: 75632 chars: 75046 spaces: 4396
    checking message 633230595973508750
    checking message 633231816524133750
    checking message 633232223822415000
    ...
    checking message 633099125665929015
    bug: Cannot decode byte '\xc2': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream

#### Background

* The program uses `xml-conduit` to scrape messages from a web forum page (file `1510-3.html`)
* The module `MyDOM.hs` is a slightly modified version of`Text.HTML.DOM` with the following changes:
  * tag and attribute names are converted to lowercase
  * the document is read using `decodeUtf8With strictDecode` instead of `lenientDecode`
* I believe the HTML file contains well-formed UTF-8. The html file is read in twice - both times in `strictDecode` mode. After the first read the number of bytes, characters and spaces are reported. The second time it is parsed as HTML and the messages are extracted. The exception is raised during the second read.


