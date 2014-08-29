This package contains a test case for a UTF-8 decoding problem I am running into.

Tested with GHC 7.8.3 (Haskell Platform 2014.2.0.0), OSX 10.8.5.

To reproduce:

1. Build the executable `bug`:

        cabal sandbox init
        cabal install --only-dependencies --force-reinstalls
        cabal build
        
2. Run the tests:

        $ cabal repl
        *Main> main1
        *** Exception: Cannot decode byte '\xc2': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream
        *Main> main1a
        output written to file output1a.html
        *Main> main2
        output written to file output2.html
        *Main> main3
        in file 1510-3.html, number of bytes: 75632 chars: 75046 spaces: 4396
    
#### Notes

* The program uses `xml-conduit` to scrape messages from a web forum page (file `1510-3.html`)
* The module `NewDOM.hs` is a copy of `Text.HTML.DOM` with `lenientDecode` changed to `strictDecode`
* `main1` reads the HTML file using `strictDecode` and writes it back out as `output1.html`
* `main1a` reads the HTML file (as a strict ByteString) using `strictDecode` and writes it back out as `output1a.html`
* `main2` reads the HTML file using `lenientDecode` and writes it back out as `output2.html`
* `main3` reads the HTML file (as a strict ByteString) using `strictDecode` and counts the number of code-points and spaces
* I believe the HTML file contains well-formed UTF-8. There is a perl script `check-utf8` which validates it.
