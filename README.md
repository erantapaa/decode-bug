
This package contains a test case for a UTF-8 decoding problem I am running into.

To reproduce:

1. Build the executable `bug`
2. Run `bug` (it reads the file `1510-3.html`)

The output I get is:

    checking message 633230595973508750
    checking message 633231816524133750
    checking message 633232223822415000
    ...
    checking message 633099125665929015
    bug: Cannot decode byte '\xc2': Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream

