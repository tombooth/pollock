# Pollock

A Clojure cli app that generates Jackson Pollock style images

## Usage

Assuming you have leiningen install you should be able to run:

    lein run

This will generate a Pollock style image with the default paramters,
to see the config allowed through the cli run:

    lein run -- --help

You can pass an unlimited number of config json files in through the command
line, as well as using - to indicate json will be passed through stdin. These
json objects will be sequencially merged with the default config in 
https://github.com/tombooth/pollock/blob/master/src/pollock/core.clj

## Debug mode

If you pass `--debug` through instead of generating an image a window will be
opened showing a 3D view of how the image would have been composed.

## License

Copyright © 2013 Tom Booth

Distributed under the Eclipse Public License, the same as Clojure.
