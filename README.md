# lein-lock

A Leiningen plugin to check transitive dependencies against a dependencies.lock
file to ensure repeatable builds.

## Usage

Put `[lein-lock "0.1.0-SNAPSHOT"]` into the `:plugins` vector of your project.clj.

To generate a new lockfile:

    $ lein lock freshen

To echo a fresh lockfile to stdout:

    $ lein lock echo

To check your dependencies against an existing lockfile:

    $ lein lock

## Customization

To customize via project.clj:

    :lock {:lockfile "different.lock" ;; defaults to dependencies.lock
           :dep-profile :test} ;; include :test scoped dependencies in lockfile. defaults to :uberjar

## License

Copyright © 2015 SparkFund
