# flahspaper

[![Build Status](https://travis-ci.org/bensteinberg/flahspaper.svg?branch=master)](https://travis-ci.org/bensteinberg/flahspaper)
[![Coverage Status](https://coveralls.io/repos/github/bensteinberg/flahspaper/badge.svg?branch=master)](https://coveralls.io/github/bensteinberg/flahspaper?branch=master)

"flahspaper" is pronounced "flashpaper". It's flashpaper with Haskell
inside.

This ~blatant ripoff of~ homage
to [go-flashpaper](https://github.com/rawdigits/go-flashpaper), a
service for one-time links, is an exercise in writing a real-world
program in Haskell. It has diverged from Ryan's excellent one-file
aesthetic, as I wanted to use `stack` to generate scaffolding and set
up tests.

I quote: "It is a web service that allows you to upload text snippets
or files and generates one time use links to share these things with
other people. As soon as the sharing link is accessed, the data is
deleted from the web service's memory and the link expires. This means
that old links are useless, even if shared somewhere insecure. This
can be used to share sensitive data with friends or colleagues."
Flahspaper, like go-flashpaper, "has a maximum data retention period
of 24 hours."

## Build and run

I commend you
to [go-flashpaper](https://github.com/rawdigits/go-flashpaper)'s
excellent README for considerations on running this anywhere other
than locally. There are several such considerations.

Locally:

1. Install
   [Haskell Stack](https://docs.haskellstack.org/en/stable/README/)
   and run `stack setup`
2. Clone this repo and enter the directory: `git clone
   https://github.com/bensteinberg/flahspaper.git ; cd flahspaper`
3. `stack build`
4. Get a TLS certificate. Put `certificate.pem` and `key.pem` in the
   application's directory. Like go-flashpaper, flahspaper "requires
   TLS, because we are not savages."
5. `stack exec flahspaper`
6. Visit your service at https://localhost:8443/

## Testing

Test with `stack test` or `stack test --coverage`. Coming up: Coveralls. Also, possibly, testing against a Warp server, which
should allow round-trip testing of creating a link, fetching the data,
and confirming that it can't be fetched again.

## Points of perhaps minor interest

Writing this program has been strangely like doing Haskell exercises
in a class or book, in that getting things to work is precisely a
matter of following the types.

This project contains a number of features commonly needed by
real-world programs, for each of which there are various possible
solutions. For the **web server**, I
chose [WAI](https://hackage.haskell.org/package/wai)
and [Warp](https://hackage.haskell.org/package/warp). I'm generating
**randomness**
with [Crypto.Random.DRBG](https://hackage.haskell.org/package/DRBG),
though I'm making a new seed every time, which may not be optimal or
idiomatic. In order to remove old secrets, we need **concurrency**,
using `forkIO` from Control.Concurrent (in base) and a `TVar`
from
[Control.Concurrent.STM](https://hackage.haskell.org/package/stm). To
know when a secret is old, we need **time**,
from [Data.Time](https://hackage.haskell.org/package/time)'s
`getCurrentTime` and `diffUTCTime`. I
use
[Text.InterpolatedString.Perl6](https://hackage.haskell.org/package/interpolatedstring-perl6) instead
of
[Text.RawString.QQ](https://hackage.haskell.org/package/raw-strings-qq) for
**quasiquoting** in order to get string interpolation.

If swap is turned off, secrets should only be kept in RAM. Presumably
that's safe, as anyone who can see your memory already owns you. The
question is, does writing a TVar actually overwrite the data in
memory? What kind of garbage collection does Haskell do? TODO:
investigate, perform the experiment.

Also TODO: don't use constants like go-flashpaper does -- make a
context/environment, maybe with ReaderT, so as to make testing easier
and remove the secrets argument to the application. Maybe also add the
random Gen to the context?

Also TODO: reorganize specs so they can be auto-discovered but don't
need to be specified in `other-modules`.
