This repository contains the DrRacket source code for RSound, a
cross-platform sound library.  Currently, it depends upon the
'portaudio' library, which is a separate repository.

This bundle represents a "pkg" for Racket, and should be installed
using "raco pkg install rsound".

IMPORTANT NOTE: It's vital
to restart DrRacket *before* upgrading the rsound (or portaudio) package using the
GUI interface. This is because both of these packages link to dynamic
libraries, and trying to update the packages after dynamically linking
to the library makes operating systems very unhappy.

