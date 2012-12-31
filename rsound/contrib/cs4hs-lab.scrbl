#lang scribble/doc

@(require scribble/manual)

@title{CS4HS Music & Racket}

@section{Getting Started}

First thing, let's log in.  You'll want to use the login "stu01", and
the password "CompSci101".

After logging in, click on the DrRacket symbol in the bottom center of
the screen.

Click on the "Run" button.  If all goes well, the bottom, "interactions"
window should show this text:

@verbatim{
Welcome to DrRacket, version 5.1.1
Language: Beginning Student with List Abbreviations [custom]; memory limit: 256 MB.
Teachpack: cs4hs-lib.rkt.
>}

If you see something different, ask for help :).

For our first program, type this in the top ("definitions") window:

@racketblock[(rsound-play ding)]

Now, click on the "Run" button.

With luck, you heard something! Adjust the computer volume so it's at a
comfortable level.

Okay, let's explore the sets of values in DrRacket.

@section{Values 1: Integers}

In Racket, integers are numbers. Try typing

@racketblock[14]

in the interactions window. Evaluate it, by clicking "Run". What did it produce?

What about this:

@racketblock[-324134127658176587622563578765]

?

@section{Operations 1: arithmetic:}

In Racket, there is just one way to use an operator: you write a left paren

(

followed by the name of the operator

+

followed by the arguments, separated by spaces

3 4 5

followed by a right parenthesis

)

Put it all together, and you get

@racketblock[(+ 3 4 5)]

Figure out what this program should produce.  *After* doing this, click on
the Run button to see if you agree with Racket.

Naturally, there are other operations as well:

@racketblock[(- 5 4)]

@racketblock[(/ 4 9)]

... did that one produce what you expected?

Exercise 1: write a program that divides twelve by three.

@section{Operations 2: nested operations:}

Just like in math class, expressions can go inside other expressions:

@racketblock[(* (- 2 1) (- 4 3))]

That is: multiply the result of two minus one by the result of four
minus three.

Exercise 2: write a program that divides three plus five by sixteen
divided by four.

@section{Values 2: lists of integers:}

Here's a list of numbers:

@racketblock[(list 60 62 64)]

Exercise 3: what will the program @racket[(list (+ 60 12) (+ 62 12))] produce? Try it!

@section{Values 3: sounds}

We've already seen one value: @racket[ding].

Try running this program:

@racketblock[ding]

...by itself.  What does it produce?

What about the @racket[rsound-play] operator... what does it do? Remember, you used
it earlier.

@section{Operations 3: music}

Finally! We're ready to make some music.

Our first try: let's apply @racket[rsound-play] to the list @racket[(list 60 60 64)].

What does it produce?

Try it! Oh dear. What kind of thing does rsound-play accept? An rsound. so...
how are we going to convert this list of numbers into an rsound?

Here's another operation: @racket[note-num-4ths]. 
It takes a list of numbers, and produces an rsound. 

Exercise: use the @racket[note-num-4ths] operator *and* the @racket[rsound-play] 
operator together to play a melody.

@section{Notes}

Next: can you figure out how the numbers are related to the music?

Numbers are hard to remember. Can we give them names? yes! using
"define". So, for instance, if you add 

@racketblock[(define c4 60)]

To the beginning of your program, then you can use the name "c4" to refer
to the value 60. Substitute c4 for the earlier use of 60, so you get

@racketblock[(rsound-play (note-num-4ths (list c4 64 67 64)))]

using "define", add these definitions:

Give the name "e4" to 64

Give the name "g4" to 67

Then, replace all the numbers in the program with names.

Here are some more:

g3 : 55

a3 : 57

b4 : 59

Exercise: Now, try to write the first line of "happy birthday."

@section{Another value: false.}

Here's another value:

@racketblock[false]

it's what's called a "boolean" value. There's another, shorter way to write this:

@racketblock[#f]

We're going to want the shorter version, for use in our songs.

We can use the #f value to represent "rests", or gaps in the melody. Like this:

@racketblock[
(define notes (list 72 69 72 #f 72 69 72 #f 74 72 70 69 67 69 70 #f))

(rsound-play (note-num-4ths notes))]

Here's another operator: "reverse". It takes a list, and produces a new list in the opposite order.

Try this program:

@racketblock[(reverse (list 60 70 80))]

Exercise: use "reverse" on the notes in the last song.  Is it recognizable?

Songs to identify:

Song 1:

@racketblock[
60 #f 60 #f 60 62 64 #f
64 62 64 65 67 #f #f #f
]

Song 2:

@racketblock[
72 #f 72 #f 72 #f 72 71
69 #f 69 #f 69 #f 69 67
65 #f 65 #f 65 #f 65 69
67 #f 67 #f 67 #f 67 67
]

Song 3:

@racketblock[
#f 64 64 64 64 62 60 62
64 #f 55 57 #f #f 60 62
60 #f #f 57 #f #f 60 62
60 #f #f 57 #f #f #f #f
]

@section{More operators:}

append: list list -> list

rsound-append: rsound rsound -> rsound

rsound-overlay: rsound rsound -> rsound

transpose : number list -> list

Can you use rsound-overlay to play two melodies at once?

