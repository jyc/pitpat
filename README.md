# PitPat

The sound of typing on keys is a tiny bit like the sound of rain hitting a car
window from the inside. Rain makes a pitter-patter sound, but pitter-patter is
too long.

When you run PitPat it records the timing of the characters you type, including
backspaces, and saves the times and the characters to a file. You can then play
back this file: the characters will be output with the same timings you used.
For me, this makes notes I write to myself seem a little bit more concrete than
just plain text files.

    $ pitpat
    Usage: pitpat [options] <file>
      -play   Play back the file instead of recording to it.
      -speed  Playback speed. Defaults to 1.0x.
      -help   Display this list of options

# Building

With [car](https://github.com/jonathanyc/car):

    $ car opt
    $ ./main.native

# Uses

- Keeping a journal on your computer.
  just plain text files.

# License

Licensed under the GNU GPL v3.

Not because I thnk this is some revolutionary technology, but because normally
I license stuff under the BSD, but recently I have been wondering if that is
really a good thing.
