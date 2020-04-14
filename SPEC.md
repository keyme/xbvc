# XBVC Specification

## Motivation

XBVC was originally born out of a frustration with using Google's
Protocol Buffers on resource constrained embedded systems. The desire
was to have a code generation platform, fed by a human maintainable
format, which output serializers, deserializers, and a function
dispatch interface for multiple languages.


## WireLine specification

The wire line protocol is composed of two layers. `Frames`, which
denote an entire packet of information, and `Fields`, which correspond
to individual data structure members.

### Frames

Frames in XBVC are delimited using [Consistent Overhead Byte
Stuffing](http://www.stuartcheshire.org/papers/COBSforToN.pdf) (or
COBS for short). COBS is a really nifty technique that allows one to
take otherwise arbitrary binary data, and strip all Zeros from it
(really any arbitrary value, but zero is often used). This is a very
fast, very low overhead operation (requiring something like 2 extra
bytes for every 255 bytes of data) and allows for packets to then be
delimited by the stripped character. In other words, it allows one to
have an almost readline like interface for binary packet reading.


### Fields

Fields on the other hand are represented using Extensible Bit Vectors
(EBV). EBVs are a technique commonly used in RFID communication which
allows for arbitrarily sized integers to be represented using the
minimum number of bytes.

This has two distinct advantages:

1) For many* values it is possible to achieve a bit of compression on
the wire. For example, the value 32 stored in a 32 bit integer is
represented by a single byte.  The value 128 only uses two
bytes. Obviously this goes the other way as well, if you have a value
stored in an unsigned 8 bit container that is > 128, you actually use
MORE bytes on the wire, however in practice this is generally not an issue.

2) As each byte in an EBV reserves its most significant bit to denote
whether or not there is additional data to be accumulated, determining
the boundary between EBVs is as easy as checking for a zero in the
most significant bit of each byte (I'm sensing a theme...). Once a
zero has been found, a field can be reconstructed and one can move on
to the next field.

<!-- TODO: Add pseudo code for EBV encoding/decoding -->


For additional information, please see the [EPC Global GEN2 standard,
appendix
A](https://www.gs1.org/sites/default/files/docs/epc/uhfc1g2_1_1_0-standard-20071017.pdf)


#### Floating point numbers

Say it with me: Floating point is the devil. While it would be nice to
assume that all targets represented FP numbers using the IEEE-754
standard, I'm not a huge fan of looking like an ass. To that point,
the XBVC project has adopted the following standard for encoding
floating point numbers which generalizes to all targets:

Floating point numbers are transmitted as two fields: A signed 32 bit
integer, and an unsigned 32 bit integer, in that order.

The first signed number is used to represent the whole number part of
the number in question

The second unsigned number represents the fractional part of the
number as the 9 digits after the decimal point IN REVERSE.

Wait what? Reverse?  Yes, reversed.  This allows us to deal with
leading zeroes in a halfway sane way.

E.g:

-32.00567 => [-32] + [76500]
