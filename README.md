XBVC: eXtensible Bit Vector Communication (protocol...)
=======================================================

The 'e' is silent.

XBVC is a compact, endian independent, framed message passing protocol
suitable for use in embedded systems, or whatever you feel like
throwing it in.

Messages and Enumerations are defined in YAML using the following
syntax:

	#This is an example Extensible Bit Vector Communication protocol message definition

	#Field types:
	#f32: 32 bit floating point ('float' in c)
	#u32: unsigned 32 bit integer
	#s32: signed 32 bit integer
	#u16: unsigned 16 bit integer
	#s16: signed 16 bit integer
	#u8: unsigned 8 bit integer
	#s8: signed 8 bit integer
	#y[x]: array of type 'y', 'x' members long

	#Example enumeration
	# enumeration values are parsed in order
	get_target: {
		RPM,
		TPS,
		MAP,
		IAT,
	}

	# Example Messages
	# Note that there are two special fields in each command
	# _targets: this specifies which target (i.e. which end of the message pipe)
	# will be responsible for generating this message.  Since this protocol was made
	# with embedded targets in mind, this is important for space savings in the code
	# Note that it is not necessary for all language targets to adhere to this directive
	# _id: This specifies the unique message id and allows for somppe minor extensibility
	get_command:
		- _targets: {host}
		- _id: 0
		- Target: u32
		- Fluff: u8[10]

The xbvcgen.py tool then converts this yaml definition into source
code for any of the supported platforms (c and python for now, more to
follow).

This source code contains the message definitions, enumerations, and the
framer / message arbitration mechanism.  The only things the user must
provide are read/write/init functions, and the individual message
handlers. (Note: The message handling paradigm varies by language)

For example:

1. In the c implementation, the user must provide: xbvc_platform_init,
   xbvc_platform_write, and xbvc_platform_read
2. in the python implementation, the user must subclass XBVCEdgePoint
   and provide connect, _read, and _write functions (see the generated
   python code for an example that provides a loopback test)

## Prerequisites
1. Python 2.7
2. pyyaml (via pip)
3. jinja2 (via pip)

## Installation
1. Clone repository
2. run 'python setup.py install' in the project root

## Use
1. Create a yaml file containing your messages and enumerations using
   the schema outlined above
2. Call xbvcgen.py, passing in your yaml file as the input parameter,
   a destination folder as the output parameter (this will be created
   for you), and the targets and languages

	./xbvcgen.py -i msgs.yaml -o code -t device -l python

3. Integrate the generated files into your build (I suggest making
   this a phony target in your makefile and copying the files into
   their final destination)

4. Provide the platform specific functionality functions and implement
   your message handlers.

## License
This software, including all language target templates is provided
under the GPLv2 license.  HOWEVER, all generated code is the property
of it's respective owner to be licensed however they wish.  Please see
the LICENSE file for a copy of the GPL.

## A note on Floating Point encoding

XBVC supports 32 bit floating point numbers (ieee-754 single precision
floats), HOWEVER, they are encoded with 9 BCD digits of precision for
the fraction/decimal portion of the number and transmitted over the
wire as integers to avoid ABI incompatability on small MCUs.  Please
be warned that there may be some small rounding error after the 8th
decimal digit.

## TODO
1. Provide a python package
2. Whip up some platform examples (pyserial, c loopback, c socket, c
   serial)
3. Add more platforms (java, c++, go)
4. Think about providing some baked in messages (unknown message /
   problem decoding)

