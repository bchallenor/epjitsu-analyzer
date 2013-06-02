epjitsu-analyzer: Analyzes `.pcap` USB logs from a Fujitsu ScanSnap (S1300i-family) scanner
===========================================================================================

I'm trying to add support for the Fujitsu ScanSnap S1300i scanner to the epjitsu SANE backend on Linux, and need some way of interpreting the USB logs from the Windows driver.

This tool can reconstruct the high-level protocol from the raw USB packets in a `.pcap` file, and write it to a human-readable log.


Sample output
-------------

    Command 0xd4: set paper feed
    Received return code: 0x06
    Sent byte: 0x01
    Received return code: 0x06

    Command 0x33: get sensor flags
    Received sensor flags: BitSet(7, 12, 13) -> Map(ScanSw -> false, AdfOpen -> false, Hopper -> true, Top -> true, Sleep -> false)

    Command 0xb5: ???
    Received unknown bytes: {
    0x06
    }
    Received unknown bytes: {
    0x19, 0x00
    }

    Command 0xd0: set lamp
    Received return code: 0x06
    Sent boolean: false
    Received return code: 0x06

    Command 0xd1: set window
    Received return code: 0x06
    Sent payload: {
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40, 0x00, 0x00, 0x01, 0x2c, 0x01, 0x2c, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x0a, 0xc0, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00,
    0x00, 0x05, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe1, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
    }
    Received return code: 0x06


License
-------

    Copyright (C) 2013 Ben Challenor <ben@challenor.org>

    Permission is hereby granted, free of charge, to any person obtaining a copy of
    this software and associated documentation files (the "Software"), to deal in
    the Software without restriction, including without limitation the rights to
    use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
    the Software, and to permit persons to whom the Software is furnished to do so,
    subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
    FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
    COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
    IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
    CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

