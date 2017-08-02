# VaporTrail

VaporTrail is a tool for data transmission over FM, using
[RPITX](https://github.com/F5OEO/RPITX) to transmit from an out-of-the-box
Raspberry Pi, and an [RTL-SDR](http://www.rtl-sdr.com/) to receive.
Rudimentary block-based error correction is implemented using the
[zfec](https://github.com/tahoe-lafs/zfec) project. VaporTrail currently
transmits at 2000 bits per second using a bandwidth of roughly 31 KHz, and can
likely work at higher data rates.

<!-- TOC depthFrom:2 -->

- [1. Building and Installing VaporTrail](#1-building-and-installing-vaportrail)
  - [1.1. Install RPITX On Transmitter](#11-install-rpitx-on-transmitter)
  - [1.2. Install GQRX And socat On Receiver](#12-install-gqrx-and-socat-on-receiver)
    - [1.2.1. Mac](#121-mac)
    - [1.2.2. Ubuntu](#122-ubuntu)
    - [1.2.3. Arch Linux](#123-arch-linux)
    - [1.2.4. Everything Else](#124-everything-else)
  - [1.3. VaporTrail Binaries](#13-vaportrail-binaries)
  - [1.4. Building From Source](#14-building-from-source)
    - [1.4.1. Building on Raspberry Pi](#141-building-on-raspberry-pi)
- [2. Using VaporTrail](#2-using-vaportrail)
  - [2.1. Transmitting From a Raspberry Pi with `tools/transmit.sh`](#21-transmitting-from-a-raspberry-pi-with-toolstransmitsh)
  - [2.2. Receiving With GQRX and `tools/receive.sh`](#22-receiving-with-gqrx-and-toolsreceivesh)
  - [2.3. Using VaporTrail Directly](#23-using-vaportrail-directly)
    - [2.3.1. Encode PCM Data](#231-encode-pcm-data)
    - [2.3.2. Encode RPITX RF Commands](#232-encode-rpitx-rf-commands)
    - [2.3.3. Decode PCM Data](#233-decode-pcm-data)
- [3. Current Limitations](#3-current-limitations)
  - [3.1. GQRX Can't Be Used Programmatically](#31-gqrx-cant-be-used-programmatically)
  - [3.2. No Raspberry Pi Zero Support](#32-no-raspberry-pi-zero-support)
  - [3.3. No Streaming Decoder](#33-no-streaming-decoder)
  - [3.4. Issues With Streaming Encoding](#34-issues-with-streaming-encoding)

<!-- /TOC -->

<!-- This TOC is updated using the Markdown TOC package in VSCode -->

## 1. Building and Installing VaporTrail

### 1.1. Install RPITX On Transmitter

Install RPITX from
[https://github.com/F5OEO/rpitx](https://github.com/F5OEO/rpitx). Ensure the
`rpitx` command is available in your PATH. RPITX's `install.sh` script will do
this automatically by installing to /usr/bin

### 1.2. Install GQRX And socat On Receiver

The current supported method of FM demodulation is to use GQRX, sending the
output to a netcat or socat listener over UDP. This is a little janky, and
could be made better by using `rtl_fm` instead, but in the meantime, you will
need to install the `gqrx` tool before using VaporTrail.

The upside is, using GQRX makes it much easier to identify and isolate the
signal before receiving due to its graphical interface.

#### 1.2.1. Mac

Install socat with Homebrew

    brew install socat

Download GQRX from [http://gqrx.dk/](http://gqrx.dk/)

Alternatively, install with Homebrew

    brew cask install gqrx

#### 1.2.2. Ubuntu

    apt-get install socat gqrx-sdr

#### 1.2.3. Arch Linux

    pacman -S socat gqrx

#### 1.2.4. Everything Else

See [http://gqrx.dk/](http://gqrx.dk/) for information on downloading and
installing GQRX.

Consult your distribution's package repositories for socat installation.


### 1.3. VaporTrail Binaries

Compiling the libraries needed for VaporTrail can take quite a long time on a
Raspberry Pi, so for your convenience we've included a [binary ARMv7 build in
the Releases](https://github.com/inguardians/VaporTrail/releases).

### 1.4. Building From Source

First, install the stack build tool. See
[https://www.haskellstack.org/](https://www.haskellstack.org/) for details.

Then, from the project root folder:

    stack setup    # Downloads/installs the Haskell compiler if needed
    stack build    # Recursively build the project and all dependencies

Stack will tell you what folder the executable file is located. For example:

    Installing executable(s) in <path>

You may either copy the file from that folder to wherever you want, or run

    stack install

to automatically install the executable in `$HOME/.local/bin`

#### 1.4.1. Building on Raspberry Pi

Compiling Haskell can take a good chunk of RAM. When building on a Raspberry Pi
it's a good idea to force stack to use only a single build job. By default,
stack will use four build jobs, one per core, but this will quickly use up all
available RAM and force the compiler to dip into swap constantly.

    stack build -j 1

Even when using a single job, be sure to have some swap available just in case.
One gigabyte of swap should be sufficient, but two gigabytes may be a good idea
just to be safe.

The first compilation will take a long time, as it will require building all
dependencies as well. Our initial build took a little over two hours.
Re-compiling after changing the source code is significantly faster, in the
order of one to two minutes.

## 2. Using VaporTrail

### 2.1. Transmitting From a Raspberry Pi with `tools/transmit.sh`

Ensure the `vaportrail` and `rpitx` binaries are both available on your PATH
(meaning you can run them without a folder prefix such as `./`).  If you
installed `vaportrail` with `stack install`, add the following line to your
`.bashrc` file and log out and back in to your raspberry pi.

    export PATH="$HOME/.local/bin:$PATH"

Then, simply pipe data into `tools/transmit.sh`. For example, to transmit the
text "Hello":

    echo "Hello" | tools/transmit.sh

To transmit a file named `transmit_me`:

    tools/transmit.sh < transmit_me

### 2.2. Receiving With GQRX and `tools/receive.sh`

Open GQRX and tune to the frequency on which you're going to transmit.

From the "Input Controls" tab, disable Hardware AGC.

From the "Receiver Options" tab, change the filter mode to "Narrow FM".

From the "FFT Settings" tab, increase the "Freq zoom" to somewhere around 30x
to make it easier to see the signal, and set the FFT size to 16384 or 32768.

Switch back to the "Receiver Options" tab.

Click and drag on the filter window box (centered around the line indicating
your current frequency tuning) until the Filter width is around 32K.

**Click the "UDP" button in the bottom right to transmit output data over
UDP.**

Now, in a terminal, run

    tools/receive.sh

Alternatively, if you wish to save to a file, run

    tools/receive.sh > outputfile

This will listen for UDP data on the default GQRX port until you press enter. After
pressing enter, it will attempt to decode all data it received.

Begin transmitting. During the transmission, monitor GQRX. The center line in
the signal display window should line up with the center hump of the incoming
signal. If it doesn't, re-tune as necessary.

Once the transmission has completed, press enter to decode it.

If all data was received successfully, you should see the same data you
transmitted.

### 2.3. Using VaporTrail Directly

This section documents how to use the `vaportrail` command directly, for if you
want to skip the wrapper scripts and do it yourself.

#### 2.3.1. Encode PCM Data

    vaportrail enc_pcm

Read data from STDIN and encode it to a raw PCM audio stream. The stream has a single 48000Hz channel of 16-bit signed little-endian samples.

The following command pipeline should pass through the input data to the output
unchanged. If it does not, please report this as a bug. Currently, this will
fail for infinite data sources such as `tail -f`.

    vaportrail enc_pcm | vaportrail dec

#### 2.3.2. Encode RPITX RF Commands

    vaportrail enc

Read data from STDIN and encode it to RPITX's RF format (`-m RF`). This format
represents an FM signal with a series of tone instructions structured as shown
here:

    8 byte little-endian double         Frequency offset from
                                        center frequency in Hz.

    4 byte little-endian unsigned int   Duration of tone in nanoseconds.

    4 byte padding                      Unused padding data

#### 2.3.3. Decode PCM Data

    vaportrail dec

Read a raw PCM audio stream and decode a VaporTrail-encoded signal. The input
stream must consist of a single 48000Hz channel of 16-bit signed little-endian
samples.

## 3. Current Limitations

There's a few limitations in VaporTrail which are definitely solvable, but have
not been fixed yet. These are listed below.

### 3.1. GQRX Can't Be Used Programmatically

I (unknownln) have never interfaced with the RTL-SDR programmatically, so I
have no idea how to do it yet. Since VaporTrail currently relies on the
graphical GQRX program to handle demodulation the incoming FM signal, the
Raspberry Pi can't decode yet, at least not without installing a GUI. I'll be
looking into this soon, to support repeaters and two-way communication.

In the mean time, feel free to experiment with the standard RTL-SDR command
line tool for FM demodulation, `rtl_fm`. It *should* be possible to use this in
place of GQRX. That said, `rtl_fm`'s FM demodulation isn't as good as GQRX's,
so VaporTrail may not be able to decode the output. If I test this method and
get it working I'll update this README with details.

### 3.2. No Raspberry Pi Zero Support

As of writing this README, VaporTrail can only encode data on a Raspberry Pi 2
or Raspberry Pi 3. An encoder will have to be written in another language to
implement VaporTrail on a Raspberry Pi Zero or B+, because they use ARMv6
processors, which Haskell does not support. Luckily, the encode process is much
simpler than the decode process, so rewriting it should not be terribly
difficult. The primary challenge will be the use of a random number generator
to generate noise which is XORed with the data to more uniformly distribute the
ones and zeroes in the data. We'll need to write our own implementation of an
RNG to ensure that the encoder and decoder get the same results from the same
input seed.

### 3.3. No Streaming Decoder

Additionally, VaporTrail can not yet decode a stream of data, meaning a
received transmission must be written entirely to disk before it can be
decoded. *Note by unknownln: I'm currently working on a solution to this, and I'll
hopefully have it implemented within a week or two of today, August 02, 2017.*

### 3.4. Issues With Streaming Encoding

There's been an issue when encoding data while piping to RPITX where RPITX will
sometimes cut off before the transmission is finished. The cause is currently
unknown. This issue is low priority, since it may be fixed during the rewrite
for Pi Zero support, or the rewrite to fix the streaming decoder. However, it
is something that should remain on the radar moving forward. To work around it,
the transmission script currently encodes input data fully to a temp file
before transmitting that file.
