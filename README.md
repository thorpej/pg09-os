# Playground OS and Monitor

This is the operating system and boot monitor environment for my
[6809 Playground](https://github.com/thorpej/pg09-system) home-brew
8-bit computer.

This is still very much evolving!  There are lots of design decisions
that have yet to be made!  And lots of documentation still to write!

# Design Overview

Playground OS has 3 basic components:
* A ROM-resident operating system kernel that provides (or will, eventually)
console and other I/O routines and memory management services.
* A ROM-resident monitor environment that provides some basic interaction
with the hardware (reading / writing / executing code in memory locations,
etc.), some basic debugging capabilities, loading code from peripherals
(e.g. S-Records via the console), and support for booting the system.
* A RAM-resident command interpreter.  This command interpreter will have
a series of built-in commands for interacting with files on storage devices
and loading and running programs (with assistance from the kernel).  This
command interpreter is loaded into bank 0 of Low Banked RAM and bank 0 of
High Banked RAM, which are reserved for this purpose.  Programs run from the
command interpreter will be loaded into other RAM banks which will be
dynamically allocated by the kernel.  This allows the command interpreter
to remain resident so it's quick to get back to it when another program is
finished running.

Operating system services are provided by so-called "system subroutines".
These are somewhere in between a system call and a library routine.  Where
a system call would normally be invoked by some sort of trap or software
interrupt instruction, system subroutines are just called like regular
subroutines, through a jump table in fixed ROM, like so:

    jsr     [SysSubr_cons_getc]

This isn't exactly a revolutionary idea, of course.  But the 6809's
extended-indirect addressing mode makes it particularly easy.

Because operating system code running in banked ROM may rely on other
routines running in other ROM banks, the operating systme provides a
trampoline routine, *SysSybr_brom_call*, that facilitates calling routines
in other ROM banks.

# Code Organization

The code is broken up into subdirectories that generally map to where the
code is located in the system memory map:

* **banked_rom0/** -- This contains code that runs in bank 0 of the banked ROM.  It mainly consists of non-critical monitor commands.
* **drivers/** -- Drivers are generally loaded into fixed ROM, but may
  have components that are loaded into banked ROM.  These files will be
  included by other top-level files.
  * **cons/** -- This is the virtualized console driver.  As of now, it
    can only perform I/O using the ACIA serial port, but eventually it
    will also support output to a video display terminal emulator and,
    at some point, arbitrary redirection.  In additional to the basic
    *getc* and *putc* routines, there is also a *getline* routine that
    handles reading a line of input from the console and handles some
    useful control characters (like CTRL-U and CTRL-C).
  * **mc6809/** -- This "driver" mainly contains constants related to
    the 6809 CPU, such as the condition code register bits and interrupt
    frame offsets.
  * **w65c21/** -- This "driver" mainly contains constants related to
    the 65C21 PIA's registers.
  * **w65c22/** -- This "driver" mainly contains constants related to
    the 65C22 VIA's registers.
  * **w65c51/** -- This is a polled-mode console driver for the 65C51
    ACIA UART.
* **examples/** -- This contains example programs that can be loaded
  and run on the Playground.
* **fixed-ram/** -- This contains the layout of the fixed RAM region.
  Fixed RAM is reserved for the kernel's use.
* **fixed-rom/** -- This contains the kernel code that runs in the
  fixed ROM.  This is the only fixed region where code can run, and thus
  contains support routines needed to switch RAM and ROM banks, call
  code running in other banks, etc., as well as the monitor routines that
  allow memory inspection.
* **lib/** -- Library routines that can be used by the kernel and user
  programs.  These files will be included by other top-level files.
* **sys-api/**
  * **pg09-os.exp** -- These are the symbols and equates exported by the
    operating system to user programs.