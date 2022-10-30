# Nemesis

Nemesis is a simplistic Forth single pass compiler written in OCaml using Menhir.
As a compiler, it does not process immediate words, these are handled explicitly in OCaml
code. This requires a slightly more complex grammar to prevent executing immediate words
when they are defined.

## Forths

Nemesis comes with an STC Forth for Cortex M0 chips, and a DTC Forth for MSP430 chips.

## Usage

See `nemesis --help` on how to use the compiler. The compiler produces a raw binary file
which can be flashed using e.g `stm32flash` for STM32 chips.