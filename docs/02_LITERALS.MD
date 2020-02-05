Literals
========

* [Integer literals](#integer-literals)
* [ASM String literals](#asm-string-literals)
* [C String literals](#c-string-literals)
* [Examples](#examples)


Integer literals
----------------

QEVAL allows you to specify the base of the integer using a suffix:

 * **D** or **T** for decimal
 * **H** or **X** for hexadecimal
 * **B** or **Y** for binary
 * **O** or **Q** for octal

~~~
 DEC = 9124
 DEC = 09124d
 DEC = 09124t
 HEX = 2C7Fh
 HEX = 2C7Fx
 BINARY = 10001010b
 BINARY = 10001010y
 OCTAL = 7511o
 OCTAL = 7511q
~~~

QEVAL also allows specify the base using a prefix:

 * **0d** or **0t** for decimal
 * **0x** for hexadecimal (in the style of C)
 * **0h** for hexadecimal
 * **0b** or **0y** for binary.
 * **0o** or **0q** for octal

~~~    
 DEC = 0d9124
 HEX = 0x2C7F 
 HEX = 0h817f
 BINARY = 0b10001010
 OCTAL = 0o7511
~~~

Hexadecimal integer can be written with the **#** or **$** prefix in the style of Borland Pascal or Motorola Assemblers.

~~~
 HEX = #F71A
 HEX = $AC49
~~~


QEVAL will treat any number beginning with a zero as octal, this behavior can be disabled by specifying the '-decimalzeroprefix' option

~~~
 OCTAL = 06127  ; <- decimal when '-decimalzeroprefix'
~~~


Integer literals can have underscores (_) to break up long numbers, this is especially useful in binary literals where the bits have grouped meaning.

~~~
 BREAKED_BINARY = 1_10110_01000_00101b
 BREAKED_HEX = #F_C72
~~~


ASM String Literals
-------------------

The simplest way to specify a string is using assembler string literals,
these consist of a series of characters or spaces enclosed in single quotes.

To escape a single quote, use two single quotes in a row. That will be the only escape sequence interpreted within these single-quoted strings.

~~~
 ASM_STRING = 'hello world!'
 ASM_STRING = 'this is the ''best'' world!'
~~~


C String Literals
-----------------

If the string is enclosed in double-quotes then it will be processed like a C string, it means that QEVAL will interpret several escape sequences for special characters.

~~~
; Escape sequences supported by C string literals:
;   \a = #07 alert (Beep, Bell)
;   \b = #08 backspace
;   \e = #1B escape character
;   \f = #0C formfeed page break
;   \n = #0A newline (line feed)
;   \r = #0D carriage return
;   \t = #09 horizontal tab
;   \v = #0B vertical tab
;   \\ = #5C backslash
;   \' = #27 single quotation mark
;   \" = #22 double quotation mark
;   \nnn = a char corresponding to value 'nnn' interpreted as octal
;   \xnn = a char corresponding to value 'nn' interpreted as hexadecimal

C_STRING = "hello world!\r\n"
C_STRING = "this is the \"best\" world!"
C_STRING = "the arroba character: \x7C"

~~~


Examples
--------
~~~
     ; Some integer examples (all are equivalent):
     VALUE = 200          ; decimal
     VALUE = 0200         ; decimal
     VALUE = 0200d        ; explicitly decimal
     VALUE = 0d200        ; explicitly decimal
     VALUE = 0c8h         ; hex
     VALUE = $0c8         ; hex
     VALUE = 0xc8         ; hex
     VALUE = 0hc8         ; hex
     VALUE = 310o         ; octal
     VALUE = 310q         ; octal
     VALUE = 0o310        ; octal
     VALUE = 0q310        ; octal
     VALUE = 11001000b    ; binary
     VALUE = 1100_1000b   ; binary with grouped bits
     VALUE = 1100_1000y   ; binary with grouped bits
     VALUE = 0b1100_1000  ; binary with grouped bits
     VALUE = 0y1100_1000  ; binary with grouped bits
~~~
