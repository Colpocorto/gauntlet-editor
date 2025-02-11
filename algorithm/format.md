LEVEL FORMAT ANALYSIS
=====================

#d000   ld  a,(iy+#4e)  ;#80 first 7 / #c0 additional blocks

#d01a   <= is #c0
when it is #80
    (#iy+#4d) <= former ID
    (#iy+#4e) <= #c0 ;prepare for next loading

    move #d0b8 -> #c800 (#800)
    ret
#d01a when it is #c0

;----
#d078 
    #8500
        #b4e1  get A=RND (0-255), self-update seed at #b4e4
        Gets a random number between 0-7, check the value is not equal to C, D or E
;----
#d087
    #8500
        Get a value between 8-9

;----
;input: A
#D08F

REPEAT A:
    BC <= (#D0A1+A*2)   ;offset
    #D0B8 += BC
RET HL

;---------------        

if bit 6, (IY-1) == 0 then
    #D06C = RND (0-7)   (LD A,X)

    if (IY+3B) >= 4
        #D0B7 = RND (0-7)   
    else
        #D0B7 = RND (8-9)  (treasure room)
else
    #D06C = RND (8-9) (LD A,X)
    #D0B7 = RND (0-7)


#D0B6 = RND (0-7) 
#D0B5 = RND (0-7)

Basically, selects 4 random values in #D0B5, 6, 7, #D06B


IX <= #D0B5
A <= (IX)

GET HL pointer for A (0-7) (CALL #D08F)
    for A = {(#D0B5), (#D0B6), (#D0B7)}

    copy them sequentially to #C800
GET one more from #D08F (value from #D06C) -> This last one is the actual next level,
and it isn't copied to #c800.

ENEMIES TABLE
=============

It's placed at #5c00.
Struct for each enemy is:

    +00 X   COORD
    +01 Y   COORD
    +02 enemy type and strength EEE000SS
    +03 Direction?  Check

Total number of enemies is stored at (IY+#17)

STRUCTURE OF BLOCK
==================

(Note: in some versions, data starts at #D0AE instead of #D0A1)

Each block is loaded at #D000. Data preparation routines are at
#D000-#D0A0.

After a block is loaded, the program chooses 4 out of 10 mazes (numbered
from 0-9). Treasure rooms are always maze numbers 8 and 9. A treasure room
can or can not be selected. Only one treasure room as maximum can be chosen.

Randomly chosen levels are stored at:

#D06C (next maze)
#D0B5
#D0B6
#D0B7


Offset:
#D0A1   MAZE 0 SIZE
#D0A3   MAZE 1 SIZE
#D0A5   MAZE 2 SIZE
#D0A7   MAZE 3 SIZE
#D0A9   MAZE 4 SIZE
#D0AB   MAZE 5 SIZE
#D0AD   MAZE 6 SIZE
#D0AF   MAZE 7 SIZE
#D0B1   MAZE 8 SIZE (TREASURE ROOM)
#D0B3   MAZE 9 SIZE (TREASURE ROOM)
..
+D0B8   MAZE 0 DATA START



STRUCTURE OF MAZE
==================

Offset:                             

+00   Maze size in bytes (8 less significant bits. The MSB bit is stored at bit 7 of offset +02)
+01   WVxxxPSH
            W   =   horizontal wrap maze (no wall)
            V   =   vertical wrap maze (wall)
            P   =   remove all exits but one?
            S   =   shot can stun other players
            H   =   shot can hurt other players
  
+02   LxCCCxxx    => 
            CCC = style (colour scheme + brick pattern)
                0   ->  6, A,     chess   pattern   (red, yellow)       *
                1   ->  C, 3,     brick   pattern   (dark green, green) *
                2   ->  4, 7,     diamond pattern   (blue, light blue)  *
                3   ->  5, D,     chess   pattern   (blue, magenta)     *
                4   ->  9, A,     brick   pattern   (light red, yellow) *
                5   ->  E, F,     diamond pattern   (grey, white)       *
                6   ->  6, A,     diamond pattern   (red, yellow)       *
                7   ->  9, A,     diamond pattern   (light red, yellow) *

            L = MSB of size stored at +00

+03         WALL DE ITION BLOCK SIZE (max 255)
+04         WALL DEFINITION DATA
+(size of previous block)
            OBJECT DEFINITION BLOCK DATA

The size of the last block is calculated by substracting "MAZE SIZE" minus offset where the last block starts.


[WALL DEFINITION BLOCK]
=======================
This block contains a list of "commands" as described below.

Available commands for wall drawing:

* SET_ORIGIN (2-byte) AND TYPE OF BLOCK (WALL/GATE)
        
        Example:

        #E8 #E6 ->  set start of drawing. Block coordinate col,row: #08, #06 to 
                    set walls. This command also sets a block on the origin spot.
                    Type of identified blocks:
                    #E0 ->  wall
                    #C0 ->  trap bound wall 
                    #80 ->  gate (starting horizontal block)
                    #40 ->  gate (starting vertical block)
                    #20 ->  exit block
                    In case more than two blocks share 3MSB in a row, the last two are a new
                    origin while the previous ones are drawing commands. This has an implication:
                    the first drawing command can't be #C0 or #E0 (LEFT and UP-RIGHT).
                    ^^^^ Can't happen in practice since does not make sense a stroke longer than 32 bytes
* DRAW STEPS (1-byte)
        Example:

        #82 #42 ->  3MSB are "draw" directions taken from the LUT at while the 5LSB
                    seem to be a counter for the number of steps. Since 5 bits are used to set
                    the number of steps, up to 32 (#1F+1) are possible in one command. This is
                    why two consecutive 3MSB equal numbers are identified as a new center
                    to start drawing.
                    Direction lookup table:

                    #0x -> UP
                    #2x -> UP-LEFT
                    #4x -> RIGHT
                    #6X -> DOWN-RIGHT
                    #8x -> DOWN
                    #Ax -> DOWN-LEFT
                    #Cx -> LEFT
                    #Ex -> UP-RIGHT

[OBJECT DEFINITION BLOCK]
=========================

Possible commands for block definition:

* Advance MAZE_BUF cursor. MSB + 1 + number of positions to advance (lineary). (the counter is increased by one to adjust the range to 1-128)
* Iter      #nn    repeat the former object #nn times (N<=#12>).
* Object.   #NN    object number (from list [PATTERN LIST], N>#12). Cursor is advanced to the next position.   
* Charpos.  #3F    character position? CHECK   00111111
* Enemy.    #40+   enemy. Bits 543 are the number of enemy:
                #40 000 Ghost * 3
                #48 001 Grunt * 3
                #50 010 Demon * 3
                #58 011 Lobber * 3
                #60 100 Sorcerer * 3
                #68 101 Death
            
            bits 10 might be enemy's power CHECK

PATTERN LIST    
============

(number on the right i.e. MSB set, means the block would disappear when a trap is touched).

NOTE: blocks from 01 to 10 are automatically placed by the game when "drawing" walls in the first block.


00  Empty space     

01  | | 81
    +-+

02  +-- 82
    |
    +--

03  |   83
    +--

04  +-+ 84
    | |

05  | | 85

06  +-- 86
    |

07  |   87
    +--
    |

08  --+ 88
      |
    --+

09    | 89
    --+

0A  --- 8A
    ---

0B   |  8B
    -+-

0C  --+ 8C
      |  

0D    | 8D
    --+
      |  

0E  -+- 8E
     |

0F   |  8F
    -+-
     |

10  | | 90
    +-+

11  horizontal gate             91
12  vertical gate               92
13  treasure                    93
14  cider                       94
15  ham                         95
16  potion                      96
17  undestructible potion       97
18  amulet                      98
19  extra armour potion         99
1A  extra pickup power potion   9A
1B  extra magic power potion    9B
1C  extra shot power potion     9C
1D  extra shot speed potion     9D
1E  extra fight power potion    9E
1F  key                         9F
20  ghost generator level 1     A0
21  ghost generator level 2     A1
22  ghost generator level 3     A2
23  grunts generator level 1    A3
24  grunts generator level 2    A4
25  grunts generator level 3    A5
26  demon generator level 1     A6
27  demon generator level 2     A7
28  demon generator level 3     A8
29  lobber generator level 1    A9
2A  lobber generator level 2    AA
2B  lobber generator level 3    AB
2C  sorcerer generator level 1  AC
2D  sorcerer generator level 2  AD
2E  sorcerer generator level 3  AE
2F  trap                        AF
30  transporter                 B0
31  spoiled cider               B1
32  many keys                   B2
33  destructible wall level 3   B3
34  destructible wall level 2   B4
35  destructible wall level 1   B5
36  EXIT                        B6
37  EXIT TO 4                   B7
38  EXIT TO 8                   B8
