# AI - Home work 1

This project is made for a course of artificial intelligence. It implements an A* algorythm on two problems. The first take a map in input, and output the shortest path. The second is a sokoban solver. It take a sokoban map in input and output the shortest path. Heuristics have been developed for the sokoban and will be explained later in this document. The program had to solve the problems in under 30 seconds and use at max 2 gb of memory.

## Installation

`$ lein uberjar`

## Usage

Map shortest path:
    `$ time sudo nice --20 java -Xmx2g -XX:+AggressiveOpts -jar target/uberjar/hw1-0.0.1-standalone.jar hw1.uqam_entry %@`
Where %@ is the path to a valid map (eg. 'resources/uqam-map-1.txt').

Sokoban solver:
    `$ time sudo nice --20 java -Xmx2g -XX:+AggressiveOpts -jar target/uberjar/hw1-0.0.1-standalone.jar hw1.sokoban_entry %@`
Where %@ is the path to a valid sokoban map (eg. 'resources/test/sokoban00.txt').

## License

See LICENSE.txt for license.
