# Chromosome Tool
This is tools to re-assemble chromosome fragments

# Usage

### requirement
- java 8

### Cmd
Just download the jar from bin file.
```bash
Chromosome tools 0.1
Usage: java -jar <path-to-your-jar> [options]

  -i, --in <value>   in is the absolute path of the input FASTA file.
  -o, --out <value>  out is the absolute path of the output file including the result.
  --help             prints this usage text
```

## Doc
No~ There are not many codes. Please read it...

## Development

### requirement
- java 8
- sbt 1.0+

## How to run tests
- run `sbt test`

## How to package a jar
- run `sbt assembly`

## TODO List
- ~~Performance Test: lack of data.~~ Cannot done before because of misunderstanding. Now fixed.
- ~~Support multi-thread~~ Tested the boundary of 50 size and 1000 length in the happiest way.
It seems multi-thread not needed according to the test result.
