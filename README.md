# COMP 3600 Optional Assignment

## Usage of the programs

### Generator

`stack run generate <n> <k?>`

The argument k is optional. If omitted, k will equal n.

### GaleShapley

`stack run galeShapley`

The galeShapley program reads the problem instance from standard input. So running it
on its own will simply give you an empty prompt, as it waits for input.

Rather than run it by itself, you likely want to run the generator, and pipe the result
into galeShapley.

Example:

`stack run generate 5 3 | stack run galeShapley`

Alternatively, you can write the generated input to a file first, so you can verify the
output.

```
stack run generate 5 3 > input.txt
cat input.txt | stack run galeShapley
```
