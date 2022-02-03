COMP 3600 Optional Assignment

Daniel Power
201244498

# Instructions for Notebook/written portion of assignment
The Notebook server should already be running. Navigate to this url:
http://192.53.121.143:9000

You will be prompted for a password. Use "kolokolova"


# Instructions for building and manual usage of the programs

## Connect to the server

Since none of the LabNet machines had either ghci (the Haskell compiler) or stack
(the Haskell build system/package manager), I figured the easiest way to give you
access was to setup a server that is pre-configured for you. So you can ssh into it
the same way you do to access Garfield, Intrepid, etc.

To access the server, use the following command:

`ssh root@192.53.121.143`

You will be prompted for a password. Use "kolokolova"

You should then navigate to the `galeShapley` directory, as that is where all the
relevant files are.

`cd galeShapley`

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
