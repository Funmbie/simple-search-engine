# Simple Search Engine
The purpose of this exercise is to implement a command line driven search engine.

## Usage
Start the engine using the ./data/index_text.txt file:
> sbt

> runMain org.SimpleSearch.Main data

You can also test other text files by changing the data folder to your preferred folder.
Enter your search keyword:
> search>

Once you are done searching, exit the engine
> search>:quit

## Tests
Run tests:
> sbt testOnly org.SimpleSearch.MainSpec

