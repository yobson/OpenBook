# OpenBook
OpenBook is a command line application to assist with open book exams.

## Usage
When it is completed, you will simply run `OpenBook [filename]` where the file is
a store of all the data you might want to look up in the exam.

When the app is running, 'ESC' closes the app, type to search and arrow keys to select a hit.
searching can be more focused with the following rules:
- Typing anything will simply search through names of the entries.
- `def text` will search for definitions
- `eqn text` will search for equations relating to text
- `opt text` will search for options, for example, in a computer security exam `opt attacks` would give me the definitions of differnt type of cyber attack
- `txt text` will search the body of data entries
- `reg text` will do the same but text is a regular expression
All searches are case insensitive

## Inputing data
not implemented

## TODO List
As you can see, not much has been implemented yet
- [X] Build UI
- [ ] Design file format and parser for text
- [ ] Implement search utility
