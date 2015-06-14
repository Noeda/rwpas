TODO List
=========

Field of view and drawing is very inefficient. Could be fixed by implementing
these parts in C.

Unicode is not rendered correctly. We'll only be fine as long as any code point
we use occupies exactly 1 column in the terminal. Fixable by using unicode
strings in cells instead of single characters.

