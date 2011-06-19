Tart project coding standards
=============================

C++ code
--------

For the most part, C++ code follows the coding style guidelines
described in the Google open-source C++ style guide:
http://google-styleguide.googlecode.com/svn/trunk/cppguide.xml.

The Tart coding style differs from the Google style in the following ways:

  * C++ files have a ".cpp" extension, not ".cc".
  * The maximum allowed length of a source line is 100 characters.
  * If a line needs to be broken, the next line should be indented exactly 4 spaces.
  * Otherwise, indentation is 2 spaces per indentation level (no TABS).
  * Prefer using LLVM utility classes and containers over their STL equivalents.
  * Source file names use mixed case ("camel case").
  * Constructor initializer lists: If the initializer list spans multiple lines, put the
    comma on the next line under the colon. This makes it easier to insert and delete
    items.
  * Visibility keywords ('public', 'private') are not indented.
  * Pointer type declarations use a space on both sides of the '*'.
  * Case blocks in switch statements always have braces.

Note that not all source files follow these conventions consistently,
work is underway to bring all source files into conformance.

Tart code
---------

