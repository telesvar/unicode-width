# unicode-width

A C library for calculating the display width of Unicode characters and strings in terminals, following the Unicode East Asian Width specification (UAX #11) and handling emoji sequences correctly.

## Overview

`unicode-width` provides facilities to accurately determine how many terminal columns a Unicode string will occupy when displayed. It handles:

- Basic ASCII and Latin characters
- Wide East Asian characters (CJK)
- Emoji (including emoji sequences, skin tone modifiers, and ZWJ sequences)
- Zero-width characters and combining marks
- Special characters like newlines and control characters

This library is particularly useful for:
- Terminal applications that need to align text
- CLI tools that format tabular data
- Text editors and UI components that need precise character positioning

## Installation

### Using the Library in Your Project

1. Copy these files to your project:
  - `unicode_width.h` - Header file
  - `unicode_width.c` - Implementation

2. Include the header in your code:
  ```c
  #include "unicode_width.h"
  ```

3. You'll also need a Unicode decoding library. The test program uses `grapheme.h`, but you can use any library that provides Unicode decoding.

## Usage

### Basic Usage

```c
#include "unicode_width.h"
#include <stdio.h>

int main() {
  // Initialize state.
  unicode_width_state_t state;
  unicode_width_init(&state);

  // Process individual codepoints.
  int width = unicode_width_process(&state, 'A');  // returns 1
  width = unicode_width_process(&state, 0x4E00);   // returns 2 (wide CJK character)
  width = unicode_width_process(&state, 0x1F600);  // returns 2 (emoji)

  // Reset state.
  unicode_width_reset(&state);

  return 0;
}
```

### Processing Strings

To process a UTF-8 string, decode each codepoint and process it:

```c
size_t unicode_width_utf8(const char *str, size_t len) {
  unicode_width_state_t state;
  unicode_width_init(&state);

  size_t width = 0;
  size_t offset = 0;
  uint_least32_t cp;
  size_t bytes;

  // You'll need a UTF-8 decoding function like this:
  while ((bytes = utf8_decode(str + offset, len - offset, &cp)) > 0) {
    int cp_width = unicode_width_process(&state, cp);
    if (cp_width >= 0) {
      width += cp_width;
    }
    offset += bytes;
  }

  return width;
}
```

### Handling ANSI Escape Sequences

If your strings include ANSI escape sequences (terminal colors/formatting), you'll need to filter them:

```c
// Example of ANSI escape sequence filtering.
void filter_ansi(const char *input, char *output) {
  size_t out_len = 0;
  int in_escape = 0;

  for (size_t i = 0; input[i] != '\0'; i++) {
    if (input[i] == '\x1B') {
      // Start of escape sequence.
      in_escape = 1;
    } else if (in_escape && input[i-1] == '\x1B' && input[i] == '[') {
      // A part of CSI sequence.
      continue;
    } else if (in_escape && input[i] >= 0x40 && input[i] <= 0x7E) {
      // End of escape sequence reached (any letter or symbol).
      in_escape = 0;
    } else if (!in_escape) {
      // Only copy if not in an escape sequence.
      output[out_len++] = input[i];
    }
  }
  output[out_len] = '\0';
}
```

## How It Works

The library implements a state machine that processes Unicode codepoints one at a time, keeping track of context to handle sequences correctly.

Key features:
- Multi-level lookup tables for efficient basic width determination
- State machine to handle complex sequences (emoji, flags, etc.)
- Special case handling for zero-width characters and modifiers
- Complete coverage of Unicode 16.0.0

## Building from Source

The C code is generated from Unicode data files. If you want to regenerate it (e.g., for a newer Unicode version):

1. Ensure you have Python 3 installed (3.12 version or higher)
2. Run the generator script:
  ```
  python3 generate.py
  ```

This will:
- Download Unicode data files as needed
- Generate the lookup tables
- Create `unicode_width.h` and `unicode_width.c`

## License

- **Generated C Code**: [0BSD license](/LICENSE-0BSD)
- **Generation Script**: [Apache 2.0](/LICENSE-APACHE) or [MIT](/LICENSE-MIT) (your choice)
- **Unicode Data**: Used under the [Unicode License Agreement](https://www.unicode.org/license.txt)

## Credits

- Based on the [Rust unicode-width](https://github.com/unicode-rs/unicode-width) crate
- Converted for left-to-right processing in C
- Uses data from the Unicode Character Database

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.
