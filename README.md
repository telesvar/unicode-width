# unicode-width

A C library for calculating text display width in terminals. Accurately handles Unicode characters including CJK, emoji, combining marks, and control characters.

## Features

- Precise width calculation for terminal display
- Support for ASCII, Latin, East Asian wide characters (CJK)
- Proper handling of emoji and complex sequences (ZWJ, skin tones, flags)
- Zero-width characters and combining marks
- Control characters (returns -1, with helper function for caret notation)
- Newlines and special characters

## Installation

Copy `unicode_width.h` and `unicode_width.c` to your project.

You'll also need a Unicode decoder.

Test program uses [libgrapheme](https://libs.suckless.org/libgrapheme/).

## Usage

```c
#include "unicode_width.h"

// Initialize state.
unicode_width_state_t state;
unicode_width_init(&state);

// Process characters and get their widths.
int width = unicode_width_process(&state, 'A');        // 1 column
width = unicode_width_process(&state, 0x4E00);         // 2 columns (CJK)
width = unicode_width_process(&state, 0x1F600);        // 2 columns (emoji)
width = unicode_width_process(&state, 0x0301);         // 0 columns (combining mark)
width = unicode_width_process(&state, '\n');           // 0 columns (newline)
width = unicode_width_process(&state, 0x07);           // -1 (control character)

// Get display width for control characters (e.g., for readline-style display).
int control_width = unicode_width_control_char(0x07);  // 2 columns (^G)

// Reset state.
unicode_width_reset(&state);
```

### Processing UTF-8 Strings

```c
int string_width(const char *str, size_t len) {
  unicode_width_state_t state;
  unicode_width_init(&state);
  int width = 0;

  // Your UTF-8 decoding loop here...
  uint_least32_t cp;
  // For each decoded codepoint:
  int cp_width = unicode_width_process(&state, cp);
  if (cp_width >= 0) {
    width += cp_width;
  } else if (cp_width == -1) {
    // Handle control characters as needed.
    width += unicode_width_control_char(cp);  // or ignore them
  }

  return width;
}
```

## Building from Source

The C code is generated from Unicode data files (v16.0.0).

To regenerate (requires Python version 3.12 or higher):

```sh
python3 generate.py
```

## License

- **Generated C Code**: [0BSD license](/LICENSE-0BSD)
- **Generation Script**: [Apache 2.0](/LICENSE-APACHE) or [MIT](/LICENSE-MIT)
- **Unicode Data**: [Unicode License Agreement](https://www.unicode.org/license.txt)

## Credits

Based on the [Rust unicode-width](https://github.com/unicode-rs/unicode-width) crate and adapted for C with left-to-right processing.
