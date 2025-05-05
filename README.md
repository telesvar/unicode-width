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
unicode_width_reset(&state);

width = unicode_width_process(&state, 0x4E00);         // 2 columns (CJK)
unicode_width_reset(&state);

width = unicode_width_process(&state, 0x1F600);        // 2 columns (emoji)
unicode_width_reset(&state);

width = unicode_width_process(&state, 0x0301);         // 0 columns (combining mark)
unicode_width_reset(&state);

width = unicode_width_process(&state, '\n');           // 0 columns (newline)
unicode_width_reset(&state);

width = unicode_width_process(&state, 0x07);           // -1 (control character)
unicode_width_reset(&state);

// Get display width for control characters (e.g., for readline-style display).
int control_width = unicode_width_control_char(0x07);  // 2 columns (^G)
```

### Processing Unicode Strings

You can use [libgrapheme](https://libs.suckless.org/libgrapheme/),
[utf8proc](https://github.com/JuliaStrings/utf8proc),
or any other Unicode decoding solutions.

```c
int string_width(const char *str, size_t len) {
  unicode_width_state_t state;
  unicode_width_init(&state);
  int width = 0;

  // Your Unicode decoding loop here...
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

Using libgrapheme:

```c
#include <stddef.h>
#include <stdint.h>

#include <grapheme.h>

#include "unicode_width.h"

/* Calculate the display width of a UTF-8 encoded string.
 *
 * @param str The UTF-8 encoded string
 * @param len Length of the string in bytes
 * @param show_control Whether to count control characters (as ^X notation)
 * @return The display width in terminal columns
 */
int string_width(const char *str, size_t len, int show_control) {
  if (str == NULL || len == 0) {
    return 0;
  }

  unicode_width_state_t state;
  unicode_width_init(&state);
  int width = 0;
  size_t offset = 0;
  size_t bytes_read;
  uint_least32_t cp;

  /* Process each codepoint in the string. */
  while (offset < len) {
    bytes_read = grapheme_decode_utf8(str + offset, len - offset, &cp);

    if (bytes_read == 0 || bytes_read > (len - offset)) {
      /* Invalid sequence or unexpected end - stop processing. */
      break;
    }

    int cp_width = unicode_width_process(&state, cp);
    if (cp_width >= 0) {
      /* Normal character with width. */
      width += cp_width;
    } else if (cp_width == -1 && show_control) {
      /* Control character, show as ^X if requested. */
      width += unicode_width_control_char(cp);
    }
    /* Else is control character that we're not showing. */

    offset += bytes_read;
  }

  return width;
}
```

## Building from Source

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
