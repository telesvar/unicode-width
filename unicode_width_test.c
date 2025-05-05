/*
 * Copyright 2025 Dair Aidarkhanov
 * SPDX-License-Identifier: 0BSD
 */

#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

#include <grapheme.h>

#include "unicode_width.h"

/* Helper function to calculate width of a UTF-8 string. */
size_t unicode_width_utf8(const char *str, size_t len) {
  unicode_width_state_t state;
  unicode_width_init(&state);

  size_t width = 0;
  size_t offset = 0;
  uint_least32_t cp;
  size_t bytes;

  while (offset < len) {
    bytes = grapheme_decode_utf8(str + offset, len - offset, &cp);
    if (bytes == 0 || bytes > (len - offset)) {
      break;
    }

    int cp_width = unicode_width_process(&state, cp);
    if (cp_width >= 0) {
      width += cp_width;
    }

    offset += bytes;
  }

  return width;
}

/* Helper for NUL-terminated strings. */
size_t unicode_width_cstr(const char *str) {
  return unicode_width_utf8(str, strlen(str));
}

/* Test case structure. */
typedef struct {
  const char *str;
  const char *desc;
  size_t expected_width;
} test_case_t;

int run_tests(test_case_t *tests, size_t test_count, const char *category) {
  printf("Running %s tests...\n", category);
  int failed = 0;

  for (size_t i = 0; i < test_count; i++) {
    test_case_t *test = &tests[i];
    size_t width = unicode_width_cstr(test->str);

    if (width == test->expected_width) {
      printf("  [%s]: PASS (width = %zu)\n", test->desc, width);
    } else {
      printf("  [%s]: FAIL - Expected %zu, got %zu\n",
             test->desc, test->expected_width, width);

      /* Print string in hex. */
      printf("    Hex: ");
      for (size_t j = 0; j < strlen(test->str); j++) {
        printf("%02X ", (unsigned char)test->str[j]);
      }
      printf("\n");

      failed++;
    }
  }

  printf("%s: %zu tests, %d failures\n\n",
         category, test_count, failed);

  return failed;
}

/* Test individual codepoint processing. */
void test_individual_codepoints() {
  printf("Testing individual codepoint processing...\n");

  struct {
    uint_least32_t cp;
    const char *desc;
    int expected_width;
  } tests[] = {
    {0x0020, "Space", 1},
    {0x0041, "Latin A", 1},
    {0x00A0, "Non-breaking space", 1},
    {0x0301, "Combining accent (zero width)", 0},
    {0x1100, "Hangul Jamo (wide)", 2},
    {0x3000, "Ideographic space (wide)", 2},
    {0x1F600, "Emoji (wide)", 2},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    int width = unicode_width_process(&state, tests[i].cp);

    if (width == tests[i].expected_width) {
      printf("  [U+%04X %s]: PASS (width = %d)\n",
             (unsigned int)tests[i].cp, tests[i].desc, width);
    } else {
      printf("  [U+%04X %s]: FAIL - Expected %d, got %d\n",
             (unsigned int)tests[i].cp, tests[i].desc,
             tests[i].expected_width, width);
      failed++;
    }
  }

  printf("Individual codepoints: %zu tests, %d failures\n\n",
         sizeof(tests) / sizeof(tests[0]), failed);
}

/* Test codepoint sequences with state. */
void test_codepoint_sequences() {
  printf("Testing codepoint sequences with state...\n");

  struct {
    uint_least32_t codepoints[8];
    size_t count;
    const char *desc;
    size_t expected_total_width;
  } tests[] = {
    {{0x0061, 0x0062, 0x0063}, 3, "ASCII 'abc'", 3},
    {{0x0041, 0x0301, 0x0042}, 3, "A + combining accent + B", 2},
    {{0x1F600}, 1, "Emoji (Grinning Face)", 2},
    {{0x1F468, 0x200D, 0x1F469}, 3, "Man ZWJ Woman", 2},
    {{0x1F1FA, 0x1F1F8}, 2, "US flag", 2},
    {{0x000D, 0x000A}, 2, "CR+LF", 1},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    size_t total_width = 0;

    for (size_t j = 0; j < tests[i].count; j++) {
      int width = unicode_width_process(&state, tests[i].codepoints[j]);
      if (width >= 0) {
        total_width += width;
      }
    }

    if (total_width == tests[i].expected_total_width) {
      printf("  [%s]: PASS (total width = %zu)\n",
             tests[i].desc, total_width);
    } else {
      printf("  [%s]: FAIL - Expected total %zu, got %zu\n",
             tests[i].desc, tests[i].expected_total_width, total_width);

      /* Print codepoints in hex. */
      printf("    Codepoints: ");
      for (size_t j = 0; j < tests[i].count; j++) {
        printf("U+%04X ", (unsigned int)tests[i].codepoints[j]);
      }
      printf("\n");

      failed++;
    }
  }

  printf("Codepoint sequences: %zu tests, %d failures\n\n",
         sizeof(tests) / sizeof(tests[0]), failed);
}

/* Test handling ANSI escape sequences. */
void test_ansi_filtering() {
  printf("Demonstration of handling ANSI escape sequences...\n");

  /* Sample text with ANSI escape codes. */
  const char *ansi_text = "\x1B[1;32mGreen\x1B[0m \x1B[3;4;35mItalic+Underline Purple\x1B[0m \x1B[7mInverted\x1B[0m";

  printf("Raw text with ANSI: \"%s\"\n", ansi_text);
  printf("Raw width (including ANSI): %zu\n", unicode_width_cstr(ansi_text));

  /* Filtering of ANSI sequences. */
  char filtered[256] = {};
  size_t filtered_len = 0;
  int in_escape = 0;

  for (size_t i = 0; i < strlen(ansi_text); i++) {
    if (ansi_text[i] == '\x1B') {
      /* Start of escape sequence. */
      in_escape = 1;
    } else if (in_escape && ansi_text[i-1] == '\x1B' && ansi_text[i] == '[') {
      /* Part of CSI sequence. */
      continue;
    } else if (in_escape && ansi_text[i] >= 0x40 && ansi_text[i] <= 0x7E) {
      /* End of escape sequence reached (any letter or symbol). */
      in_escape = 0;
    } else if (!in_escape) {
      /* Only copy if not in an escape sequence. */
      filtered[filtered_len++] = ansi_text[i];
    }
  }
  filtered[filtered_len] = '\0';

  printf("Filtered text: \"%s\"\n", filtered);
  printf("Correct width (after filtering): %zu\n\n", unicode_width_cstr(filtered));
}

int main() {
  int failures = 0;

  printf("Unicode Width Test Program\n");
  printf("==========================\n\n");

  /* Basic ASCII. */
  test_case_t basic_tests[] = {
    {"", "Empty string", 0},
    {"Hello", "ASCII text", 5},
    {"Hello, world!", "ASCII with punctuation", 13},
    {"\t\n\r", "Control characters", 1},
  };
  failures += run_tests(basic_tests, sizeof(basic_tests) / sizeof(basic_tests[0]), "Basic ASCII");

  /* Latin-1 and basic Unicode */
  test_case_t latin_tests[] = {
    {"résumé", "Latin-1 text", 6},
    {"café", "Latin-1 with accent", 4},
    {"ñandú", "Latin-1 with tilde", 5},
    {"ÄÖÜäöüß", "German umlauts", 7},
  };
  failures += run_tests(latin_tests, sizeof(latin_tests) / sizeof(latin_tests[0]), "Latin-1");

  /* Wide and combined characters. */
  test_case_t wide_tests[] = {
    {"你好", "Chinese (wide)", 4},
    {"こんにちは", "Japanese (wide)", 10},
    {"안녕하세요", "Korean (wide)", 10},
    {"ｈｅｌｌｏ", "Fullwidth Latin", 10},
    {"→←↑↓", "Arrows (narrow)", 4},
    {"①②③④", "Circled numbers", 4},
  };
  failures += run_tests(wide_tests, sizeof(wide_tests) / sizeof(wide_tests[0]), "Wide characters");

  /* Emoji. */
  test_case_t emoji_tests[] = {
    {"😀", "Basic emoji", 2},
    {"👨‍👩‍👧", "Family emoji (ZWJ sequence)", 2},
    {"🇺🇸", "Flag emoji", 2},
    {"👍🏼", "Emoji with skin tone", 2},
    {"☕️", "Coffee with emoji presentation", 2},
    {"⚠️", "Warning with emoji presentation", 2},
  };
  failures += run_tests(emoji_tests, sizeof(emoji_tests) / sizeof(emoji_tests[0]), "Emoji");

  /* Special cases. */
  test_case_t special_tests[] = {
    {"\r\n", "CR+LF", 1},
    {"a\u0301", "a + combining accent", 1},
    {"é", "e with acute (precomposed)", 1},
    {"e\u0301", "e + combining accent", 1},
    {"\u200B", "Zero width space", 0},
    {"\u200D", "Zero width joiner", 0},
    {"\u2060", "Word joiner", 0},
  };
  failures += run_tests(special_tests, sizeof(special_tests) / sizeof(special_tests[0]), "Special cases");

  /* Test individual codepoints. */
  test_individual_codepoints();

  /* Test codepoint sequences. */
  test_codepoint_sequences();

  /* Demonstrate ANSI handling. */
  test_ansi_filtering();

  if (failures == 0) {
    printf("All test categories passed!\n");
    return 0;
  } else {
    printf("Total failures: %d\n", failures);
    return 1;
  }
}
