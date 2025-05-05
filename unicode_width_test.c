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

/* Test case structure. */
typedef struct {
  const char *str;
  const char *desc;
  int expected_width;
} test_case_t;

int run_tests(test_case_t *tests, size_t test_count, const char *category) {
  printf("Running %s tests...\n", category);
  int failed = 0;

  for (size_t i = 0; i < test_count; i++) {
    test_case_t *test = &tests[i];

    /* Calculate width, treating -1 special characters as width 0. */
    int width = 0;
    unicode_width_state_t state;
    unicode_width_init(&state);

    for (size_t j = 0; j < strlen(test->str); j++) {
      unsigned char c = test->str[j];
      /* Handle UTF-8 sequences. */
      if (c < 128) {
        int char_width = unicode_width_process(&state, c);
        if (char_width > 0) {
          width += char_width;
        }
        /* Skip control characters (-1). */
      } else {
        /* For multi-byte characters, use grapheme library. */
        uint_least32_t cp;
        size_t bytes = grapheme_decode_utf8(test->str + j, strlen(test->str) - j, &cp);
        if (bytes > 1) {
          int char_width = unicode_width_process(&state, cp);
          if (char_width > 0) {
            width += char_width;
          }
          j += bytes - 1;
        }
      }
    }

    if (width == test->expected_width) {
      printf("  [%s]: PASS (width = %d)\n", test->desc, width);
    } else {
      printf("  [%s]: FAIL - Expected %d, got %d\n",
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

/* Test control characters handling. */
void test_control_char_width() {
  printf("Testing control character width function...\n");

  struct {
    uint_least32_t cp;
    const char *desc;
    int expected_width;
  } tests[] = {
    {0x01, "SOH (Start of Heading)", 2},
    {0x07, "BEL (Bell)", 2},
    {0x1B, "ESC (Escape)", 2},
    {0x7F, "DEL", 2},
    {0x90, "DCS (Device Control String)", 4},
    {0x41, "A (not a control char)", -1},
    {0x0A, "LF (handled separately)", -1},
    {0x0D, "CR (handled separately)", -1},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    int width = unicode_width_control_char(tests[i].cp);

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

  printf("Control character width: %zu tests, %d failures\n\n",
         sizeof(tests) / sizeof(tests[0]), failed);
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
    int expected_total_width;
  } tests[] = {
    {{0x0061, 0x0062, 0x0063}, 3, "ASCII 'abc'", 3},
    {{0x0041, 0x0301, 0x0042}, 3, "A + combining accent + B", 2},
    {{0x1F600}, 1, "Emoji (Grinning Face)", 2},
    {{0x1F468, 0x200D, 0x1F469}, 3, "Man ZWJ Woman", 2},
    {{0x1F1FA, 0x1F1F8}, 2, "US flag", 2},
    {{0x000D, 0x000A}, 2, "CR+LF", 0},
    {{0x000A, 0x000D}, 2, "LF+CR", 0},
    {{0x0007, 0x0007}, 2, "BEL+BEL (control chars)", 0},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    int total_width = 0;

    for (size_t j = 0; j < tests[i].count; j++) {
      int width = unicode_width_process(&state, tests[i].codepoints[j]);
      if (width >= 0) {
        total_width += width;
      }
      /* Ignoring -1 (control chars) in this test. */
    }

    if (total_width == tests[i].expected_total_width) {
      printf("  [%s]: PASS (total width = %d)\n",
             tests[i].desc, total_width);
    } else {
      printf("  [%s]: FAIL - Expected total %d, got %d\n",
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

void test_mixed_sequences() {
  printf("Testing mixed control and display character sequences...\n");

  struct {
    uint_least32_t codepoints[10];
    size_t count;
    const char *desc;
    /* Expected width when control chars are displayed as ^X. */
    int expected_width_caret;
    /* Expected width when control chars are ignored. */
    int expected_width_ignore;
  } tests[] = {
    {{0x0061, 0x0007, 0x0062}, 3, "a BEL b", 4, 2},  /* a (1) + ^G (2) + b (1) = 4 */
    {{0x000D, 0x000A, 0x0063}, 3, "CRLF c", 1, 1},   /* CRLF (0) + c (1) = 1 */
    {{0x0009, 0x0041, 0x0042}, 3, "TAB A B", 4, 2},  /* ^I (2) + A (1) + B (1) = 4 */
    {{0x001B, 0x005B, 0x0041}, 3, "ESC [ A", 4, 2},  /* ^[ (2) + [ (1) + A (1) = 4 */
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    int width_ignore = 0;
    int width_caret = 0;

    for (size_t j = 0; j < tests[i].count; j++) {
      int width = unicode_width_process(&state, tests[i].codepoints[j]);

      if (width >= 0) {
        /* Normal character with width. */
        width_ignore += width;
        width_caret += width;
      } else if (width == -1) {
        /* Ignore or use caret width. */
        int control_width = unicode_width_control_char(tests[i].codepoints[j]);
        if (control_width >= 0) {
          width_caret += control_width;
        }
      }
    }

    /* Check both methods. */
    int ignore_pass = (width_ignore == tests[i].expected_width_ignore);
    int caret_pass = (width_caret == tests[i].expected_width_caret);

    if (ignore_pass && caret_pass) {
      printf("  [%s]: PASS (ignore: %d, caret: %d)\n",
             tests[i].desc, width_ignore, width_caret);
    } else {
      printf("  [%s]: FAIL - Expected (ignore: %d, caret: %d), got (ignore: %d, caret: %d)\n",
             tests[i].desc,
             tests[i].expected_width_ignore, tests[i].expected_width_caret,
             width_ignore, width_caret);
      failed++;
    }
  }

  printf("Mixed sequences: %zu tests, %d failures\n\n",
         sizeof(tests) / sizeof(tests[0]), failed);
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
    {"\t\n\r", "Control characters", 0},
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
    {"\r\n", "CR+LF", 0},
    {"\n\r", "LF+CR", 0},
    {"a\u0301", "a + combining accent", 1},
    {"é", "e with acute (precomposed)", 1},
    {"e\u0301", "e + combining accent", 1},
    {"\u200B", "Zero width space", 0},
    {"\u200D", "Zero width joiner", 0},
    {"\u2060", "Word joiner", 0},
  };
  failures += run_tests(special_tests, sizeof(special_tests) / sizeof(special_tests[0]), "Special cases");

  /* Test control characters handling. */
  test_control_char_width();

  /* Test individual codepoints. */
  test_individual_codepoints();

  /* Test codepoint sequences. */
  test_codepoint_sequences();

  /* Test mixed sequences with control characters. */
  test_mixed_sequences();

  if (failures == 0) {
    printf("All test categories passed!\n");
    return 0;
  } else {
    printf("Total failures: %d\n", failures);
    return 1;
  }
}
