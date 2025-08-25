/*
 * Copyright 2025 Dair Aidarkhanov
 * SPDX-License-Identifier: 0BSD
 */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <grapheme.h>

#include "unicode_width.h"

/* Test case structure. */
typedef struct {
  const char *str;
  const char *desc;
  int expected_width;
} test_case_t;

/* Return 1 if cp is a control char that unicode_width_process would treat as
 * -1. */
static int is_control_codepoint(uint_least32_t cp) {
  if (cp < 0x20 && cp != 0x0A && cp != 0x0D)
    return 1; /* C0 except LF/CR */
  if (cp == 0x7F)
    return 1; /* DEL */
  if (cp >= 0x80 && cp <= 0x9F)
    return 1; /* C1 */
  return 0;
}

int run_tests(test_case_t *tests, size_t test_count, const char *category) {
  printf("Running %s tests...\n", category);
  int failed = 0;

  for (size_t i = 0; i < test_count; i++) {
    test_case_t *test = &tests[i];

    /* Calculate width. Controls (-1) are ignored; FE0E adjustments (-1) are
     * applied. */
    int width = 0;
    unicode_width_state_t state;
    unicode_width_init(&state);

    size_t len = strlen(test->str);
    size_t j = 0;

    while (j < len) {
      unsigned char b0 = (unsigned char)test->str[j];

      if (b0 < 128) {
        /* ASCII fast path. */
        int char_width = unicode_width_process(&state, (uint_least32_t)b0);
        if (char_width >= 0) {
          width += char_width;
        }
        /* else: control char => ignore */
        j += 1;
      } else {
        /* Multi-byte UTF-8. */
        uint_least32_t cp = 0;
        size_t bytes = grapheme_decode_utf8(test->str + j, len - j, &cp);
        if (bytes == 0 || bytes > (len - j)) {
          /* Invalid sequence; stop to avoid infinite loop. */
          break;
        }

        int char_width = unicode_width_process(&state, cp);
        if (char_width >= 0) {
          width += char_width;
        } else {
          /* Distinguish real controls from negative adjustments (e.g., FE0E).
           */
          int ctrl = unicode_width_control_char(cp);
          if (ctrl < 0 && !is_control_codepoint(cp)) {
            /* Negative adjustment (e.g., VS15): apply it. */
            width += char_width; /* char_width is negative here */
          }
          /* Otherwise: control char => ignore in this test mode. */
        }

        j += bytes;
      }
    }

    if (width == test->expected_width) {
      printf("  [%s]: PASS (width = %d)\n", test->desc, width);
    } else {
      printf("  [%s]: FAIL - Expected %d, got %d\n", test->desc,
             test->expected_width, width);

      /* Print string in hex. */
      printf("    Hex: ");
      for (size_t k = 0; k < strlen(test->str); k++) {
        printf("%02X ", (unsigned char)test->str[k]);
      }
      printf("\n");

      failed++;
    }
  }

  printf("%s: %zu tests, %d failures\n\n", category, test_count, failed);

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
      printf("  [U+%04X %s]: PASS (width = %d)\n", (unsigned int)tests[i].cp,
             tests[i].desc, width);
    } else {
      printf("  [U+%04X %s]: FAIL - Expected %d, got %d\n",
             (unsigned int)tests[i].cp, tests[i].desc, tests[i].expected_width,
             width);
      failed++;
    }
  }

  printf("Control character width: %zu tests, %d failures\n\n",
         sizeof(tests) / sizeof(tests[0]), failed);
}

/* Test individual codepoints. */
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
      {0x1100, "Hangul Jamo Leading (wide on this table)", 2},
      {0x3000, "Ideographic space (wide)", 2},
      {0x1F600, "Emoji (Grinning Face)", 2},
      {0xFF21, "Fullwidth Latin 'Ａ'", 2},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    int width = unicode_width_process(&state, tests[i].cp);

    if (width == tests[i].expected_width) {
      printf("  [U+%04X %s]: PASS (width = %d)\n", (unsigned int)tests[i].cp,
             tests[i].desc, width);
    } else {
      printf("  [U+%04X %s]: FAIL - Expected %d, got %d\n",
             (unsigned int)tests[i].cp, tests[i].desc, tests[i].expected_width,
             width);
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
      {{0x0041, 0x200D, 0x0042}, 3, "A ZWJ B (no emoji join)", 2},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    int total_width = 0;

    for (size_t j = 0; j < tests[i].count; j++) {
      int w = unicode_width_process(&state, tests[i].codepoints[j]);
      if (w >= 0) {
        total_width += w;
      }
    }

    if (total_width == tests[i].expected_total_width) {
      printf("  [%s]: PASS (total width = %d)\n", tests[i].desc, total_width);
    } else {
      printf("  [%s]: FAIL - Expected total %d, got %d\n", tests[i].desc,
             tests[i].expected_total_width, total_width);

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
      {{0x0061, 0x0007, 0x0062}, 3, "a BEL b", 4, 2},
      {{0x000D, 0x000A, 0x0063}, 3, "CRLF c", 1, 1},
      {{0x0009, 0x0041, 0x0042}, 3, "TAB A B", 4, 2},
      {{0x001B, 0x005B, 0x0041}, 3, "ESC [ A", 4, 2},
  };

  int failed = 0;
  for (size_t i = 0; i < sizeof(tests) / sizeof(tests[0]); i++) {
    unicode_width_state_t state;
    unicode_width_init(&state);

    int width_ignore = 0;
    int width_caret = 0;

    for (size_t j = 0; j < tests[i].count; j++) {
      uint_least32_t cp = tests[i].codepoints[j];
      int w = unicode_width_process(&state, cp);

      if (w >= 0) {
        width_ignore += w;
        width_caret += w;
      } else {
        int caret = unicode_width_control_char(cp);
        if (caret >= 0) {
          width_caret += caret;
        }
      }
    }

    int ignore_pass = (width_ignore == tests[i].expected_width_ignore);
    int caret_pass = (width_caret == tests[i].expected_width_caret);

    if (ignore_pass && caret_pass) {
      printf("  [%s]: PASS (ignore: %d, caret: %d)\n", tests[i].desc,
             width_ignore, width_caret);
    } else {
      printf("  [%s]: FAIL - Expected (ignore: %d, caret: %d), got (ignore: "
             "%d, caret: %d)\n",
             tests[i].desc, tests[i].expected_width_ignore,
             tests[i].expected_width_caret, width_ignore, width_caret);
      failed++;
    }
  }

  printf("Mixed sequences: %zu tests, %d failures\n\n",
         sizeof(tests) / sizeof(tests[0]), failed);
}

/* New: Variation selector sequences, keycaps, more ZWJ, RIs, fullwidth,
 * combining. */
void test_variation_sequences() {
  printf("Testing variation selector sequences (VS15/VS16) and keycaps...\n");

  test_case_t tests[] = {
      {"©", "Copyright (text default)", 1},
      {"©️", "Copyright + VS16 (emoji)", 2},
      {"©︎", "Copyright + VS15 (text)", 1},

      {"✈", "Airplane (text default)", 1},
      {"✈️", "Airplane + VS16 (emoji)", 2},
      {"✈︎", "Airplane + VS15 (text)", 1},

      {"™", "TM sign (text default)", 1},
      {"™️", "TM + VS16 (emoji)", 2},

      {"1️⃣", "Keycap digit 1", 2}, /* 1 + FE0F + 20E3 */
      {"#️⃣", "Keycap #", 2},       /* # + FE0F + 20E3 */
      {"*️⃣", "Keycap *", 2},       /* * + FE0F + 20E3 */
  };

  (void)run_tests(tests, sizeof(tests) / sizeof(tests[0]),
                  "Variation selectors");
}

void test_zwj_sequences() {
  printf("Testing ZWJ sequences (emoji clusters and non-emoji)...\n");

  test_case_t tests[] = {
      {"👨‍👩‍👧", "Family (ZWJ sequence)", 2},
      {"🧑‍🌾x", "Farmer + x (ZWJ then ASCII)", 3},
      {"👩‍🦰", "Woman: red hair (ZWJ sequence)", 2},
      {"👨‍🦯", "Man with white cane (ZWJ sequence)", 2},
      {"🙂‍🙂", "Smiley ZWJ Smiley (non-standard, join)", 2},
      {"🙂‍a", "Smiley ZWJ 'a' (no join with ASCII)", 3},
      {"A‍B", "ASCII A ZWJ B (no emoji join)", 2},
      {"🏳️‍🌈", "Rainbow flag (ZWJ + VS16 inside)", 2},
  };

  (void)run_tests(tests, sizeof(tests) / sizeof(tests[0]), "ZWJ sequences");
}

void test_fullwidth_and_cjk() {
  printf("Testing fullwidth forms and CJK...\n");

  test_case_t tests[] = {
      {"ＡＢＣ！", "Fullwidth ABC!", 8},
      {"你好", "Chinese (wide)", 4},
      {"こんにちは", "Japanese (wide)", 10},
      {"안녕하세요", "Korean (wide)", 10},
  };

  (void)run_tests(tests, sizeof(tests) / sizeof(tests[0]), "Fullwidth/CJK");
}

void test_regional_indicator_runs() {
  printf("Testing Regional Indicator (RI) runs...\n");

  test_case_t tests[] = {
      {"🇦🇧", "RI pair", 2},
      {"🇦🇧🇨", "RI triple", 3},
      {"🇦🇧🇨🇩", "RI quadruple", 4},
      {"🇺🇸🇨🇦", "US + Canada flags", 4},
  };

  (void)run_tests(tests, sizeof(tests) / sizeof(tests[0]),
                  "Regional Indicator runs");
}

void test_combining_sequences() {
  printf("Testing combining mark sequences...\n");

  test_case_t tests[] = {
      {"á̧", "a + acute + cedilla", 1},     /* U+0061 U+0301 U+0327 */
      {"áb̧", "a + acute; b + cedilla", 2}, /* U+0061 U+0301 U+0062 U+0327 */
      {"é", "e + acute", 1},               /* decomposed */
      {"é", "e with acute (precomposed)", 1},
  };

  (void)run_tests(tests, sizeof(tests) / sizeof(tests[0]),
                  "Combining sequences");
}

void test_hangul_decomposed() {
  printf("Testing decomposed Hangul...\n");

  test_case_t tests[] = {
      {"가", "L U+1100 + V U+1161 => '가'", 2},
      {"한", "L U+1112 + V U+1161 + T U+11AB => '한'", 2},
  };

  (void)run_tests(tests, sizeof(tests) / sizeof(tests[0]),
                  "Hangul (decomposed)");
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
  failures += run_tests(
      basic_tests, sizeof(basic_tests) / sizeof(basic_tests[0]), "Basic ASCII");

  /* Latin-1 and basic Unicode */
  test_case_t latin_tests[] = {
      {"résumé", "Latin-1 text", 6},
      {"café", "Latin-1 with accent", 4},
      {"ñandú", "Latin-1 with tilde", 5},
      {"ÄÖÜäöüß", "German umlauts", 7},
      {"·", "Greek ano teleia (ambiguous; narrow here)", 1},
  };
  failures += run_tests(
      latin_tests, sizeof(latin_tests) / sizeof(latin_tests[0]), "Latin-1");

  /* Wide and combined characters. */
  test_case_t wide_tests[] = {
      {"你好", "Chinese (wide)", 4},
      {"こんにちは", "Japanese (wide)", 10},
      {"안녕하세요", "Korean (wide)", 10},
      {"ｈｅｌｌｏ", "Fullwidth Latin", 10},
      {"→←↑↓", "Arrows (narrow)", 4},
      {"①②③④", "Circled numbers", 4},
  };
  failures += run_tests(wide_tests, sizeof(wide_tests) / sizeof(wide_tests[0]),
                        "Wide characters");

  /* Emoji. */
  test_case_t emoji_tests[] = {
      {"😀", "Basic emoji", 2},
      {"👨‍👩‍👧", "Family emoji (ZWJ sequence)", 2},
      {"🇺🇸", "Flag emoji", 2},
      {"👍🏼", "Emoji with skin tone", 2},
      {"☕️", "Coffee with emoji presentation", 2},
      {"⚠️", "Warning with emoji presentation", 2},
  };
  failures += run_tests(emoji_tests,
                        sizeof(emoji_tests) / sizeof(emoji_tests[0]), "Emoji");

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
      {"A\u200DB", "ASCII A ZWJ B", 2},
  };
  failures +=
      run_tests(special_tests, sizeof(special_tests) / sizeof(special_tests[0]),
                "Special cases");

  /* Additional suites */
  test_control_char_width();
  test_individual_codepoints();
  test_codepoint_sequences();
  test_mixed_sequences();

  test_variation_sequences();
  test_zwj_sequences();
  test_fullwidth_and_cjk();
  test_regional_indicator_runs();
  test_combining_sequences();
  test_hangul_decomposed();

  if (failures == 0) {
    printf("All test categories passed!\n");
    return 0;
  } else {
    printf("Total failures: %d\n", failures);
    return 1;
  }
}
