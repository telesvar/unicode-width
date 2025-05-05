#!/usr/bin/env python3
#
# Generate C code for unicode-width calculations for terminal applications.
# Based on the Rust unicode-width crate but converted for left-to-right
# processing.
#
# Copyright 2011-2025 The Rust Project Developers.
# Copyright 2025 Dair Aidarkhanov.
#
# This generation script is licensed under the Apache License, Version 2.0
# <LICENSE-APACHE or http://www.apache.org/licenses/LICENSE-2.0> or the MIT
# license <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your option.
# This file may not be copied, modified, or distributed except according to
# those terms.
#
# The generated C code output from the generation script is licensed under the
# 0BSD license <LICENSE-0BSD or https://opensource.org/license/0bsd>.
#
# This script uses Unicode® data files which are downloaded during execution.
# Use of these Unicode data files is subject to the Unicode License Agreement.
# By using this script, you agree to the terms of that license, which can be
# found at <https://www.unicode.org/license.txt>.

import enum
import math
import operator
import os
import re
import sys
import urllib.request
from collections import defaultdict
from typing import Callable, Iterable, List, Set, Dict, Tuple, Optional

UNICODE_VERSION = "16.0.0"
"""The version of the Unicode data files to download."""

NUM_CODEPOINTS = 0x110000
"""An upper bound for which `range(0, NUM_CODEPOINTS)` contains Unicode's codespace."""

MAX_CODEPOINT_BITS = math.ceil(math.log2(NUM_CODEPOINTS - 1))
"""The maximum number of bits required to represent a Unicode codepoint."""

TABLE_SPLITS = [7, 13]
"""The splits between the bits of the codepoint used to index each subtable."""

OUTPUT_HEADER = "unicode_width.h"
"""Output header file path."""

OUTPUT_SOURCE = "unicode_width.c"
"""Output source file path."""

class OffsetType(enum.IntEnum):
  """Represents the data type of a lookup table's offsets."""
  U2 = 2  # 2-bit unsigned integers, packed four-per-byte
  U4 = 4  # 4-bit unsigned integers, packed two-per-byte
  U8 = 8  # Each offset is a single byte (u8)

Codepoint = int
BitPos = int

def fetch_open(filename: str, local_prefix: str = "", emoji: bool = False):
  """Opens `filename` and return its corresponding file object.
  If `filename` isn't on disk, fetches it from `https://www.unicode.org/Public/`.

  Args:
    filename: The name of the file to open
    local_prefix: Optional directory prefix for the local file
    emoji: Whether the file is in the emoji directory

  Returns:
    An open file object for the requested file

  Raises:
    SystemExit: If the file cannot be downloaded or opened
  """
  basename = os.path.basename(filename)
  localname = os.path.join(local_prefix, basename)
  if not os.path.exists(localname):
    try:
      if emoji:
        prefix = f"emoji/{UNICODE_VERSION[:-2]}"
      else:
        prefix = f"{UNICODE_VERSION}/ucd"

      if not hasattr(fetch_open, '_unicode_notice_shown'):
        print("\nThis script will download Unicode data files from unicode.org.")
        print("These files are subject to the Unicode License Agreement.")
        print("By continuing, you agree to the terms of that license.")
        print("See <https://www.unicode.org/license.txt>.\n")
        print("Downloading Unicode data files...")
        fetch_open._unicode_notice_shown = True

      urllib.request.urlretrieve(
        f"https://www.unicode.org/Public/{prefix}/{filename}",
        localname,
      )
    except Exception as e:
      sys.stderr.write(f"Error downloading {filename}: {e}\n")
      sys.exit(1)

  try:
    return open(localname, encoding="utf-8")
  except OSError as e:
    sys.stderr.write(f"Cannot load {localname}: {e}\n")
    sys.exit(1)

def load_unicode_version() -> tuple[int, int, int]:
  """Returns the current Unicode version as a tuple of (major, minor, patch).

  Returns:
    A tuple containing the major, minor, and patch version numbers
  """
  with fetch_open("ReadMe.txt") as readme:
    pattern = r"for Version (\d+)\.(\d+)\.(\d+) of the Unicode"
    match = re.search(pattern, readme.read())
    if not match:
      sys.stderr.write("Could not determine Unicode version\n")
      sys.exit(1)
    return tuple(map(int, match.groups()))

def load_property(filename: str, pattern: str, action: Callable[[int], None]):
  """Load Unicode properties from a data file and call action for each matching codepoint.

  Args:
    filename: The Unicode data file to process
    pattern: Regular expression pattern to match property values
    action: Function to call for each matching codepoint
  """
  with fetch_open(filename) as properties:
    single = re.compile(rf"^([0-9A-F]+)\s*;\s*{pattern}\s+")
    multiple = re.compile(rf"^([0-9A-F]+)\.\.([0-9A-F]+)\s*;\s*{pattern}\s+")

    for line in properties.readlines():
      raw_data = None  # (low, high)
      if match := single.match(line):
        raw_data = (match.group(1), match.group(1))
      elif match := multiple.match(line):
        raw_data = (match.group(1), match.group(2))
      else:
        continue
      low = int(raw_data[0], 16)
      high = int(raw_data[1], 16)
      for cp in range(low, high + 1):
        action(cp)

def to_sorted_ranges(iter: Iterable[Codepoint]) -> list[tuple[Codepoint, Codepoint]]:
  """Creates a sorted list of ranges from an iterable of codepoints.

  Args:
    iter: An iterable of codepoints

  Returns:
    A list of (start, end) range tuples sorted by codepoint
  """
  lst = sorted(iter)
  ret = []
  for cp in lst:
    if len(ret) > 0 and ret[-1][1] == cp - 1:
      ret[-1] = (ret[-1][0], cp)
    else:
      ret.append((cp, cp))
  return ret

class EastAsianWidth(enum.IntEnum):
  """Represents the width of a Unicode character according to UAX 16."""
  NARROW    = 1  # One column wide
  WIDE      = 2  # Two columns wide
  AMBIGUOUS = 3  # Two columns wide in CJK context, one otherwise (we'll use one for terminals)

class CharWidthInTable(enum.IntEnum):
  """Represents the width of a Unicode character as stored in the tables."""
  ZERO    = 0
  ONE     = 1
  TWO     = 2
  SPECIAL = 3

class WidthState(enum.IntEnum):
  """Width calculation state machine states."""

  # Basic widths
  ZERO   = 0x1_0000  # Zero columns wide
  NARROW = 0x1_0001  # One column wide
  WIDE   = 0x1_0002  # Two columns wide
  THREE  = 0x1_0003  # Three columns wide

  # Line feed handling
  LINE_FEED = 0b0000_0000_0000_0001  # \n (CRLF has width 1)

  # Emoji states
  EMOJI_MODIFIER                      = 0b0000_0000_0000_0010  # Emoji_Modifier
  REGIONAL_INDICATOR                  = 0b0000_0000_0000_0011  # Regional_Indicator
  SEVERAL_REGIONAL_INDICATOR          = 0b0000_0000_0000_0100  # Multiple Regional_Indicators
  EMOJI_PRESENTATION                  = 0b0000_0000_0000_0101  # Emoji_Presentation
  ZWJ_EMOJI_PRESENTATION              = 0b0001_0000_0000_0110  # \u200D Emoji_Presentation
  VS16_ZWJ_EMOJI_PRESENTATION         = 0b1001_0000_0000_0110  # \uFEOF \u200D Emoji_Presentation
  KEYCAP_ZWJ_EMOJI_PRESENTATION       = 0b0001_0000_0000_0111  # \u20E3 \u200D Emoji_Presentation
  VS16_KEYCAP_ZWJ_EMOJI_PRESENTATION  = 0b1001_0000_0000_0111  # \uFE0F \u20E3 \u200D Emoji_Presentation
  REGIONAL_INDICATOR_ZWJ_PRESENTATION = 0b0000_0000_0000_1001  # Regional_Indicator \u200D Emoji_Presentation
  TAG_END_ZWJ_EMOJI_PRESENTATION      = 0b0000_0000_0001_0000  # \uE007F \u200D Emoji_Presentation

  # Variation selector
  VARIATION_SELECTOR_15 = 0b0100_0000_0000_0000  # \uFE0E Text_Presentation
  VARIATION_SELECTOR_16 = 0b1000_0000_0000_0000  # \uFE0F Emoji_Presentation

  def table_width(self) -> CharWidthInTable:
    """The width of a character as stored in the lookup tables."""
    match self:
      case WidthState.ZERO:
        return CharWidthInTable.ZERO
      case WidthState.NARROW:
        return CharWidthInTable.ONE
      case WidthState.WIDE:
        return CharWidthInTable.TWO
      case _:
        return CharWidthInTable.SPECIAL

  def is_carried(self) -> bool:
    """Whether this corresponds to a non-default width info."""
    return int(self) <= 0xFFFF

  def width_alone(self) -> int:
    """The width of a character with this type when it appears alone."""
    match self:
      case WidthState.ZERO | WidthState.VARIATION_SELECTOR_15 | WidthState.VARIATION_SELECTOR_16:
        return 0
      case WidthState.WIDE | WidthState.EMOJI_MODIFIER | WidthState.EMOJI_PRESENTATION:
        return 2
      case WidthState.THREE:
        return 3
      case _:
        return 1

def load_east_asian_widths() -> list[EastAsianWidth]:
  """Return a list of effective widths, indexed by codepoint.

  Returns:
    A list where each index is a codepoint and the value is its East Asian width
  """
  with fetch_open("EastAsianWidth.txt") as eaw:
    # Matches a width assignment for a single codepoint
    single = re.compile(r"^([0-9A-F]+)\s*;\s*(\w+) +# (\w+)")
    # Matches a width assignment for a range of codepoints
    multiple = re.compile(r"^([0-9A-F]+)\.\.([0-9A-F]+)\s*;\s*(\w+) +# (\w+)")
    # Map between width category code and condensed width
    width_codes = {
      **{c: EastAsianWidth.NARROW for c in ["N", "Na", "H"]},
      **{c: EastAsianWidth.WIDE for c in ["W", "F"]},
      "A": EastAsianWidth.AMBIGUOUS,
    }

    width_map = []
    current = 0
    for line in eaw.readlines():
      raw_data = None  # (low, high, width)
      if match := single.match(line):
        raw_data = (match.group(1), match.group(1), match.group(2))
      elif match := multiple.match(line):
        raw_data = (match.group(1), match.group(2), match.group(3))
      else:
        continue
      low = int(raw_data[0], 16)
      high = int(raw_data[1], 16)
      width = width_codes[raw_data[2]]

      assert current <= high
      while current <= high:
        # Some codepoints don't fall into any of the ranges in EastAsianWidth.txt
        # All such codepoints are implicitly given Neural width (resolves to narrow)
        width_map.append(EastAsianWidth.NARROW if current < low else width)
        current += 1

    while len(width_map) < NUM_CODEPOINTS:
      # Catch any leftover codepoints and assign them implicit Neutral/Narrow width
      width_map.append(EastAsianWidth.NARROW)

  # Characters with ambiguous line breaking are ambiguous
  load_property(
    "LineBreak.txt",
    "AI",
    lambda cp: (operator.setitem(width_map, cp, EastAsianWidth.AMBIGUOUS)),
  )

  # Ambiguous `Letter`s and `Modifier_Symbol`s are narrow
  load_property(
    "extracted/DerivedGeneralCategory.txt",
    r"(:?Lu|Ll|Lt|Lm|Lo|Sk)",
    lambda cp: (
      operator.setitem(width_map, cp, EastAsianWidth.NARROW)
      if width_map[cp] == EastAsianWidth.AMBIGUOUS
      else None
    ),
  )

  # Special case for GREEK ANO TELEIA
  width_map[0x0387] = EastAsianWidth.AMBIGUOUS

  # Canonical equivalence for symbols with stroke
  with fetch_open("UnicodeData.txt") as udata:
    single = re.compile(r"([0-9A-Z]+);.*?;.*?;.*?;.*?;([0-9A-Z]+) 0338;")
    for line in udata.readlines():
      if match := single.match(line):
        composed = int(match.group(1), 16)
        decomposed = int(match.group(2), 16)
        if width_map[decomposed] == EastAsianWidth.AMBIGUOUS:
          width_map[composed] = EastAsianWidth.AMBIGUOUS

  return width_map

def load_zero_widths() -> list[bool]:
  """Returns a list of booleans indicating which codepoints have zero width.

  Returns:
    A list where each index is a codepoint and the value is True if zero-width
  """
  zw_map = [False] * NUM_CODEPOINTS

  # Default_Ignorable_Code_Point and Grapheme_Extend are zero-width
  load_property(
    "DerivedCoreProperties.txt",
    r"(?:Default_Ignorable_Code_Point|Grapheme_Extend)",
    lambda cp: operator.setitem(zw_map, cp, True),
  )

  # Vowel_Jamo and Trailing_Jamo are zero-width
  load_property(
    "HangulSyllableType.txt",
    r"(?:V|T)",
    lambda cp: operator.setitem(zw_map, cp, True),
  )

  # Various special marks are zero-width
  for cp in [0x070F, 0x0605, 0x0890, 0x0891, 0x08E2]:
    zw_map[cp] = True

  # Prepended marks but not Prepended_Concatenation_Mark
  gcb_prepend = set()
  load_property(
    "auxiliary/GraphemeBreakProperty.txt",
    "Prepend",
    lambda cp: gcb_prepend.add(cp),
  )
  load_property(
    "PropList.txt",
    "Prepended_Concatenation_Mark",
    lambda cp: gcb_prepend.discard(cp),
  )
  for cp in gcb_prepend:
    zw_map[cp] = True

  # Special exceptions
  zw_map[0x115F] = False  # HANGUL CHOSEONG FILLER - not zero width
  zw_map[0x2D7F] = False  # TIFINAGH CONSONANT JOINER - not zero width
  zw_map[0xA8FA] = True   # DEVANAGARI CARET - zero width
  zw_map[0x200D] = True   # ZERO WIDTH JOINER - zero width

  return zw_map

def load_width_maps() -> list[WidthState]:
  """Load width table for terminal applications.

  Returns:
    A list of WidthState values indexed by codepoint
  """
  eaws = load_east_asian_widths()
  zws = load_zero_widths()

  width_map = []

  for eaw, zw in zip(eaws, zws):
    if zw:
      width_map.append(WidthState.ZERO)
    else:
      if eaw == EastAsianWidth.WIDE:
        width_map.append(WidthState.WIDE)
      else:
        width_map.append(WidthState.NARROW)

  # Load emoji data
  regional_indicators = []
  load_property(
    "PropList.txt",
    "Regional_Indicator",
    lambda cp: regional_indicators.append(cp),
  )

  emoji_modifiers = []
  load_property(
    "emoji/emoji-data.txt",
    "Emoji_Modifier",
    lambda cp: emoji_modifiers.append(cp),
  )

  emoji_presentation = []
  load_property(
    "emoji/emoji-data.txt",
    "Emoji_Presentation",
    lambda cp: emoji_presentation.append(cp),
  )

  # Apply special cases for terminal applications
  for cps, width in [
    ([0x0A], WidthState.LINE_FEED),
    ([0x17A4], WidthState.WIDE),
    ([0x17D8], WidthState.THREE),
    ([0xFE0E], WidthState.VARIATION_SELECTOR_15),
    ([0xFE0F], WidthState.VARIATION_SELECTOR_16),
    (emoji_presentation, WidthState.EMOJI_PRESENTATION),
    (emoji_modifiers, WidthState.EMOJI_MODIFIER),
    (regional_indicators, WidthState.REGIONAL_INDICATOR),
  ]:
    for cp in cps:
      width_map[cp] = width

  return width_map

def make_special_ranges(
  width_map: list[WidthState],
) -> list[tuple[tuple[Codepoint, Codepoint], WidthState]]:
  """Assign ranges of characters to their special behavior for use in match statements.

  Args:
    width_map: The list of width states indexed by codepoint

  Returns:
    A list of ((range_start, range_end), width_state) tuples
  """
  ret = []
  can_merge_with_prev = False
  for cp, width in enumerate(width_map):
    if width == WidthState.EMOJI_PRESENTATION:
      can_merge_with_prev = False
    elif width.table_width() == CharWidthInTable.SPECIAL:
      if can_merge_with_prev and ret[-1][1] == width:
        ret[-1] = ((ret[-1][0][0], cp), width)
      else:
        ret.append(((cp, cp), width))
        can_merge_with_prev = True
  return ret

class Bucket:
  """A bucket contains a group of codepoints and an ordered width list."""

  def __init__(self):
    """Creates an empty bucket."""
    self.entry_set = set()
    self.widths = []

  def append(self, codepoint: Codepoint, width: CharWidthInTable):
    """Adds a codepoint/width pair to the bucket, and appends `width` to the width list."""
    self.entry_set.add((codepoint, width))
    self.widths.append(width)

  def try_extend(self, attempt: "Bucket") -> bool:
    """Try to merge this bucket with another.

    Args:
      attempt: Bucket to try to merge with

    Returns:
      True if merge was successful, False otherwise
    """
    (less, more) = (self.widths, attempt.widths)
    if len(self.widths) > len(attempt.widths):
      (less, more) = (attempt.widths, self.widths)
    if less != more[: len(less)]:
      return False
    self.entry_set |= attempt.entry_set
    self.widths = more
    return True

  def entries(self) -> list[tuple[Codepoint, CharWidthInTable]]:
    """Return a list of the codepoint/width pairs in this bucket, sorted by codepoint."""
    result = list(self.entry_set)
    result.sort()
    return result

  def width(self) -> CharWidthInTable | None:
    """If all codepoints in this bucket have the same width, return that width."""
    if len(self.widths) == 0:
      return None
    potential_width = self.widths[0]
    for width in self.widths[1:]:
      if potential_width != width:
        return None
    return potential_width

def make_buckets(
  entries: Iterable[tuple[int, CharWidthInTable]], low_bit: BitPos, cap_bit: BitPos
) -> list[Bucket]:
  """Partitions entries into buckets based on bit patterns.

  Args:
    entries: Iterable of (codepoint, width) tuples
    low_bit: Lower bit position for partitioning
    cap_bit: Upper bit position for partitioning

  Returns:
    List of Bucket objects
  """
  num_bits = cap_bit - low_bit
  assert num_bits > 0
  buckets = [Bucket() for _ in range(0, 2**num_bits)]
  mask = (1 << num_bits) - 1
  for codepoint, width in entries:
    buckets[(codepoint >> low_bit) & mask].append(codepoint, width)
  return buckets

class Table:
  """Represents a lookup table with subtables."""

  def __init__(
    self,
    name: str,
    entry_groups: Iterable[Iterable[tuple[int, CharWidthInTable]]],
    low_bit: BitPos,
    cap_bit: BitPos,
    offset_type: OffsetType,
    align: int,
    bytes_per_row: int | None = None,
    starting_indexed: list[Bucket] = [],
  ):
    """Create a lookup table with subtables.

    Args:
      name: Name of the table
      entry_groups: Groups of entries for each subtable
      low_bit: Lower bit position for indexing
      cap_bit: Upper bit position for indexing
      offset_type: Type of offset (bit width) to use
      align: Alignment value for the table
      bytes_per_row: Optional bytes per row for formatting
      starting_indexed: Optional list of initial buckets
    """
    self.name = name
    self.low_bit = low_bit
    self.cap_bit = cap_bit
    self.offset_type = offset_type
    self.entries: list[int] = []
    self.indexed: list[Bucket] = list(starting_indexed)
    self.align = align
    self.bytes_per_row = bytes_per_row

    buckets: list[Bucket] = []
    for entries in entry_groups:
      buckets.extend(make_buckets(entries, self.low_bit, self.cap_bit))

    for bucket in buckets:
      for i, existing in enumerate(self.indexed):
        if existing.try_extend(bucket):
          self.entries.append(i)
          break
      else:
        self.entries.append(len(self.indexed))
        self.indexed.append(bucket)

    # Validate offset type
    max_index = 1 << int(self.offset_type)
    for index in self.entries:
      assert index < max_index, f"{index} <= {max_index}"

  def indices_to_widths(self):
    """Converts indices to width values."""
    self.entries = list(map(lambda i: int(self.indexed[i].width()), self.entries))
    del self.indexed

  def buckets(self):
    """Returns an iterator over this table's buckets."""
    return self.indexed

  def to_bytes(self) -> list[int]:
    """Returns this table's entries as a list of bytes."""
    entries_per_byte = 8 // int(self.offset_type)
    byte_array = []
    for i in range(0, len(self.entries), entries_per_byte):
      byte = 0
      for j in range(0, entries_per_byte):
        if i + j < len(self.entries):
          byte |= self.entries[i + j] << (j * int(self.offset_type))
      byte_array.append(byte)
    return byte_array

def make_tables(width_map: list[WidthState]) -> list[Table]:
  """Creates the tables for width lookup.

  Args:
    width_map: Width map indexed by codepoint

  Returns:
    List of Table objects for lookup
  """
  entries = enumerate([w.table_width() for w in width_map])

  root_table = Table(
    "WIDTH_ROOT",
    [entries],
    TABLE_SPLITS[1],
    MAX_CODEPOINT_BITS,
    OffsetType.U8,
    128,
  )

  middle_table = Table(
    "WIDTH_MIDDLE",
    map(lambda bucket: bucket.entries(), root_table.buckets()),
    TABLE_SPLITS[0],
    TABLE_SPLITS[1],
    OffsetType.U8,
    2 ** (TABLE_SPLITS[1] - TABLE_SPLITS[0]),
    bytes_per_row=2 ** (TABLE_SPLITS[1] - TABLE_SPLITS[0]),
  )

  leaves_table = Table(
    "WIDTH_LEAVES",
    map(lambda bucket: bucket.entries(), middle_table.buckets()),
    0,
    TABLE_SPLITS[0],
    OffsetType.U2,
    2 ** (TABLE_SPLITS[0] - 2),
    bytes_per_row=2 ** (TABLE_SPLITS[0] - 2),
  )

  leaves_table.indices_to_widths()  # Convert indices to actual widths
  return [root_table, middle_table, leaves_table]

def load_emoji_presentation_sequences() -> list[Codepoint]:
  """Get all codepoints that can start emoji presentation sequences.

  Returns:
    List of codepoints that can start emoji presentation sequences
  """
  with fetch_open("emoji/emoji-variation-sequences.txt") as sequences:
    sequence = re.compile(r"^([0-9A-F]+)\s+FE0F\s*;\s*emoji style")
    codepoints = []
    for line in sequences.readlines():
      if match := sequence.match(line):
        cp = int(match.group(1), 16)
        codepoints.append(cp)
  return codepoints

def load_text_presentation_sequences() -> list[Codepoint]:
  """Get all codepoints whose width changes with a text presentation sequence.

  Returns:
    List of codepoints whose width changes with text presentation sequences
  """
  text_presentation_seq_codepoints = set()
  with fetch_open("emoji/emoji-variation-sequences.txt", emoji=True) as sequences:
    # Match all text presentation sequences
    # (one codepoint followed by U+FE0E, and labeled "text style")
    sequence = re.compile(r"^([0-9A-F]+)\s+FE0E\s*;\s*text style")
    for line in sequences.readlines():
      if match := sequence.match(line):
        cp = int(match.group(1), 16)
        text_presentation_seq_codepoints.add(cp)

  default_emoji_codepoints = set()

  load_property(
    "emoji/emoji-data.txt",
    "Emoji_Presentation",
    lambda cp: default_emoji_codepoints.add(cp),
  )

  codepoints = []
  for cp in text_presentation_seq_codepoints.intersection(default_emoji_codepoints):
    # "Enclosed Ideographic Supplement" block;
    # Wide even in text presentation
    if not cp in range(0x1F200, 0x1F300):
      codepoints.append(cp)

  codepoints.sort()
  return codepoints

def load_emoji_modifier_bases() -> list[Codepoint]:
  """Get all emoji modifier base codepoints.

  Returns:
    List of emoji modifier base codepoints
  """
  ret = []
  load_property(
    "emoji/emoji-data.txt",
    "Emoji_Modifier_Base",
    lambda cp: ret.append(cp),
  )
  ret.sort()
  return ret

def make_presentation_sequence_table(
  seqs: list[Codepoint],
  lsb: int = 10,
) -> tuple[list[tuple[int, int]], list[list[int]]]:
  """Generates 2-level lookup table for emoji presentation sequences.

  Args:
    seqs: List of codepoints that can start emoji presentation sequences
    lsb: Number of least significant bits to use for the second level

  Returns:
    Tuple of (indexes, leaves) for the lookup table
  """
  prefixes_dict = defaultdict(set)
  for cp in seqs:
    prefixes_dict[cp >> lsb].add(cp & (2**lsb - 1))

  msbs: list[int] = list(prefixes_dict.keys())

  leaves: list[list[int]] = []
  for cps in prefixes_dict.values():
    leaf = [0] * (2 ** (lsb - 3))
    for cp in cps:
      idx_in_leaf, bit_shift = divmod(cp, 8)
      leaf[idx_in_leaf] |= 1 << bit_shift
    leaves.append(leaf)

  indexes = [(msb, index) for (index, msb) in enumerate(msbs)]

  # Cull duplicate leaves
  i = 0
  while i < len(leaves):
    first_idx = leaves.index(leaves[i])
    if first_idx == i:
      i += 1
    else:
      for j in range(0, len(indexes)):
        if indexes[j][1] == i:
          indexes[j] = (indexes[j][0], first_idx)
        elif indexes[j][1] > i:
          indexes[j] = (indexes[j][0], indexes[j][1] - 1)

      leaves.pop(i)

  return (indexes, leaves)

def make_ranges_table(
  seqs: list[Codepoint],
) -> tuple[list[tuple[int, int]], list[list[tuple[int, int]]]]:
  """Generates 2-level lookup table for a binary property of a codepoint.

  Args:
    seqs: List of codepoints with the property

  Returns:
    Tuple of (indexes, leaves) for the lookup table
  """
  prefixes_dict = defaultdict(list)
  for cp in seqs:
    prefixes_dict[cp >> 8].append(cp & 0xFF)

  msbs: list[int] = list(prefixes_dict.keys())

  leaves: list[list[tuple[int, int]]] = []
  for cps in prefixes_dict.values():
    leaf = []
    for cp in sorted(cps):
      if len(leaf) > 0 and leaf[-1][1] == cp - 1:
        leaf[-1] = (leaf[-1][0], cp)
      else:
        leaf.append((cp, cp))
    leaves.append(leaf)

  indexes = [(msb, index) for (index, msb) in enumerate(msbs)]

  # Cull duplicate leaves
  i = 0
  while i < len(leaves):
    first_idx = leaves.index(leaves[i])
    if first_idx == i:
      i += 1
    else:
      for j in range(0, len(indexes)):
        if indexes[j][1] == i:
          indexes[j] = (indexes[j][0], first_idx)
        elif indexes[j][1] > i:
          indexes[j] = (indexes[j][0], indexes[j][1] - 1)

      leaves.pop(i)

  return (indexes, leaves)

def emit_header_file(filename: str, version: tuple[int, int, int]):
  """Generate the C header file.

  Args:
    filename: Name of the header file to generate
    version: Unicode version as (major, minor, patch)
  """
  with open(filename, "w", newline="\n") as f:
    f.write(f"""/* Generated by unicode-width generator.
 *
 * Unicode {version[0]}.{version[1]}.{version[2]} data.
 * For terminal width calculation.
 *
 * Copyright 2025 Dair Aidarkhanov
 * SPDX-License-Identifier: 0BSD
 */

#ifndef UNICODE_WIDTH_H
#define UNICODE_WIDTH_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C" {{
#endif

/* Version of Unicode data.
 */
#define UNICODE_WIDTH_VERSION_MAJOR {version[0]}
#define UNICODE_WIDTH_VERSION_MINOR {version[1]}
#define UNICODE_WIDTH_VERSION_PATCH {version[2]}

/* Width state values.
 */
typedef enum {{
  WIDTH_STATE_DEFAULT                             = 0,
  WIDTH_STATE_LINE_FEED                           = 1,
  WIDTH_STATE_EMOJI_MODIFIER                      = 2,
  WIDTH_STATE_REGIONAL_INDICATOR                  = 3,
  WIDTH_STATE_SEVERAL_REGIONAL_INDICATOR          = 4,
  WIDTH_STATE_EMOJI_PRESENTATION                  = 5,
  WIDTH_STATE_ZWJ_EMOJI_PRESENTATION              = 6,
  WIDTH_STATE_KEYCAP_ZWJ_EMOJI_PRESENTATION       = 7,
  WIDTH_STATE_REGIONAL_INDICATOR_ZWJ_PRESENTATION = 8,
  WIDTH_STATE_TAG_END_ZWJ_EMOJI_PRESENTATION      = 9,
  WIDTH_STATE_ZWJ_SEQUENCE_MEMBER                 = 10,
}} width_state_t;

/* State for the width calculation state machine.
 */
typedef struct {{
  width_state_t  state;
  uint_least32_t previous_codepoint;
}} unicode_width_state_t;

/* Initialize a unicode width state.
 * Must be called before any other function.
 *
 * @param state Pointer to state to initialize
 */
void unicode_width_init(unicode_width_state_t *state);

/* Process a Unicode codepoint and return its width.
 * Width is 0, 1, 2, or 3, or -1 for control characters.
 *
 * Control characters (0x00-0x1F except newlines, 0x7F, and 0x80-0x9F) return -1,
 * allowing the caller to decide how to display them. For readline-like applications,
 * control characters are typically displayed using caret notation (^X) with width 2.
 * Use unicode_width_control_char() to get this width.
 *
 * Newlines (LF, CR, CRLF) return width 0 as they don't consume horizontal space
 * in terminal displays.
 *
 * Note that the width of a codepoint may depend on context (preceding/following
 * codepoints), so this function keeps track of context in the provided state.
 *
 * @param state Pointer to state
 * @param codepoint Unicode codepoint to process
 * @return Width of the codepoint in columns, or -1 for control characters
 */
int unicode_width_process(unicode_width_state_t *state, uint_least32_t codepoint);

/* Get the display width of a control character in caret notation (e.g., ^A).
 * This is useful for applications like readline that display control chars.
 *
 * @param codepoint The Unicode codepoint to check
 * @return The display width (usually 2 for ^X notation), or -1 if not a control char
 */
int unicode_width_control_char(uint_least32_t codepoint);

/* Reset a unicode width state to its initial state.
 *
 * @param state Pointer to state to reset
 */
void unicode_width_reset(unicode_width_state_t *state);

#ifdef __cplusplus
}} /* extern "C" */
#endif

#endif /* UNICODE_WIDTH_H */
""")

def emit_source_file(
  filename: str,
  version: tuple[int, int, int],
  tables: list[Table],
  special_ranges: list[tuple[tuple[Codepoint, Codepoint], WidthState]],
  emoji_presentation_table: tuple[list[tuple[int, int]], list[list[int]]],
  emoji_modifier_table: tuple[list[tuple[int, int]], list[list[tuple[int, int]]]],
):
  """Generate the C source file.

  Args:
    filename: Name of the source file to generate
    version: Unicode version as (major, minor, patch)
    tables: List of tables for width lookup
    special_ranges: Special character ranges for width lookup
    emoji_presentation_table: Emoji presentation lookup table
    emoji_modifier_table: Emoji modifier base lookup table
  """
  with open(filename, "w", newline="\n") as f:
    f.write(f"""/* Generated by unicode-width generator.
 *
 * Unicode {version[0]}.{version[1]}.{version[2]} data.
 * For terminal width calculation.
 *
 * Copyright 2025 Dair Aidarkhanov
 * SPDX-License-Identifier: 0BSD
 */

#include "unicode_width.h"
#include <assert.h>

/* Tables for width lookup.
 */
""")

    # Write tables
    for table in tables:
      byte_array = table.to_bytes()
      if table.bytes_per_row is None:
        f.write(f"static const uint_least8_t {table.name}[{len(byte_array)}] = {{\n")
        for j, byte in enumerate(byte_array):
          if j % 16 == 0:
            f.write("  ")
          f.write(f" 0x{byte:02X},")
          if j % 16 == 15:
            f.write("\n")
        if len(byte_array) % 16 != 0:
          f.write("\n")
        f.write("};\n\n")
      else:
        num_rows = len(byte_array) // table.bytes_per_row
        f.write(f"static const uint_least8_t {table.name}[{num_rows}][{table.bytes_per_row}] = {{\n")
        for row_num in range(0, num_rows):
          f.write("  {")
          row = byte_array[row_num * table.bytes_per_row: (row_num + 1) * table.bytes_per_row]
          for j, entry in enumerate(row):
            if j % 16 == 0 and j > 0:
              f.write("\n   ")
            f.write(f" 0x{entry:02X},")
          f.write("\n  },\n")
        f.write("};\n\n")

    # Write emoji presentation and modifier tables
    emoji_presentation_idx, emoji_presentation_leaves = emoji_presentation_table
    emoji_modifier_idx, emoji_modifier_leaves = emoji_modifier_table

    f.write("/* Emoji presentation sequence lookup.\n */\n")
    f.write("static int is_emoji_presentation_start(uint_least32_t cp) {\n")
    f.write("  /* First level lookup by top bits */\n")
    f.write("  int leaf_idx;\n")
    f.write("  uint_least16_t bottom;\n\n")
    f.write("  switch (cp >> 10) {\n")
    for msb, idx in emoji_presentation_idx:
      f.write(f"    case 0x{msb:X}: leaf_idx = {idx}; break;\n")
    f.write("    default: return 0;\n")
    f.write("  }\n\n")
    f.write("  /* Second level lookup by bottom 10 bits. */\n")
    f.write("  bottom = cp & 0x3FF;\n")
    f.write("  uint_least8_t byte_idx = bottom >> 3;\n")
    f.write("  uint_least8_t bit_idx = bottom & 0x7;\n\n")

    f.write("  /* Bitmap lookup - check if the bit is set. */\n")
    f.write("  static const uint_least8_t bitmap[][128] = {\n")
    for leaf in emoji_presentation_leaves:
      f.write("    {")
      for byte_idx, byte in enumerate(leaf):
        if byte_idx % 16 == 0 and byte_idx > 0:
          f.write("\n     ")
        f.write(f" 0x{byte:02X},")
      f.write("\n    },\n")
    f.write("  };\n\n")

    f.write("  return (bitmap[leaf_idx][byte_idx] >> bit_idx) & 1;\n")
    f.write("}\n\n")

    f.write("/* Emoji modifier base lookup.\n */\n")
    f.write("static int is_emoji_modifier_base(uint_least32_t cp) {\n")
    f.write("  /* First level lookup by top bits. */\n")
    f.write("  int leaf_idx;\n")
    f.write("  uint_least8_t bottom;\n\n")
    f.write("  switch (cp >> 8) {\n")
    for msb, idx in emoji_modifier_idx:
      f.write(f"    case 0x{msb:X}: leaf_idx = {idx}; break;\n")
    f.write("    default: return 0;\n")
    f.write("  }\n\n")
    f.write("  /* Second level lookup by bottom 8 bits. */\n")
    f.write("  bottom = cp & 0xFF;\n")

    for idx, leaf in enumerate(emoji_modifier_leaves):
      f.write(f"  static const uint_least8_t LEAF_{idx}_LO[] = {{")
      for lo, hi in leaf:
        f.write(f" 0x{lo:02X},")
      f.write(" };\n")
      f.write(f"  static const uint_least8_t LEAF_{idx}_HI[] = {{")
      for lo, hi in leaf:
        f.write(f" 0x{hi:02X},")
      f.write(" };\n")

    f.write("\n")
    f.write("  /* Binary search in the leaf. */\n")
    f.write("  switch (leaf_idx) {\n")
    for idx, leaf in enumerate(emoji_modifier_leaves):
      f.write(f"    case {idx}: {{\n")
      f.write(f"      int lo = 0, hi = {len(leaf) - 1}, mid;\n")
      f.write("      while (lo <= hi) {\n")
      f.write("        mid = (lo + hi) / 2;\n")
      f.write(f"        if (bottom < LEAF_{idx}_LO[mid]) hi = mid - 1;\n")
      f.write(f"        else if (bottom > LEAF_{idx}_HI[mid]) lo = mid + 1;\n")
      f.write("        else return 1;\n")
      f.write("      }\n")
      f.write("      return 0;\n")
      f.write("    }\n")
    f.write("  }\n")
    f.write("  return 0;\n")
    f.write("}\n")

    # Width lookup function
    f.write("""
/* Get the basic width of a codepoint using table lookup.
 */
static int lookup_width(uint_least32_t cp) {
  uint_least8_t t1_offset;
  uint_least8_t t2_offset;
  uint_least8_t packed_widths;
  uint_least8_t width;

  /* Get offset into middle table from root table. */
  t1_offset = WIDTH_ROOT[cp >> %d];

  /* Get offset into leaf table from middle table. */
  t2_offset = WIDTH_MIDDLE[t1_offset][cp >> %d & 0x%X];

  /* Get packed widths from leaf table. */
  packed_widths = WIDTH_LEAVES[t2_offset][cp >> 2 & 0x%X];

  /* Extract width from packed widths. */
  width = (packed_widths >> (2 * (cp & 0b11))) & 0b11;

  /* Handle special cases. */
  if (width < 3) {
    return width;
  } else {
    switch (cp) {
""" % (TABLE_SPLITS[1], TABLE_SPLITS[0], (2 ** (TABLE_SPLITS[1] - TABLE_SPLITS[0]) - 1), (2 ** (TABLE_SPLITS[0] - 2) - 1)))

    for (lo, hi), width in special_ranges:
      f.write(f"      case 0x{lo:X}:")
      if hi != lo:
        for cp in range(lo + 1, hi + 1):
          f.write(f"\n      case 0x{cp:X}:")
      f.write(f" return {width.width_alone()};\n")

    f.write("\n      /* EMOJI_PRESENTATION. */\n")
    f.write("      default: return 2;\n")
    f.write("    }\n")
    f.write("  }\n")
    f.write("}\n")

    # API implementation
    f.write("""
/* Initialize a width calculation state machine.
 */
void unicode_width_init(unicode_width_state_t *state) {
  assert(state != NULL);
  state->state = WIDTH_STATE_DEFAULT;
  state->previous_codepoint = 0;
}

/* Process a Unicode codepoint and return its width.
 * This function keeps track of context via the state parameter.
 */
int unicode_width_process(unicode_width_state_t *state, uint_least32_t codepoint) {
  int width;
  width_state_t next_state = WIDTH_STATE_DEFAULT;
  int adjust = 0;

  assert(state != NULL);

  /* Handle control characters. */
  if ((codepoint < 0x20 && codepoint != 0x0A && codepoint != 0x0D) ||
      codepoint == 0x7F ||
      (codepoint >= 0x80 && codepoint <= 0x9F)) {
    /* Control characters return -1, letting the caller decide how to display them. */
    state->previous_codepoint = codepoint;
    return -1;
  }

  /* ASCII fast path. */
  if (codepoint < 0x7F) {
    /* Handle newline specially. */
    if (codepoint == 0x0A) {
      /* LF: In terminals, newlines don't consume horizontal space. */
      if (state->previous_codepoint == 0x0D) {
        /* Part of CRLF sequence. */
        next_state = WIDTH_STATE_DEFAULT;
      } else {
        /* Standalone LF. */
        next_state = WIDTH_STATE_LINE_FEED;
      }
      width = 0;
    } else if (codepoint == 0x0D) {
      /* CR: Always zero width. */
      width = 0;
    } else {
      /* Regular ASCII. */
      width = 1;
    }
  } else {
    /* Look up basic width. */
    width = lookup_width(codepoint);
  }

  /* Handle state transitions based on current state and codepoint. */
  switch (state->state) {
    case WIDTH_STATE_LINE_FEED:
      if (codepoint == 0x0D) {
        /* CR after LF: zero width (part of CRLF). */
        next_state = WIDTH_STATE_DEFAULT;
        width = 0;
      }
      break;

    case WIDTH_STATE_EMOJI_MODIFIER:
      if (is_emoji_modifier_base(codepoint)) {
        /* Emoji modifier followed by base: zero width (part of sequence). */
        next_state = WIDTH_STATE_EMOJI_PRESENTATION;
        width = 0;
      }
      break;

    case WIDTH_STATE_EMOJI_PRESENTATION:
      if (codepoint == 0xFE0F) {
        /* Variation selector-16 after standalone emoji: zero width. */
        next_state = WIDTH_STATE_DEFAULT;
        width = 0;

        /* If the base character has width 1, increase to 2. */
        if (lookup_width(state->previous_codepoint) == 1) {
          adjust = 1;
        }
      } else if (codepoint == 0x200D) {
        /* ZWJ after emoji: zero width. */
        next_state = WIDTH_STATE_ZWJ_EMOJI_PRESENTATION;
        width = 0;
      } else if (codepoint >= 0x1F3FB && codepoint <= 0x1F3FF) {
        /* Skin tone modifier after emoji: zero width. */
        next_state = WIDTH_STATE_EMOJI_MODIFIER;
        width = 0;
      }
      break;

    case WIDTH_STATE_ZWJ_EMOJI_PRESENTATION:
      /* After ZWJ, direct to appropriate state based on character type.
       * Always zero width for anything after ZWJ.
       */
      width = 0;

      if (codepoint == 0x20E3) {
        /* Keycap after ZWJ. */
        next_state = WIDTH_STATE_KEYCAP_ZWJ_EMOJI_PRESENTATION;
      } else if (codepoint >= 0x1F1E6 && codepoint <= 0x1F1FF) {
        /* Regional indicator after ZWJ. */
        next_state = WIDTH_STATE_REGIONAL_INDICATOR_ZWJ_PRESENTATION;
      } else if (codepoint == 0xE007F) {
        /* Tag terminator after ZWJ. */
        next_state = WIDTH_STATE_TAG_END_ZWJ_EMOJI_PRESENTATION;
      } else {
        /* Any other character after ZWJ (including medical symbols, etc.) */
        next_state = WIDTH_STATE_ZWJ_SEQUENCE_MEMBER;
      }
      break;

    case WIDTH_STATE_ZWJ_SEQUENCE_MEMBER:
      /* Handle characters that are part of a ZWJ sequence.
       * Always zero width within a ZWJ sequence.
       */
      width = 0;

      if (codepoint == 0xFE0F) {
        /* VS16 in a ZWJ sequence: zero width with no adjustment. */
        next_state = WIDTH_STATE_DEFAULT;
      } else if (codepoint == 0x200D) {
        /* Another ZWJ - continue the ZWJ sequence. */
        next_state = WIDTH_STATE_ZWJ_EMOJI_PRESENTATION;
      } else {
        /* Any other character - remain in ZWJ sequence. */
        next_state = WIDTH_STATE_ZWJ_SEQUENCE_MEMBER;
      }
      break;

    case WIDTH_STATE_KEYCAP_ZWJ_EMOJI_PRESENTATION:
      /* Always zero width in ZWJ sequence. */
      width = 0;
      if (codepoint == 0xFE0F) {
        /* VS16 after keycap in ZWJ sequence: no adjustment. */
        next_state = WIDTH_STATE_DEFAULT;
      } else if (codepoint == 0x200D) {
        /* ZWJ after keycap: continue sequence. */
        next_state = WIDTH_STATE_ZWJ_EMOJI_PRESENTATION;
      } else {
        /* Any other character. */
        next_state = WIDTH_STATE_ZWJ_SEQUENCE_MEMBER;
      }
      break;

    case WIDTH_STATE_REGIONAL_INDICATOR:
      if (codepoint >= 0x1F1E6 && codepoint <= 0x1F1FF) {
        /* Second regional indicator: part of flag, set width to 2 total. */
        next_state = WIDTH_STATE_SEVERAL_REGIONAL_INDICATOR;
        width = 1;
      }
      break;

    case WIDTH_STATE_SEVERAL_REGIONAL_INDICATOR:
      if (codepoint >= 0x1F1E6 && codepoint <= 0x1F1FF) {
        /* Another regional indicator: reset counter for even/odd pairs. */
        next_state = WIDTH_STATE_REGIONAL_INDICATOR;
        width = 0;
      }
      break;

    case WIDTH_STATE_REGIONAL_INDICATOR_ZWJ_PRESENTATION:
      /* Always zero width in ZWJ context. */
      width = 0;
      if (codepoint >= 0x1F1E6 && codepoint <= 0x1F1FF) {
        /* Regional indicator after another in ZWJ context. */
        next_state = WIDTH_STATE_REGIONAL_INDICATOR;
        width = 1; /* Special case: regional indicators still need width for flags. */
      } else if (codepoint == 0x200D) {
        /* ZWJ after regional indicator in ZWJ context. */
        next_state = WIDTH_STATE_ZWJ_EMOJI_PRESENTATION;
      } else {
        /* Any other character. */
        next_state = WIDTH_STATE_ZWJ_SEQUENCE_MEMBER;
      }
      break;

    case WIDTH_STATE_TAG_END_ZWJ_EMOJI_PRESENTATION:
      /* Always zero width in tag sequence. */
      width = 0;
      if ((codepoint >= 0xE0030 && codepoint <= 0xE0039) ||
          (codepoint >= 0xE0061 && codepoint <= 0xE007A)) {
        /* Tag spec characters after tag end: stay in tag state. */
        next_state = WIDTH_STATE_TAG_END_ZWJ_EMOJI_PRESENTATION;
      } else if (codepoint == 0x1F3F4) {
        /* Black flag: part of tag sequence. */
        next_state = WIDTH_STATE_ZWJ_SEQUENCE_MEMBER;
      } else if (codepoint == 0x200D) {
        /* ZWJ after tag: continue ZWJ sequence. */
        next_state = WIDTH_STATE_ZWJ_EMOJI_PRESENTATION;
      } else {
        /* Any other character. */
        next_state = WIDTH_STATE_ZWJ_SEQUENCE_MEMBER;
      }
      break;

    case WIDTH_STATE_DEFAULT:
      /* Handle carriage return. */
      if (codepoint == 0x0D) {
        /* CR: zero width. */
        width = 0;
      }
      /* Handle emoji presentation selector. */
      else if (codepoint == 0xFE0F) {
        /* Variation selector-16: zero width, but make previous character width 2 if applicable. */
        width = 0;
        if (state->previous_codepoint > 0 && is_emoji_presentation_start(state->previous_codepoint)) {
          /* Add 1 to make previous character width 2. */
          adjust = 1;
        }
      }
      /* Handle text presentation selector. */
      else if (codepoint == 0xFE0E) {
        /* Variation selector-15: zero width, but make previous character width 1 if applicable. */
        width = 0;
        if (state->previous_codepoint > 0 && is_emoji_presentation_start(state->previous_codepoint)) {
          /* Subtract 1 to make previous character width 1. */
          adjust = -1;
        }
      }
      /* Handle emoji modifiers. */
      else if (codepoint >= 0x1F3FB && codepoint <= 0x1F3FF) {
        next_state = WIDTH_STATE_EMOJI_MODIFIER;
      }
      /* Handle regional indicators (flags). */
      else if (codepoint >= 0x1F1E6 && codepoint <= 0x1F1FF) {
        next_state = WIDTH_STATE_REGIONAL_INDICATOR;
        /* First regional indicator has width 1, second will add 1. */
        width = 1;
      }
      /* Handle emoji with default presentation. */
      else if (codepoint >= 0x1F000 || is_emoji_presentation_start(codepoint)) {
        next_state = WIDTH_STATE_EMOJI_PRESENTATION;
      }
      break;
  }

  /* Apply width adjustments from previous operations. */
  width += adjust;

  /* Update state. */
  state->state = next_state;
  state->previous_codepoint = codepoint;

  return width;
}

/* Get the display width of a control character in caret notation (e.g., ^A).
 * This is useful for applications like readline that display control chars.
 */
int unicode_width_control_char(uint_least32_t codepoint) {
  /* C0 control characters (except CR and LF). */
  if (codepoint < 0x20 && codepoint != 0x0A && codepoint != 0x0D) {
    /* Displayed as ^X in readline-style (e.g., Ctrl+A is ^A). */
    return 2;
  }
  /* DEL character. */
  else if (codepoint == 0x7F) {
    /* Displayed as ^? in readline-style. */
    return 2;
  }
  /* C1 control characters. */
  else if (codepoint >= 0x80 && codepoint <= 0x9F) {
    /* There are different conventions for displaying C1 controls:
     * 1. Some terminals use M-^X notation (4 columns wide, e.g., M-^A)
     * 2. Others use hex notation like <80> (4 columns wide)
     * 3. Some use Unicode code point notation (variable width)
     *
     * We'll return 4 as a reasonable default that works with common conventions.
     */
    return 4;
  }

  /* Not a control character. */
  return -1;
}

/* Reset a width calculation state machine.
 */
void unicode_width_reset(unicode_width_state_t *state) {
  assert(state != NULL);
  state->state = WIDTH_STATE_DEFAULT;
  state->previous_codepoint = 0;
}
""")

def main():
  """Main entry point for the script."""
  print(f"Generating C code for Unicode width calculation...")
  version = load_unicode_version()
  print(f"Unicode version: {version[0]}.{version[1]}.{version[2]}")

  print("Loading width maps...")
  width_map = load_width_maps()

  print("Making tables...")
  tables = make_tables(width_map)

  print("Processing special ranges...")
  special_ranges = make_special_ranges(width_map)

  print("Loading emoji data...")
  emoji_presentations = load_emoji_presentation_sequences()
  emoji_presentation_table = make_presentation_sequence_table(emoji_presentations)

  emoji_modifier_bases = load_emoji_modifier_bases()
  emoji_modifier_table = make_ranges_table(emoji_modifier_bases)

  print("Generating header file...")
  emit_header_file(OUTPUT_HEADER, version)

  print("Generating source file...")
  emit_source_file(
    OUTPUT_SOURCE,
    version,
    tables,
    special_ranges,
    emoji_presentation_table,
    emoji_modifier_table,
  )

  print(f"Done! Generated {OUTPUT_HEADER} and {OUTPUT_SOURCE}.")

if __name__ == "__main__":
  main()
