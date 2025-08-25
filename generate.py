#!/usr/bin/env python3
#
# Generate C code for unicode-width calculations for terminal applications.
# Left-to-right processing; useful subset for terminals (EAW, emoji, combining,
# control chars). All Unicode files are fetched from .../<version>/ucd/<path>.
#
# Copyright 2011-2025 The Rust Project Developers.
# Copyright 2025 Dair Aidarkhanov.
#
# Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
# http://www.apache.org/licenses/LICENSE-2.0> or the MIT license <LICENSE-MIT
# or http://opensource.org/licenses/MIT>, at your option. The generated C
# code is licensed under the 0BSD.
#
# Unicode® data: https://www.unicode.org/license.txt

import enum
import math
import operator
import os
import re
import sys
import urllib.request
from collections import defaultdict
from typing import Callable, Iterable, Dict, List, Tuple

UNICODE_VERSION = "16.0.0"
NUM_CODEPOINTS = 0x110000
MAX_CODEPOINT_BITS = math.ceil(math.log2(NUM_CODEPOINTS - 1))
TABLE_SPLITS = [7, 13]  # leaves: [0..7), middle: [7..13), root: [13..]
OUTPUT_HEADER = "unicode_width.h"
OUTPUT_SOURCE = "unicode_width.c"


class OffsetType(enum.IntEnum):
    U2 = 2
    U4 = 4
    U8 = 8


Codepoint = int
BitPos = int


def fetch_open(path_rel_ucd: str, local_prefix: str = ""):
    """Fetch file from Public/<version>/ucd/<path_rel_ucd> into local cache."""
    basename = os.path.basename(path_rel_ucd)
    localname = os.path.join(local_prefix, basename)
    if not os.path.exists(localname):
        try:
            if not hasattr(fetch_open, "_notice"):
                print("\nDownloading Unicode data files from unicode.org...")
                print("By continuing, you agree to the Unicode License:")
                print("  https://www.unicode.org/license.txt\n")
                fetch_open._notice = True
            url = (
                f"https://www.unicode.org/Public/"
                f"{UNICODE_VERSION}/ucd/{path_rel_ucd}"
            )
            urllib.request.urlretrieve(url, localname)
        except Exception as e:
            sys.stderr.write(f"Error downloading {path_rel_ucd}: {e}\n")
            sys.exit(1)

    try:
        return open(localname, encoding="utf-8")
    except OSError as e:
        sys.stderr.write(f"Cannot load {localname}: {e}\n")
        sys.exit(1)


def load_unicode_version() -> tuple[int, int, int]:
    with fetch_open("ReadMe.txt") as readme:
        m = re.search(r"for Version (\d+)\.(\d+)\.(\d+)", readme.read())
        if not m:
            sys.stderr.write("Could not determine Unicode version\n")
            sys.exit(1)
        return tuple(map(int, m.groups()))


def load_property(path_rel_ucd: str, pattern: str, action: Callable[[int], None]):
    with fetch_open(path_rel_ucd) as properties:
        single = re.compile(rf"^([0-9A-F]+)\s*;\s*{pattern}\s+")
        multiple = re.compile(rf"^([0-9A-F]+)\.\.([0-9A-F]+)\s*;\s*{pattern}\s+")
        for line in properties.readlines():
            raw = None
            if m := single.match(line):
                raw = (m.group(1), m.group(1))
            elif m := multiple.match(line):
                raw = (m.group(1), m.group(2))
            else:
                continue
            lo, hi = int(raw[0], 16), int(raw[1], 16)
            for cp in range(lo, hi + 1):
                action(cp)


def to_sorted_ranges(it: Iterable[Codepoint]) -> list[tuple[Codepoint, Codepoint]]:
    lst = sorted(it)
    out: list[tuple[int, int]] = []
    for cp in lst:
        if out and out[-1][1] == cp - 1:
            out[-1] = (out[-1][0], cp)
        else:
            out.append((cp, cp))
    return out


class EastAsianWidth(enum.IntEnum):
    NARROW = 1
    WIDE = 2
    AMBIGUOUS = 3


class CharWidthInTable(enum.IntEnum):
    ZERO = 0
    ONE = 1
    TWO = 2
    SPECIAL = 3


class WidthState(enum.IntEnum):
    ZERO = 0x10000
    NARROW = 0x10001
    WIDE = 0x10002
    THREE = 0x10003

    LINE_FEED = 0x0001
    EMOJI_MODIFIER = 0x0002
    REGIONAL_INDICATOR = 0x0003
    EMOJI_PRESENTATION = 0x0005

    VARIATION_SELECTOR_15 = 0x4000  # FE0E
    VARIATION_SELECTOR_16 = 0x8000  # FE0F

    def table_width(self) -> CharWidthInTable:
        if self == WidthState.ZERO:
            return CharWidthInTable.ZERO
        if self == WidthState.NARROW:
            return CharWidthInTable.ONE
        if self == WidthState.WIDE:
            return CharWidthInTable.TWO
        return CharWidthInTable.SPECIAL

    def width_alone(self) -> int:
        if self in (
            WidthState.ZERO,
            WidthState.VARIATION_SELECTOR_15,
            WidthState.VARIATION_SELECTOR_16,
        ):
            return 0
        if self in (WidthState.WIDE, WidthState.EMOJI_MODIFIER, WidthState.EMOJI_PRESENTATION):
            return 2
        if self == WidthState.THREE:
            return 3
        return 1


def load_eaw() -> list[EastAsianWidth]:
    with fetch_open("EastAsianWidth.txt") as eaw:
        single = re.compile(r"^([0-9A-F]+)\s*;\s*(\w+) +# ")
        multiple = re.compile(r"^([0-9A-F]+)\.\.([0-9A-F]+)\s*;\s*(\w+) +# ")
        codes = {
            **{c: EastAsianWidth.NARROW for c in ["N", "Na", "H"]},
            **{c: EastAsianWidth.WIDE for c in ["W", "F"]},
            "A": EastAsianWidth.AMBIGUOUS,
        }
        out: list[EastAsianWidth] = []
        cur = 0
        for line in eaw.readlines():
            raw = None
            if m := single.match(line):
                raw = (m.group(1), m.group(1), m.group(2))
            elif m := multiple.match(line):
                raw = (m.group(1), m.group(2), m.group(3))
            else:
                continue
            lo, hi, w = int(raw[0], 16), int(raw[1], 16), codes[raw[2]]
            assert cur <= hi
            while cur <= hi:
                out.append(EastAsianWidth.NARROW if cur < lo else w)
                cur += 1
        while len(out) < NUM_CODEPOINTS:
            out.append(EastAsianWidth.NARROW)

    load_property("LineBreak.txt", "AI", lambda cp: operator.setitem(out, cp, EastAsianWidth.AMBIGUOUS))
    load_property(
        "extracted/DerivedGeneralCategory.txt",
        r"(:?Lu|Ll|Lt|Lm|Lo|Sk)",
        lambda cp: operator.setitem(out, cp, EastAsianWidth.NARROW)
        if out[cp] == EastAsianWidth.AMBIGUOUS
        else None,
    )
    out[0x0387] = EastAsianWidth.AMBIGUOUS

    with fetch_open("UnicodeData.txt") as udata:
        m = re.compile(r"([0-9A-Z]+);.*?;.*?;.*?;.*?;([0-9A-Z]+) 0338;")
        for line in udata.readlines():
            if mm := m.match(line):
                composed = int(mm.group(1), 16)
                decomposed = int(mm.group(2), 16)
                if out[decomposed] == EastAsianWidth.AMBIGUOUS:
                    out[composed] = EastAsianWidth.AMBIGUOUS

    return out


def load_zero_widths() -> list[bool]:
    zw = [False] * NUM_CODEPOINTS
    load_property(
        "DerivedCoreProperties.txt",
        r"(?:Default_Ignorable_Code_Point|Grapheme_Extend)",
        lambda cp: operator.setitem(zw, cp, True),
    )
    load_property("HangulSyllableType.txt", r"(?:V|T)", lambda cp: operator.setitem(zw, cp, True))

    for cp in [0x070F, 0x0605, 0x0890, 0x0891, 0x08E2]:
        zw[cp] = True

    prepend: set[int] = set()
    load_property("auxiliary/GraphemeBreakProperty.txt", "Prepend", lambda cp: prepend.add(cp))
    load_property("PropList.txt", "Prepended_Concatenation_Mark", lambda cp: prepend.discard(cp))
    for cp in prepend:
        zw[cp] = True

    zw[0x115F] = False
    zw[0x2D7F] = False
    zw[0xA8FA] = True
    zw[0x200D] = True
    return zw


def build_width_map() -> list[WidthState]:
    eaw = load_eaw()
    zw = load_zero_widths()
    wm: list[WidthState] = []
    for w, z in zip(eaw, zw):
        wm.append(WidthState.ZERO if z else (WidthState.WIDE if w == EastAsianWidth.WIDE else WidthState.NARROW))

    ri: list[int] = []
    load_property("PropList.txt", "Regional_Indicator", lambda cp: ri.append(cp))

    emod: list[int] = []
    load_property("emoji/emoji-data.txt", "Emoji_Modifier", lambda cp: emod.append(cp))

    epres: list[int] = []
    load_property("emoji/emoji-data.txt", "Emoji_Presentation", lambda cp: epres.append(cp))

    specials = [
        ([0x000A], WidthState.LINE_FEED),
        ([0x17A4], WidthState.WIDE),
        ([0x17D8], WidthState.THREE),
        ([0xFE0E], WidthState.VARIATION_SELECTOR_15),
        ([0xFE0F], WidthState.VARIATION_SELECTOR_16),
        (epres, WidthState.EMOJI_PRESENTATION),
        (emod, WidthState.EMOJI_MODIFIER),
        (ri, WidthState.REGIONAL_INDICATOR),
    ]
    for cps, w in specials:
        for cp in cps:
            wm[cp] = w
    return wm


def make_special_ranges(wm: list[WidthState]) -> list[tuple[tuple[int, int], WidthState]]:
    out: list[tuple[tuple[int, int], WidthState]] = []
    can_merge = False
    for cp, w in enumerate(wm):
        if w == WidthState.EMOJI_PRESENTATION:
            can_merge = False
        elif w.table_width() == CharWidthInTable.SPECIAL:
            if can_merge and out[-1][1] == w:
                out[-1] = ((out[-1][0][0], cp), w)
            else:
                out.append(((cp, cp), w))
                can_merge = True
    return out


class Bucket:
    def __init__(self):
        self.entry_set: set[tuple[int, CharWidthInTable]] = set()
        self.widths: list[CharWidthInTable] = []

    def append(self, cp: Codepoint, width: CharWidthInTable):
        self.entry_set.add((cp, width))
        self.widths.append(width)

    def try_extend(self, other: "Bucket") -> bool:
        less, more = (self.widths, other.widths)
        if len(less) > len(more):
            less, more = more, less
        if less != more[: len(less)]:
            return False
        self.entry_set |= other.entry_set
        self.widths = more
        return True

    def entries(self) -> list[tuple[Codepoint, CharWidthInTable]]:
        lst = list(self.entry_set)
        lst.sort()
        return lst

    def width(self) -> CharWidthInTable | None:
        if not self.widths:
            return None
        ref = self.widths[0]
        for w in self.widths[1:]:
            if w != ref:
                return None
        return ref


def make_buckets(
    entries: Iterable[tuple[int, CharWidthInTable]], low_bit: BitPos, cap_bit: BitPos
) -> list[Bucket]:
    num_bits = cap_bit - low_bit
    assert num_bits > 0
    buckets = [Bucket() for _ in range(0, 2**num_bits)]
    mask = (1 << num_bits) - 1
    for cp, width in entries:
        buckets[(cp >> low_bit) & mask].append(cp, width)
    return buckets


class Table:
    def __init__(
        self,
        name: str,
        entry_groups: Iterable[Iterable[tuple[int, CharWidthInTable]]],
        low_bit: BitPos,
        cap_bit: BitPos,
        offset_type: OffsetType,
        align: int,
        bytes_per_row: int | None = None,
        starting_indexed: list[Bucket] | None = None,
    ):
        self.name = name
        self.low_bit = low_bit
        self.cap_bit = cap_bit
        self.offset_type = offset_type
        self.entries: list[int] = []
        self.indexed: list[Bucket] = list(starting_indexed) if starting_indexed else []
        self.align = align
        self.bytes_per_row = bytes_per_row

        buckets: list[Bucket] = []
        for seq in entry_groups:
            buckets.extend(make_buckets(seq, self.low_bit, self.cap_bit))

        for b in buckets:
            for i, exist in enumerate(self.indexed):
                if exist.try_extend(b):
                    self.entries.append(i)
                    break
            else:
                self.entries.append(len(self.indexed))
                self.indexed.append(b)

        max_index = 1 << int(self.offset_type)
        for idx in self.entries:
            assert idx < max_index, f"{idx} <= {max_index}"

    def indices_to_widths(self):
        self.entries = [int(self.indexed[i].width()) for i in self.entries]  # type: ignore
        del self.indexed

    def buckets(self):
        return self.indexed

    def to_bytes(self) -> list[int]:
        per_byte = 8 // int(self.offset_type)
        out: list[int] = []
        for i in range(0, len(self.entries), per_byte):
            byte = 0
            for j in range(0, per_byte):
                if i + j < len(self.entries):
                    byte |= self.entries[i + j] << (j * int(self.offset_type))
            out.append(byte)
        return out


def make_tables(wm: list[WidthState]) -> list[Table]:
    entries = enumerate([w.table_width() for w in wm])

    root = Table("WIDTH_ROOT", [entries], TABLE_SPLITS[1], MAX_CODEPOINT_BITS, OffsetType.U8, 128)

    middle = Table(
        "WIDTH_MIDDLE",
        map(lambda b: b.entries(), root.buckets()),
        TABLE_SPLITS[0],
        TABLE_SPLITS[1],
        OffsetType.U8,
        2 ** (TABLE_SPLITS[1] - TABLE_SPLITS[0]),
        bytes_per_row=2 ** (TABLE_SPLITS[1] - TABLE_SPLITS[0]),
    )

    leaves = Table(
        "WIDTH_LEAVES",
        map(lambda b: b.entries(), middle.buckets()),
        0,
        TABLE_SPLITS[0],
        OffsetType.U2,
        2 ** (TABLE_SPLITS[0] - 2),
        bytes_per_row=2 ** (TABLE_SPLITS[0] - 2),
    )

    leaves.indices_to_widths()
    return [root, middle, leaves]


def load_emoji_presentation_starts() -> list[Codepoint]:
    with fetch_open("emoji/emoji-variation-sequences.txt") as seqs:
        rx = re.compile(r"^([0-9A-F]+)\s+FE0F\s*;\s*emoji style")
        out: list[int] = []
        for line in seqs.readlines():
            if m := rx.match(line):
                out.append(int(m.group(1), 16))
    return out


def load_emoji_modifier_bases() -> list[Codepoint]:
    out: list[int] = []
    load_property("emoji/emoji-data.txt", "Emoji_Modifier_Base", lambda cp: out.append(cp))
    out.sort()
    return out


def load_extended_pictographic() -> list[Codepoint]:
    out: list[int] = []
    load_property("emoji/emoji-data.txt", "Extended_Pictographic", lambda cp: out.append(cp))
    out.sort()
    return out


def load_emoji_yes() -> list[Codepoint]:
    out: list[int] = []
    load_property("emoji/emoji-data.txt", "Emoji", lambda cp: out.append(cp))
    out.sort()
    return out


def make_presentation_sequence_table(
    cps: list[Codepoint], lsb: int = 10
) -> tuple[list[tuple[int, int]], list[list[int]]]:
    pref: Dict[int, set[int]] = defaultdict(set)
    for cp in cps:
        pref[cp >> lsb].add(cp & (2**lsb - 1))

    msbs = sorted(pref.keys())
    leaves: list[list[int]] = []
    index_map: Dict[int, int] = {}

    for i, msb in enumerate(msbs):
        bits = [0] * (2 ** (lsb - 3))
        for val in sorted(pref[msb]):
            idx, bit = divmod(val, 8)
            bits[idx] |= 1 << bit
        leaves.append(bits)
        index_map[msb] = i

    indexes: list[tuple[int, int]] = []
    uniq_leaves: list[list[int]] = []
    leaf_ids: Dict[tuple[int, ...], int] = {}
    for msb in msbs:
        key = tuple(leaves[index_map[msb]])
        if key in leaf_ids:
            indexes.append((msb, leaf_ids[key]))
        else:
            leaf_ids[key] = len(uniq_leaves)
            uniq_leaves.append(list(key))
            indexes.append((msb, leaf_ids[key]))

    return (indexes, uniq_leaves)


def make_ranges_table(
    cps: list[Codepoint],
) -> tuple[list[tuple[int, int]], list[list[tuple[int, int]]]]:
    pref: Dict[int, list[int]] = defaultdict(list)
    for cp in cps:
        pref[cp >> 8].append(cp & 0xFF)

    msbs = sorted(pref.keys())
    leaves: list[list[tuple[int, int]]] = []
    index_map: Dict[int, int] = {}

    for i, msb in enumerate(msbs):
        arr = sorted(pref[msb])
        leaf: list[tuple[int, int]] = []
        for v in arr:
            if leaf and leaf[-1][1] == v - 1:
                leaf[-1] = (leaf[-1][0], v)
            else:
                leaf.append((v, v))
        leaves.append(leaf)
        index_map[msb] = i

    indexes: list[tuple[int, int]] = []
    uniq_leaves: list[list[tuple[int, int]]] = []
    leaf_ids: Dict[tuple[tuple[int, int], ...], int] = {}

    for msb in msbs:
        key = tuple(leaves[index_map[msb]])
        if key in leaf_ids:
            indexes.append((msb, leaf_ids[key]))
        else:
            leaf_ids[key] = len(uniq_leaves)
            uniq_leaves.append(list(key))
            indexes.append((msb, leaf_ids[key]))

    return (indexes, uniq_leaves)


def emit_header(path: str, ver: tuple[int, int, int]):
    with open(path, "w", newline="\n") as f:
        f.write(
            f"""/* Generated by unicode-width generator.
 *
 * Unicode {ver[0]}.{ver[1]}.{ver[2]} data.
 * For terminal width calculation.
 *
 * Copyright 2025 Dair Aidarkhanov
 * SPDX-License-Identifier: 0BSD
 */

#ifndef UNICODE_WIDTH_H_
#define UNICODE_WIDTH_H_

#include <stddef.h> /* size_t */

#if defined(__STDC_VERSION__) && (__STDC_VERSION__ >= 199901L)
  #include <stdint.h> /* uint_least32_t, uint_least16_t, uint_least8_t */
#else
  #include <limits.h>

  /* Compile-time environment checks for C89. */

  /* Enforce that char is at least 8 bits. */
  #if CHAR_BIT < 8
    #error "this implementation has `CHAR_BIT` < 8; unsupported"
  #endif

  /* uint_least8_t: choose the smallest type with at least 8 bits. */
  #if UCHAR_MAX >= 0xFFu
  typedef unsigned char  uint_least8_t;
  #elif USHRT_MAX >= 0xFFu
  typedef unsigned short uint_least8_t;
  #elif UINT_MAX >= 0xFFu
  typedef unsigned int   uint_least8_t;
  #elif ULONG_MAX >= 0xFFul
  typedef unsigned long  uint_least8_t;
  #else
  #error "no suitable type for `uint_least8_t` (need >= 8 bits)"
  #endif

  /* uint_least16_t: choose the smallest type with at least 16 bits. */
  #if USHRT_MAX >= 0xFFFFu
  typedef unsigned short uint_least16_t;
  #elif UINT_MAX >= 0xFFFFu
  typedef unsigned int   uint_least16_t;
  #elif ULONG_MAX >= 0xFFFFul
  typedef unsigned long  uint_least16_t;
  #else
  #error "no suitable type for `uint_least16_t` (need >= 16 bits)"
  #endif

  /* uint_least32_t: choose the smallest type with at least 32 bits. */
  #if UINT_MAX >= 0xFFFFFFFFu
  typedef unsigned int  uint_least32_t;
  #elif ULONG_MAX >= 0xFFFFFFFFul
  typedef unsigned long uint_least32_t;
  #else
  #error "no suitable type for `uint_least32_t` (need >= 32 bits)"
  #endif

  /* Enforce that unsigned long is at least 32 bits. */
  #if ULONG_MAX < 0xFFFFFFFFul
  #error "this implementation has unsigned long < 32 bits; unsupported"
  #endif
#endif

#ifdef __cplusplus
extern "C" {{
#endif

#define UNICODE_WIDTH_VERSION_MAJOR {ver[0]}
#define UNICODE_WIDTH_VERSION_MINOR {ver[1]}
#define UNICODE_WIDTH_VERSION_PATCH {ver[2]}

typedef enum {{
  WIDTH_STATE_DEFAULT     = 0,
  WIDTH_STATE_AFTER_CR    = 1,
  WIDTH_STATE_RI_ODD      = 2,
  WIDTH_STATE_RI_EVEN     = 3,
  WIDTH_STATE_ZWJ_PENDING = 4,
  WIDTH_STATE_ZWJ_ACTIVE  = 5
}} width_state_t;

typedef struct {{
  width_state_t  state;
  uint_least32_t prev_codepoint;
  uint_least8_t  last_base_width;
  uint_least8_t  last_base_is_emoji_variation;
}} unicode_width_state_t;

void unicode_width_init(unicode_width_state_t *state);
int  unicode_width_process(unicode_width_state_t *state, uint_least32_t codepoint);
int  unicode_width_control_char(uint_least32_t codepoint);
void unicode_width_reset(unicode_width_state_t *state);

#ifdef __cplusplus
}} /* extern "C" */
#endif

#endif /* UNICODE_WIDTH_H_ */
"""
        )


def emit_source(
    path: str,
    ver: tuple[int, int, int],
    tables: list[Table],
    special_ranges: list[tuple[tuple[Codepoint, Codepoint], WidthState]],
    epres_table: tuple[list[tuple[int, int]], list[list[int]]],
    emod_table: tuple[list[tuple[int, int]], list[list[tuple[int, int]]]],
    epicto_table: tuple[list[tuple[int, int]], list[list[tuple[int, int]]]],
    emoji_yes_table: tuple[list[tuple[int, int]], list[list[tuple[int, int]]]],
):
    with open(path, "w", newline="\n") as f:
        f.write(
            f"""/* Generated by unicode-width generator.
 *
 * Unicode {ver[0]}.{ver[1]}.{ver[2]} data.
 * For terminal width calculation.
 *
 * Copyright 2025 Dair Aidarkhanov
 * SPDX-License-Identifier: 0BSD
 */

#include "unicode_width.h"
#include <assert.h>

/* Base width tables (root/middle/leaves). */
"""
        )

        # Base tables
        for t in tables:
            data = t.to_bytes()
            if t.bytes_per_row is None:
                f.write(f"static const uint_least8_t {t.name}[{len(data)}] = {{\n")
                for i, b in enumerate(data):
                    if i % 16 == 0:
                        f.write("  ")
                    f.write(f" 0x{b:02X},")
                    if i % 16 == 15:
                        f.write("\n")
                if len(data) % 16 != 0:
                    f.write("\n")
                f.write("};\n\n")
            else:
                rows = len(data) // t.bytes_per_row
                f.write(f"static const uint_least8_t {t.name}[{rows}][{t.bytes_per_row}] = {{\n")
                for r in range(rows):
                    f.write("  {")
                    row = data[r * t.bytes_per_row : (r + 1) * t.bytes_per_row]
                    for j, b in enumerate(row):
                        if j % 16 == 0 and j > 0:
                            f.write("\n   ")
                        f.write(f" 0x{b:02X},")
                    f.write("\n  },\n")
                f.write("};\n\n")

        # Emoji presentation bitmaps
        ep_idx, ep_leaves = epres_table
        if len(ep_leaves) > 0:
            for i, leaf in enumerate(ep_leaves):
                f.write(f"static const uint_least8_t EP_LEAF_{i}[128] = {{\n")
                for j, b in enumerate(leaf):
                    if j % 16 == 0:
                        f.write("  ")
                    f.write(f" 0x{b:02X},")
                    if j % 16 == 15:
                        f.write("\n")
                if len(leaf) % 16 != 0:
                    f.write("\n")
                f.write("};\n")
            f.write("\n")

        def emit_range_leaves(prefix: str, leaves: list[list[tuple[int, int]]]):
            if len(leaves) == 0:
                return
            for i, leaf in enumerate(leaves):
                f.write(f"static const uint_least8_t {prefix}_LEAF_{i}_LO[{len(leaf)}] = {{")
                for lo, hi in leaf:
                    f.write(f" 0x{lo:02X},")
                f.write(" };\n")
                f.write(f"static const uint_least8_t {prefix}_LEAF_{i}_HI[{len(leaf)}] = {{")
                for lo, hi in leaf:
                    f.write(f" 0x{hi:02X},")
                f.write(" };\n")
            f.write("\n")

        em_idx, em_leaves = emod_table
        epi_idx, epi_leaves = epicto_table
        ey_idx, ey_leaves = emoji_yes_table
        emit_range_leaves("EMOD", em_leaves)
        emit_range_leaves("EPICTO", epi_leaves)
        emit_range_leaves("EYES", ey_leaves)

        # is_emoji_presentation_start
        if len(ep_idx) == 0:
            f.write("static int is_emoji_presentation_start(uint_least32_t cp) {(void)cp; return 0;}\n\n")
        else:
            f.write("static int is_emoji_presentation_start(uint_least32_t cp) {\n")
            f.write("  switch (cp >> 10) {\n")
            for msb, lid in ep_idx:
                f.write(f"    case 0x{msb:X}: {{\n")
                f.write("      uint_least16_t bottom = (uint_least16_t)(cp & 0x3FF);\n")
                f.write(f"      return (EP_LEAF_{lid}[bottom >> 3] >> (bottom & 7)) & 1;\n")
                f.write("    }\n")
            f.write("    default: return 0;\n")
            f.write("  }\n")
            f.write("}\n\n")

        # Range table helpers
        def emit_range_fn(name: str, idx: list[tuple[int, int]], leaves: list[list[tuple[int, int]]], pfx: str):
            if len(idx) == 0:
                f.write(f"static int {name}(uint_least32_t cp) {{ (void)cp; return 0; }}\n\n")
                return
            f.write(f"static int {name}(uint_least32_t cp) {{\n")
            f.write("  switch (cp >> 8) {\n")
            for msb, lid in idx:
                n = len(leaves[lid])
                f.write(f"    case 0x{msb:X}: {{\n")
                f.write("      uint_least8_t bottom = (uint_least8_t)(cp & 0xFF);\n")
                if n == 0:
                    f.write("      return 0;\n")
                else:
                    f.write("      int lo = 0;\n")
                    f.write(f"      int hi = {n - 1};\n")
                    f.write("      while (lo <= hi) {\n")
                    f.write("        int mid = (lo + hi) / 2;\n")
                    f.write(f"        if (bottom < {pfx}_LEAF_{lid}_LO[mid]) hi = mid - 1;\n")
                    f.write(f"        else if (bottom > {pfx}_LEAF_{lid}_HI[mid]) lo = mid + 1;\n")
                    f.write("        else return 1;\n")
                    f.write("      }\n")
                    f.write("      return 0;\n")
                f.write("    }\n")
            f.write("    default: return 0;\n")
            f.write("  }\n")
            f.write("}\n\n")

        emit_range_fn("is_emoji_modifier_base", em_idx, em_leaves, "EMOD")
        emit_range_fn("is_extended_pictographic", epi_idx, epi_leaves, "EPICTO")
        emit_range_fn("is_emoji", ey_idx, ey_leaves, "EYES")

        # Base width lookup with classification
        f.write("""typedef enum {
  WC_DEFAULT = 0,
  WC_LINE_FEED,
  WC_EMOJI_MODIFIER,
  WC_REGIONAL_INDICATOR,
  WC_EMOJI_PRESENTATION,
  WC_VS15,
  WC_VS16
} width_class_t;\n\n""")

        f.write(
            f"""static uint_least8_t table_leaf_index(uint_least32_t cp) {{
  uint_least8_t t1;
  uint_least8_t t2;
  t1 = WIDTH_ROOT[cp >> {TABLE_SPLITS[1]}];
  t2 = WIDTH_MIDDLE[t1][(cp >> {TABLE_SPLITS[0]}) & 0x{(2 ** (TABLE_SPLITS[1] - TABLE_SPLITS[0]) - 1):X}];
  return t2;
}}

static void lookup_char(uint_least32_t cp, int *width_out, width_class_t *class_out) {{
  uint_least8_t leaf;
  uint_least8_t packed;
  uint_least8_t w;

  leaf = table_leaf_index(cp);
  packed = WIDTH_LEAVES[leaf][(cp >> 2) & 0x{(2 ** (TABLE_SPLITS[0] - 2) - 1):X}];
  w = (uint_least8_t)((packed >> (2 * (cp & 3))) & 0x3);

  if (w < 3) {{
    *width_out = (int)w;
    *class_out = WC_DEFAULT;
    return;
  }}

  switch (cp) {{
"""
        )
        for (lo, hi), ws in special_ranges:
            f.write(f"    case 0x{lo:X}:")
            if hi != lo:
                for cpv in range(lo + 1, hi + 1):
                    f.write(f"\n    case 0x{cpv:X}:")
            if ws == WidthState.LINE_FEED:
                f.write(" *class_out = WC_LINE_FEED; *width_out = 0; break;\n")
            elif ws == WidthState.EMOJI_MODIFIER:
                f.write(" *class_out = WC_EMOJI_MODIFIER; *width_out = 2; break;\n")
            elif ws == WidthState.REGIONAL_INDICATOR:
                f.write(" *class_out = WC_REGIONAL_INDICATOR; *width_out = 1; break;\n")
            elif ws == WidthState.EMOJI_PRESENTATION:
                f.write(" *class_out = WC_EMOJI_PRESENTATION; *width_out = 2; break;\n")
            elif ws == WidthState.VARIATION_SELECTOR_15:
                f.write(" *class_out = WC_VS15; *width_out = 0; break;\n")
            elif ws == WidthState.VARIATION_SELECTOR_16:
                f.write(" *class_out = WC_VS16; *width_out = 0; break;\n")
            else:
                f.write(f" *class_out = WC_DEFAULT; *width_out = {ws.width_alone()}; break;\n")
        f.write(
            """    default:
      *class_out = WC_EMOJI_PRESENTATION;
      *width_out = 2;
      break;
  }
}
"""
        )

        # State machine
        f.write(
            """
void unicode_width_init(unicode_width_state_t *state) {
  assert(state != NULL);
  state->state = WIDTH_STATE_DEFAULT;
  state->prev_codepoint = 0;
  state->last_base_width = 0;
  state->last_base_is_emoji_variation = 0;
}

static int is_control_char(uint_least32_t cp) {
  if (cp < 0x20u) return 1; /* includes CR/LF */
  if (cp == 0x7Fu) return 1;
  if (cp >= 0x80u && cp <= 0x9Fu) return 1;
  return 0;
}

static int qualifies_zwj_partner(uint_least32_t cp) {
  if (is_emoji_presentation_start(cp)) return 1;
  if (is_extended_pictographic(cp) || is_emoji(cp)) return 1;
  if (cp >= 0x1F1E6u && cp <= 0x1F1FFu) return 1;
  if (cp == 0x23u || cp == 0x2Au || (cp >= (uint_least32_t)'0' && cp <= (uint_least32_t)'9')) return 1;
  if (is_emoji_modifier_base(cp)) return 1;
  return 0;
}

int unicode_width_process(unicode_width_state_t *state, uint_least32_t cp) {
  int base_width = 0;
  width_class_t wc = WC_DEFAULT;

  assert(state != NULL);

  /* Controls */
  if (is_control_char(cp)) {
    if (cp == 0x0Au) { /* LF */
      state->state = WIDTH_STATE_DEFAULT;
      state->prev_codepoint = cp;
      state->last_base_width = 0;
      state->last_base_is_emoji_variation = 0;
      return 0;
    }
    if (cp == 0x0Du) { /* CR */
      state->state = WIDTH_STATE_AFTER_CR;
      state->prev_codepoint = cp;
      state->last_base_width = 0;
      state->last_base_is_emoji_variation = 0;
      return 0;
    }
    state->prev_codepoint = cp;
    state->last_base_width = 0;
    state->last_base_is_emoji_variation = 0;
    return -1;
  }

  /* CRLF */
  if (state->state == WIDTH_STATE_AFTER_CR) {
    if (cp == 0x0Au) {
      state->state = WIDTH_STATE_DEFAULT;
      state->prev_codepoint = cp;
      state->last_base_width = 0;
      state->last_base_is_emoji_variation = 0;
      return 0;
    } else {
      state->state = WIDTH_STATE_DEFAULT;
    }
  }

  /* Inside ZWJ cluster */
  if (state->state == WIDTH_STATE_ZWJ_ACTIVE) {
    if (cp == 0xFE0Fu || cp == 0xFE0Eu) {
      state->state = WIDTH_STATE_DEFAULT;
      state->prev_codepoint = cp;
      return 0;
    } else if (cp == 0x200Du) {
      state->state = WIDTH_STATE_ZWJ_PENDING;
      state->prev_codepoint = cp;
      return 0;
    } else {
      state->state = WIDTH_STATE_DEFAULT;
    }
  }

  /* VS adjustments (outside ZWJ-active) */
  if (cp == 0xFE0Fu) {
    int adjust = 0;
    if (state->last_base_is_emoji_variation) {
      if (state->last_base_width == 1u) adjust = +1;
    }
    state->prev_codepoint = cp;
    state->last_base_width = 0;
    state->last_base_is_emoji_variation = 0;
    return adjust;
  }
  if (cp == 0xFE0Eu) {
    int adjust = 0;
    if (state->last_base_is_emoji_variation) {
      if (state->last_base_width == 2u) adjust = -1;
    }
    state->prev_codepoint = cp;
    state->last_base_width = 0;
    state->last_base_is_emoji_variation = 0;
    return adjust;
  }

  /* ZWJ starts pending */
  if (cp == 0x200Du) {
    state->state = WIDTH_STATE_ZWJ_PENDING;
    state->prev_codepoint = cp;
    return 0;
  }

  /* If ZWJ pending, only next qualifying cp joins */
  if (state->state == WIDTH_STATE_ZWJ_PENDING) {
    if (qualifies_zwj_partner(cp)) {
      state->state = WIDTH_STATE_ZWJ_ACTIVE;
      state->prev_codepoint = cp;
      state->last_base_width = 0;
      state->last_base_is_emoji_variation = 0;
      return 0;
    } else {
      state->state = WIDTH_STATE_DEFAULT;
    }
  }

  /* Skin tone modifiers after emoji-capable base => zero width */
  if (cp >= 0x1F3FBu && cp <= 0x1F3FFu) {
    if (state->last_base_is_emoji_variation) {
      state->prev_codepoint = cp;
      return 0;
    }
  }

  lookup_char(cp, &base_width, &wc);

  /* Regional Indicator parity */
  if (wc == WC_REGIONAL_INDICATOR) {
    if (state->state == WIDTH_STATE_RI_ODD) {
      state->state = WIDTH_STATE_RI_EVEN;
    } else {
      state->state = WIDTH_STATE_RI_ODD;
    }
  } else {
    if (state->state == WIDTH_STATE_RI_EVEN || state->state == WIDTH_STATE_RI_ODD) {
      state->state = WIDTH_STATE_DEFAULT;
    }
  }

  /* Track if this base can vary with VS */
  {
    int can_vary = 0;
    if (wc == WC_EMOJI_PRESENTATION || is_emoji_presentation_start(cp)) {
      can_vary = 1;
    }
    {
      int ret = base_width;
      state->last_base_width = (uint_least8_t)((ret <= 0) ? 0 : ((ret > 2) ? 2 : ret));
      state->last_base_is_emoji_variation = (uint_least8_t)(can_vary ? 1 : 0);
      state->prev_codepoint = cp;
      return ret;
    }
  }
}
"""
        )

        f.write(
            """
int unicode_width_control_char(uint_least32_t codepoint) {
  if (codepoint < 0x20u && codepoint != 0x0Au && codepoint != 0x0Du) return 2;
  if (codepoint == 0x7Fu) return 2;
  if (codepoint >= 0x80u && codepoint <= 0x9Fu) return 4;
  return -1;
}

void unicode_width_reset(unicode_width_state_t *state) {
  unicode_width_init(state);
}
"""
        )


def main():
    print("Generating C code for Unicode width calculation...")
    ver = load_unicode_version()
    print(f"Unicode version: {ver[0]}.{ver[1]}.{ver[2]}")

    print("Loading width map...")
    wm = build_width_map()

    print("Building tables...")
    tables = make_tables(wm)
    special = make_special_ranges(wm)

    print("Building property tables...")
    epres = make_presentation_sequence_table(load_emoji_presentation_starts())
    emod = make_ranges_table(load_emoji_modifier_bases())
    epict = make_ranges_table(load_extended_pictographic())
    eyes = make_ranges_table(load_emoji_yes())

    print("Emitting header/source...")
    emit_header(OUTPUT_HEADER, ver)
    emit_source(OUTPUT_SOURCE, ver, tables, special, epres, emod, epict, eyes)

    print(f"Done. Generated {OUTPUT_HEADER} and {OUTPUT_SOURCE}.")


if __name__ == "__main__":
    main()
