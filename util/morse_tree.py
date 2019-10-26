# MORSE CODE TREE GENERATOR
# Written for Reddit /r/DailyProgrammer Challenge #380
# [Intermediate] Smooshed Morse Code 2
# https://www.reddit.com/r/dailyprogrammer/comments/cn6gz5
#
# Copyright (C) 2019 Andrew Smith
# 
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.


# Latin alphabet -> Morse code translation table
MORSE_LETTERS = {
    'a': '.-',      'b': '-...',    'c': '-.-.',    'd': '-..',
    'e': '.',       'f': '..-.',    'g': '--.',     'h': '....',
    'i': '..',      'j': '.---',    'k': '-.-',     'l': '.-..',
    'm': '--',      'n': '-.',      'o': '---',     'p': '.--.',
    'q': '--.-',    'r': '.-.',     's': '...',     't': '-',
    'u': '..-',     'v': '...-',    'w': '.--',     'x': '-..-',
    'y': '-.--',    'z': '--..'
}

# Morse code can be represented as a binary tree (trie) with a depth of 4.
# Each dot (dit) is interpreted as navigating to the left of the current node,
# and a dash (dah) as navigating to the right.  This tree will have at most 31
# nodes.  For simplicity, if a node represents a valid Morse pattern for a
# letter, that letter is placed in the corresponding node in the tree; for
# invalid patterns, a space is used as a placeholder as it is a named constant
# in COBOL.  The tree is further flattened to an array representing its in-order
# traversal.
morse_tree = [' '] * 31


def place_letter(pattern: str, letter: str, index: int, adjust: int) -> None:
    """
    Recursive function to place the LETTER corresponding to a PATTERN into the
    binary tree as a child of the node at the specified INDEX.  If the first
    character in the pattern is a dot, navigate leftward; if it is a dash, go
    rightward.  The distance to navigate left or right is controlled by ADJUST.
    """
    if len(pattern) == 0:
        # Pattern exhausted: insert the letter
        morse_tree[index] = letter
    else:
        # Pattern remains: recursive call
        next_index = index - adjust if pattern[0] == '.' else index + adjust
        place_letter(pattern[1:], letter, next_index, adjust // 2)


if __name__ == '__main__':
    # Place each letter in the tree, beginning at the center
    for latin, morse in MORSE_LETTERS.items():
        place_letter(morse, latin, 15, 8)
    # Serialise the flat tree to a string for ease of use in COBOL
    print('"' + ''.join(morse_tree) + '"')