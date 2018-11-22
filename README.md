txtfmt
======

Txtfmt (The Vim Highlighter) : "Rich text" highlighting in Vim! (text color, highlighting, underline, bold, italic, etc...)

<br>**Note:** If you wish to try the examples below, you will need to install and load the plugin. If you're in a hurry, and are familiar with plugin installation, just drop the bundle of files in the proper location and do...

`set ft=txtfmt`

Alternatively, you can have Txtfmt create a "test page" for you:

`:MakeTestPage`

The test page offers a quick way to verify that the plugin is installed properly, but it also provides a convenient mechanism for visualizing the effects of various configuration changes. When executed with no arguments, `:MakeTestPage` uses default settings, taking into account any Txtfmt option variables you've set in your vimrc. But if you supply arguments, `:MakeTestPage` will process them as though they were part of a "Txtfmt modeline" within the test page itself.

`:help txtfmt-:MakeTestPage`

The "Installation Details" section (further down) discusses plugin load in greater detail... 

# Introduction
Txtfmt brings rich text formatting to plain text files. The commands used to accomplish highlighting fall into 2 categories:

* **Auto maps:** Change the highlighting of a range of text. There are currently 2 types of auto map:
    * Visual: Operate on the visually selected text 
    * Operator: Operate on the text moved over (or included in a "text object") 
* **Manual maps:** Insert one ore more _tokens_, which affect all subsequent text up to the next token.

**Note:** Txtfmt's highlighting relies upon invisible characters (tokens) in the text, but you needn't know or care about this if you're using auto maps, which completely automate token insertion/removal. Manual maps (the _only_ type available until Txtfmt 3.0) do not shield the user nearly so well from this implementation detail, and hence, are provided mostly for backwards compatibility.

**Note:** All examples on this page assume that your <LocalLeader> is at the default value (i.e., backslash). If you have set <LocalLeader> to something other than the default, replace the backslash in the examples with the appropriate character.
    :help <LocalLeader>

# Auto Maps
Before diving into the details of auto maps, I'll present a few examples that show how easy they are to use.

## Visual Auto Maps
**Example:** Make selected text blue, bold-italic. 
1. Select the text to be highlighted using either mouse or (non-blockwise) visual mode (e.g., v or V).
1. Type `\h`
1. At the prompt, type `f=bi,cb`
1. Hit Enter to apply highlighting
<br>**Note:** You could also have typed `fbi,cb` (without the `=`) or even `f+bi,cb`, either of which would have _added_ bold-italic on top of any existing format attributes.

## Operator Auto Maps
**Example (part 1):** Make the word under the cursor bold red.
1. In Normal mode, position cursor on word to be highlighted
1. Type `\h` (enters "operator-pending" mode)
1. Type `iw` (specifies "inner word" text object)
1. At the prompt, type `cr,f=b`
1. Hit Enter to apply highlighting

**Example (part 2):** Italicize and underline the current and subsequent 2 lines, highlighting the background green.
1. In Normal mode, with cursor still in the line from the preceding example...<br>
   Type `\h` (enters "operator-pending" mode)
1. Hit `2j` (includes current and next 2 lines in range)
1. At the prompt, type `fui,kg`
1. Hit Enter to apply highlighting
<br>**Note:** `fui` is short for `f+ui`, which _adds_ the underline and italic attributes to the existing highlighting. Thus, the word you highlighted in the preceding example has retained the bold attribute, and is now bold-underline-italic. Use `f=` instead of `f+` when you wish to _replace_ or _overwrite_ any existing highlighting.

## Highlighting Spec Basics
Strings such as `f=bi,cb` and `fui` are known as _highlighting specs_. Each spec can be a comma or space-separated list of format/color components, so it's possible to alter foreground color, background color and format attributes with a single command. The first letter of each component determines the type of highlighting affected:

Letter | Type
-------|-----
f | format attributes
c | foreground color
k | background color

<br>**Orthogonality Note:** Because the 3 types of highlighting are completely orthogonal, there is no need to specify components whose highlighting you do not wish to change: e.g., `fb` adds bold attribute without affecting text color or background color in any way.

### Color Specs
For fg/bg colors, a color name or hyphen (`'-'`) follows the `c` or `k`. The color names are configurable, but the defaults allow common colors to be abbreviated to a single letter: e.g., `r`=red, `b`=blue, `g`=green, etc... A `'-'` specifies "no color": i.e., it removes color from the region. Both the color RGB values and the color names are completely configurable. Details may be found in section "Color Configuration" below...

### Format Attribute Specs
The `f` of a format specification is followed by a string of operators and single character format attribute flags:

#### Operators
Operator | Description
---------|----------
`+` (default) | Add to existing format attributes
`-` | Remove from existing format attributes
`=` | Replace/overwrite existing format attributes

Each operator applies to all subsequent format attribute flags until the next operator.

#### Format Attribute Flags
Attributes | Description | Availability
-----------|-------------|-------------
u | underline
b | bold
i | italic
s | standout | "long" 'tokrange' only
r | reverse | "long" 'tokrange' only
c | undercurl | "long" 'tokrange' only (Vim version >= 7)
<br>
**Note:** The default configuration provides only underline, bold and italic. If you wish to enable standout, reverse and undercurl, read section "Token Range" below.


### Highlighting Spec Examples
Spec | Result
-----|-------
`fbi` | Add bold-italic
`f+bi` | _same as previous_
`f+bi-u` | Add bold-italic and remove underline
`f=bi` | Replace any existing format attributes with bold-italic
`f-,c-` | Remove all format attributes and any foreground color
`fu-b,cb,kr` | Add underline and remove bold, make text color blue and background color red
`fu-b cb kr` | _same as previous_

`:help txtfmt-fmt-spec`
`:help txtfmt-clr-spec`

## Selective (Pattern-Based) Highlighting
Up until now, we've been applying highlighting to _all_ of the text in a range. It is also possible to target specific _sub-regions_ within the visually-selected or operated-on text. To apply highlighting selectively, append a `/` to the highlighting spec, followed by a "selector pattern expression": e.g.,

&nbsp;&nbsp;&nbsp;&nbsp;_highlighting-spec_ `/` _selector-pattern_

**Usage Example:** Suppose after highlighting many words and phrases in your document with `f=b,cr,kg` (bold, red text on green background), you decide that `f=bi,cb` (bold-italic, blue text) would have been a better choice. Making the change manually would be tedious; fortunately, selector patterns provide a better way. Simply select a range of lines containing all the text you wish to change and execute...

1. Select a range of lines containing all the text you wish to change.
1. Type `\h`
1. At the prompt, type `f+i,cb,k- / fb && cr && kg`
1. Hit Enter to apply highlighting

**Explanation:** The highlighting spec adds italic, changes text color to blue and removes background highlighting altogether, but affects **_only_** bold red text on a green background.

Selector patterns are essentially boolean expressions combining format/color specs with standard logical operators:

Logical Operators | Description
------------------|------------
`\|[\|]` | Logical OR (may be abbreviated as `\|`)
`&[&]` | Logical AND (may be abbreviated as `&`)
`!` | Logical negation
`(` ... `)` | Groups sub-patterns

**Tip:** `|` and `&` are equivalent to `||` and `&&`, respectively, but to avoid confusion with the special `f|` and `f&` primitives, some users may prefer the long forms. 

The color specs used in selector patterns are identical to the ones used for highlighting. As with format highlighting specs, format selector specs begin with special operators that modify their behavior:

Modifier | Description
---------|------------
`\|` | Matches regions containing _any_ of the subsequent format attributes
`&` | Matches regions containing _all_ of the subsequent format attributes
`=` ( **_default_** ) | Matches regions containing _exactly_ (all and only) the subsequent format attributes

**Note:** `f&...`, `f|...` and `f=...` are atomic constructs: i.e., space is not permitted after the `f`, or between operators and attribute flags. 

**Tip:** Because it's the default, you can drop the `=` from `f=<attrs>` in a selector expression. 

### Selector Pattern Examples
The following examples illustrate the use of highlighting specs with attached selector patterns:

Spec/Pattern | Action | Applies To
-------------|--------|-----------
<nobr>`fu/cr`</nobr> | Add underline | red text
<nobr>`cb/cr\|\|f&bi`</nobr> | Make text blue | text that is red _**or**_ has both bold and italic attributes (and possibly others)
<nobr>`fu,kg/cr&&f|bi`</nobr> | Add underline and make background green | text that is red _**and**_ has _either_ bold _or_ italic attributes (and possibly both)
<nobr>`fi/cr\|\|f=bu`</nobr> | Add italic | text that is red _**or**_ bold-underline
<nobr>`cb kr f-i / fbi & !(cr & k-)`</nobr> | Make text blue on a red background and remove italic attribute | text that is bold-italic and _not_ red on a colorless background

**Tip:** Whitespace is mostly ignored by Txtfmt, and can be used to make the expressions easier to read.<br>
**Note:** Highlighting types omitted from the selector expression are unconstrained: e.g., a selector expression of `fb` matches bold text of _any_ foreground/background color (including none).

## "Smart" Delete Operator Override
Although Txtfmt makes the highlighting tokens invisible, to Vim they are simply characters in your buffer like any other: thus, Vim's builtin delete operators make no distinction between text and tokens. To see how this can be problematic, consider the following scenario... While sitting on the first line of a multiline block of highlighted text, you execute `dd` to delete a line of text. Although you intended only to delete only text, the `dd` also removed the hidden tokens, with the result that you have inadvertently removed the highlighting of the entire block! To mitigate this problem, Txtfmt overrides the Normal and Visual mode delete operator (`d`) with its own "smart" version, which will add, remove and replace tokens as needed to prevent change to the highlighting that follows the deleted text.

**Note:** If you prefer **_not_** to remap the builtin delete operator, simply specify your own 1st level mapping to inhibit the default: e.g.,

`nmap <buffer> <LocalLeader>d <Plug>TxtfmtOperatorDelete`<br>
`xmap <buffer> <LocalLeader>d <Plug>TxtfmtVmapDelete`

**Note:** The example above assumes you'll be defining the maps from an autocmd that runs only for Txfmt buffers. If you'd rather put the override in your vimrc, simply omit the `<buffer>`, but be aware that this will define the map globally (not only in Txtfmt buffers).

`:help txtfmt-map-config`

## "Smart" Shift/Indent Overrides
Because highlighting tokens are invisible, yet appear as normal text to Vim, their presence in leading indent would lead to unexpected shift/indent behavior if Txtfmt did not override the builtin shift/indent commands `<<`, `>>`, `CTRL-T` and `CTRL-D`. The Txtfmt overrides understand the special role of tokens in a Txtfmt buffer (as well as the implications of the various `'leadingindent'` option settings), and will go to great lengths to ensure that various shift/indent commands "do the right thing".

# Leading Indent (highlighting vs page/paragraph fill)
If you've ever used a word-processor, you may have noticed that when a block of text is indented, attributes such as underline and background color are not visible in the indent area. When leading spaces or tabs are used to indent blocks of text in Vim, it generally looks best not to highlight them. But what is "leading indent", exactly? All leading whitespace? Spaces only? Tabs only? Some combination of tabs and spaces, as determined by options such as 'tabstop' and 'shiftwidth'? The value of Txtfmt's `'leadingindent'` option provides the answer to this question, as shown in the table below:

Setting | Behavior
------- | --------
none | No special treatment of leading whitespace, which will be highlighted like any other text. Note that this was the default behavior up until Txtfmt version 3.1, which introduced this option.
space | Longest sequence of leading spaces (ASCII 32).
tab | Longest sequence of leading tabs (ASCII 9).
white (**_default_**) | Longest sequence of leading whitespace (spaces and/or tabs).
smart | Algorithm uses 'tabstop' and 'shiftwidth' to determine whether a sequence of leading tabs and/or spaces is considered leading indent.<br> **Heuristic:** Any sequence of leading whitespace that could be generated by a sequence of `CTRL-T`'s in insert mode on an empty line. Alternatively, any sequence of leading whitespace that could be generated by a sequence of right-shifts (`>>`) in normal mode.<br> **Caveat:** If you use the "smart" setting, you should consistently use mechanisms such as `CTRL-T` and `>>` to insert leading indent: in particular, inserting literal TABs for leading indent may yield unexpected results.

**Note:** The logic that recognizes leading indent considers only _actual_ whitespace, ignoring any embedded tokens.

# Manual Maps

Manual maps are used to insert specific highlighting tokens at specific locations in the buffer. Each token determines either the text color, background color, or format attributes in effect up until the subsequent token of the same type (possibly the end token `'-'`). With auto maps, you simply specify the desired highlighting changes, and Txtfmt _automagically_ inserts/removes the requisite tokens at both the beginning and end of the selection. This task is actually more complicated than it sounds. Suppose you've selected some text and use an auto map with highlighting spec `fb,cr,kg` (add bold, text red, background green). You might assume that Txtfmt would insert 6 tokens as follows:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
_`<kr><cb><fb> <selected text...> <k-><c-><f->`_

This is certainly one possibility, but the situation is actually more complicated... For one thing, there may be tokens _within_ the selected text that need to be removed. Leaving a `<cg>` token, for instance, would be harmful, as it would prematurely terminate the blue text region. A `<cb>` token within the region would be harmless but redundant with the one at the beginning of the region, so it should really be removed. But what about format attribute tokens within the region? They cannot simply be removed, since the highlighting spec requested only the _**addition**_ of bold, not the removal of anything else! An `<fi>` token within the region, therefore, would need to be _**changed**_ to an `<fbi>` token to satisfy the request without disturbing existing highlighting. Similarly, if the text just _past_ the selected region were originally underline-italic, the auto map would need to end the region with an `<fui>` token ( _**not**_ `<f->` ), to avoid changing the highlighting of unselected text.

Auto maps provide an abstraction that lets you think in terms of a desired highlighting change, shielding you from the often tedious details involved in effecting the change. So why would you ever want to use manual maps instead of auto maps? Most users won't, but there may be scenarios in which the explicit control afforded by manual maps is needed. Accordingly, here's a quick walkthrough...

Suppose you want to enter some green text... Execute `\i`, and enter `cg` at the prompt (**_mnemonic:_** color green): 

The text you type now is green. 

While typing green text, you wish to emphasize a phrase by making it bold-italic. Still in Insert mode, you execute _insert-token_ map `CTRL-\CTRL-\` and enter `fbi` (or `fib`) at the prompt (**_mmemonic:_** format bold italic).

The text you type now is green bold-italic.

Now you wish to enter some text with a blue background. Still in insert mode, you execute _insert-token_ map `CTRL-\CTRL-\` and enter `kb` at the prompt (**_mnemonic:_** bac _k_ ground blue). 

The text you type now is green bold-italic on a blue background. 

At this point, any of the open highlighting regions can be terminated with the corresponding terminator token:
    `f-' (no format attributes)
    `c-' (no color)
    `k-' (no background color)

Alternatively, you can enter a non-terminator highlighting spec to change subsequent highlighting without explicitly terminating the preceding region. Inserting a "cr" token, for instance, would switch from green text color to red.

To finish the example, let's terminate the 3 active regions by executing the _insert-token_ map `CTRL-\ CTRL-\` one last time and entering `c-,f-,k-` (or `c- f- k-`) at the prompt (**_mnemonic:_** no color, no format, no bac _k_ ground color).

The text you type now is plain, unhighlighted text.

**Note:** In the preceding example, you used _insert token_ maps `\i` and `CTRL-\CTRL-\` to insert highlighting tokens. There are actually 15 such maps in all, but they're easy for a Vim user to remember by analogy with Vim's builtin commands for entering insert mode: e.g., `\i` inserts token before cursor; `\a` inserts token after cursor; `\I` inserts token at beginning of line; etc... Moreover, each of those commands has a variant beginning with `\v`, which returns to Normal mode after inserting the token. Finally, `CTRL-\CTRL-\` allows you to insert a token without leaving Insert mode.

`:help txtfmt-ins-tok-maps`


## Cursor Positioning with Dot (`.`)
As with auto map highlighting specs, multiple format/color specs can be concatenated in a comma or space-separated list. Moreover, you can replace one of the commas with a dot (`.`) to specify where the cursor should be positioned relative to the inserted tokens. These features provide a convenient way to enter both the start and end tokens of a region before beginning to type the highlighted text. If, for example, you wanted to enter red bold text on a yellow background, you could enter `cr fb ky . c- f- k-` to insert all the tokens at once, leaving the cursor between the start and end tokens.

## Jump to Token Maps
As long as you're using auto maps, you should never need to think about Txtfmt tokens in the buffer: the tokens are invisible as long as the Txtfmt syntax is active, and all token insertion/removal is handled automatically by the plugin. If you're using manual maps, however, you _**will**_ need to work with the tokens: specifically, you will need to insert, replace and delete them. Txtfmt provides maps to facilitate insertion (`\i`, `\I`, `\a`, `\A`, `\o`, `\O`) and replacement (`\s`). Vim's builtin delete operators (e.g., `x`, `X`, etc...) may be used to delete tokens, but since the tokens themselves are invisible, Txtfmt provides a handful of maps to help _**find**_ the tokens to be deleted/changed. Collectively, these maps are referred to as "Jump to Token" maps, and they can all be used in both Normal and Operator-pending mode. There are quite a few variants, allowing you to specify direction of jump, type of token sought (text color, background color, format attributes), and whether to land _on_ or _next to_ the target token.

### Jump to Token Map Examples

Map | Description
--- | -----------
]f  | forward to next format token
[c  | backward to previous text color token
]a  | forward to next token of _any_ type
]tf | forward to char _**before**_ next format token
[ea | backward to previous _end_ token of any type (i.e., `<f->`, `<c->` or `<k->`)
]bf | forward to next format _begin_ token (i.e., token that specifies format attributes, _**not**_ `<f->` )

`:help txtfmt-jump-to-tok`

## Token Inspection
If you ever need to know a token's type, simply position your cursor on it (e.g., using one of the "jump to token" maps described above) and execute...
    `\ga`
Like builtin `ga` and `g8`, this command displays information in the statusline about the character under the cursor, but instead of character codes, it displays a short string indicating the token type.

### \ga Examples

Statusline | Description
---------- | -----------
fibu | Format italic-bold-underline
c1 | Foreground color #1
k3 | Background color #3
f- | Format end token

`:help txtfmt-get-tok-info`

# Configuration

## Options
Although the default settings of most Txtfmt options will be fine for the average user, there are 3 distinct mechanisms provided for customization:

Mechanism | Description | Motivation | Example
--------- | ----------- | ---------- | -------
Txtfmt "modelines" | analogous to Vim modelines | associate options with file | <nobr>`    txtfmt: li=smart nested`</nobr><br><nobr>`    vim: ft=txtfmt`
Buf-local variables | setting applies to current buffer only | set from an autocommand for specific file type | <nobr>`let b:txtfmtLeadingindent = 'white'`</nobr>
Global variables | setting applies to all Txtfmt buffers | set from vimrc | <nobr>`let g:txtfmtTokrange = '180X'`</nobr>

`:help txtfmt-options`

For a list of all supported options...

`:help txtfmt-opt-list`


## Color Configuration
Txtfmt's default configuration provides 8 foreground colors and 3 background colors:

Color | Color # | Abbrev | Terminal | GUI | Available in Background?
----- | ------- | ---- | -------- | --- | -----------
Black | 1 | k | Black | #000000 | No
Blue | 2 | b | DarkBlue | #0000FF | Yes
Green | 3 | g | DarkGreen | #00FF00 | Yes
Turquoise | 4 | t | DarkCyan | #00FFFF | No
Red | 5 | r | DarkRed | #FF0000 | Yes
Violet | 6 | v | DarkMagenta | #FF00FF | No
Yellow | 7 | y | DarkYellow | #FFFF00 | No
White | 8 | w | White | #FFFFFF | No


But everything about these defaults is configurable. For instance, if you have a relatively modern computer, you might wish to activate all 8 colors for background highlighting. You could do so with the following override of `'bgcolormask'` in your vimrc:

`" Unmask background colors disabled by default<br>
let g:txtfmtBgcolormask = "11111111"`

You could accomplish the same thing _for a single file_ by adding the following Txtfmt "modeline" to the beginning or end of the file:

&nbsp;&nbsp;&nbsp;&nbsp;`	txtfmt:bcm=11111111`

**Note:** Foreground and background colors can be enabled/disabled independently.

`:help txtfmt-'fgcolormask'`<br>
`:help txtfmt-'bgcolormask'`

The color definitions themselves are also customizable. If black is the default text color in your preferred colorscheme, you may not want to waste 1 of 8 color slots on it. You could change color #1 from black to "slategray" by adding the following line to your vimrc:

`let g:txtfmtColor{1} = '^G$,c:Gray,g:#708090'`

Similarly, if the default for color #6 (violet) seems a bit too bright in the GUI, the following line in your vimrc would tone it down a bit and change its abbreviation from "v" (violet) to "p" (purple):

`let g:txtfmtColor{6} = '^p$,c:DarkMagenta,g:#800080'`

**Note:** You don't actually specify a _name_ for a color, but a regex that matches any string you might use to specify it in a highlighting spec. The `\%[]` construct is used in the default regexes to permit names to be abbreviated or typed out in part or full, but most users will probably want to use single-letter abbreviations.


**Note:** The `c:` and `g:` in the color definition strings refer to "color terminal" and "gui", respectively. The full color string syntax allows you to define different colors for different terminal/GUI types.

`:help txtfmt-color-config`

## Token Range
Txtfmt requires a block of contiguous characters for use as highlighting tokens. Both the location and size of the block are determined by the value of the `'tokrange'` option, which can be set via Txtfmt option variable or Txtfmt modeline. The `'tokrange'` option is a string consisting of a decimal or hexadecimal number with a trailing suffix: e.g., `"0xE000X"` or `"180S"`. The numeric value is the character code of the first character in the block, and the suffix determines the available highlighting, as shown in the following table:

'tokrange' Suffix | Supported Highlighting | # Characters in Range
----------------- | ----------- | -------------------
`S` (short) | underline,bold,italic + fg colors | 17
`X` (extended)<br> **_(default)_** | underline,bold,italic + fg colors + bg colors | 26
`L` (long) | underline,bold,italic,standout,reverse (and optionally undercurl) + fg colors | 41 (73 if "undercurl" enabled)

`:help txtfmt-'tokrange'`
`:help txtfmt-'undercurl'`

The default 'tokrange' should be fine for most users, but you can easily change it by adding something like this to your vimrc...

`" Place Txtfmt tokens at 0xE100 (256 bytes past start of Unicode 'Private Use Area')<br>
" and enable 'Long' formats
let g:txtfmtTokrange = '0xE100L'`

Alternatively, you could customize `'tokrange'` for a single file using a Txtfmt modeline: e.g.,

&nbsp;&nbsp;&nbsp;&nbsp;`txtfmt:tokrange=0xE100X`

**Caveat:** Changes to g:txtfmtTokrange impact highlighting in **_all_** existing Txtfmt files without a modeline; thus, any change to g:txtfmtTokrange globally should be accompanied by a migration of existing files. Txtfmt provides the `:MoveStartTok` command to help with this.

`:help txtfmt-'MoveStartTok'`

### Displaying Token Range
The `:ShowTokenMap` command provides a tabular illustration of the 'tokrange' in effect for the current buffer. Each row of the table lists a token's character code along with its meaning (e.g., "underline-bold" or "Color3"). Additional information is displayed for a color token: its name pattern, RGB value (either as hex value or terminal-specific color name) and enabled status.

`:help txtfmt-:ShowTokenMap`

# Installation
Txtfmt can be installed using any of the standard Vim plugin mechanisms: e.g., Pathogen, Vundle, etc. If you're not using a plugin manager, simply drop the uncompressed Txtfmt distribution somewhere in your 'runtimepath', then run `helptags ALL` to prepare the Txtfmt help.

`:help 'runtimepath'`
`:help txtfmt-installation`

