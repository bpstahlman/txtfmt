txtfmt
======

Txtfmt (The Vim Highlighter) : "Rich text" highlighting in Vim! (text color, highlighting, underline, bold, italic, etc...)

The next few sections give usage examples of visual/operator auto maps and manual maps... 
<br>**Note:** If you wish to try the usage examples now, you will need to install and load the plugin. If you're in a hurry, and are familiar with plugin installation, just drop the bundle of files in the proper location and do...

```vim
set ft=txtfmt
```
The "Installation Details" section (further down) discusses plugin load in greater detail... 

# Introduction
Txtfmt brings rich text formatting to plain text files. The commands used to accomplish highlighting fall into 2 categories:

* Auto maps: Change the highlighting of a range of text. There are currently 2 types of auto map:
    * Visual: Operate on the visually selected text 
    * Operator: Operate on the text moved over (or included in a "text object") 
* Manual maps: Insert one ore more _tokens_, which affect all subsequent text up to the next token.

**Note:** Txtfmt's highlighting relies upon invisible characters (tokens) in the text, but you needn't know or care about this if you're using auto maps, which completely automate token insertion/removal. Manual maps (the _only_ type available until Txtfmt 3.0) do not shield the user nearly so well from this implementation detail, and hence, are provided mostly for backwards compatibility.
 
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
<br>**Note:** `fui` is short for `f+ui`, which _adds_ the underline and italic attributes to the existing highlighting. Thus, the word you highlighted in the preceding example has retained the bold attribute, and is now red, bold-underline-italic. Use `f=` instead of `f+` when you wish to _replace_ or _overwrite_ any existing highlighting.

## Highlighting Spec Basics
Strings such as `f=bi,cb` and `fui` are known as _highlighting specs_. Each spec can be a comma or space-separated list of format/color components, so it's possible to alter foreground color, background color and format attributes with a single command. The first letter of each component determines the type of highlighting affected:

Letter | Type
-------|-----
f | format attributes
c | foreground color
k | background color

<br>**Orthogonality Note:** Because the 3 types of highlighting are completely orthogonal, there is no need to specify components whose highlighting you do not wish to change: e.g., `fb` adds bold attribute without affecting text color or background color in any way.

### Color Specs
For fg/bg colors, a color name or hyphen (`-`) follows the `c` or `k`. The color names are configurable, but the defaults allow common colors to be abbreviated to a single letter: e.g., `r`=red, `b`=blue, `g`=green, etc... A `-` specifies "no color": i.e., it removes color from the region. Both the color RGB values and the color names are completely configurable.
<br> `:help txtfmt-color-config`

### Format Attribute Specs
The `f` of a format specification is followed by a string of operators and single character format attribute flags:
`([-+=][ubisrc]+)+` (TODO - EBNR?)

Operator | Description
---------|----------
`+` (default) | Add to existing format attributes
`-` | Remove from existing format attributes
`=` | Replace/overwrite existing format attributes

Each operator applies to all subsequent format attribute flags until the next operator.

Attributes | Description | Availability
-----------|-------------|-------------
u | underline
b | bold
i | italic
s | standout | "long" 'tokrange' only
r | reverse | "long" 'tokrange' only
c | undercurl | "long" 'tokrange' only
<br>
**Note:** The default configuration provides only underline, bold and italic. If you wish to enable standout, reverse and undercurl...
:help txtfmt-'tokrange'

### Highlighting Spec Examples
Spec | Result
-----|-------
`fbi` | Add bold-italic
`f+bi` | _same as previous_
`f+bi-u` | Add bold-italic and remove underline
`f=bi` | Replace any existing format attributes with bold-italic
`f-,c-` | Remove all format attributes and any foreground color
`fu-b,cb,kr` | Add underline and remove bold, make text color blue and background color red
`fu cb kr` | _same as previous_


## Selective (Pattern-Based) Highlighting
Up until now, we've been applying highlighting to _all_ of the text in a range. It is also possible to target specific _sub-regions_ within the visually-selected or operated-on text. To apply highlighting selectively, append a `/` to the highlighting spec, followed by a "selector pattern expression": e.g.,

    highlighting-spec / selector-pattern 

Selector patterns are essentially boolean expressions combining format/color specs with standard logical operators:

Logical Operators | Description
------------------|------------
`|[|]` | Logical OR (may be abbreviated as `|`
`&[&]` | Logical AND (may be abbreviated as `&`
`!` | Logical negation
`(` ... `)` | Groups sub-patterns

**Tip:** `|` and `&` are equivalent to `||` and `&&`, respectively, but to avoid confusion with the special `f|` and `f&` primitives, some users may prefer the long forms. 

The color specs used in selector patterns are identical to the ones used for highlighting. As with format highlighting specs, format pattern specs begin with special operators that modify their behavior:

Modifier | Description
---------|------------
`|` | Matches regions containing _any_ of the subsequent format attributes
`&` | Matches regions containing _all_ of the subsequent format attributes
`=` (default) | Matches regions containing _exactly_ (all and only) the subsequent format attributes

**Note:** `f&...`, `f|...` and `f=...` are atomic constructs: space is not permitted between the `f` and the operator, or between the operator and the list of attributes. 

**Tip:** You can drop the `=` from `f=<attrs>` in a selector expression. 

The following examples illustrate the use of highlighting specs with attached selector patterns:

Spec/Pattern | Action | Applies To
-------------|--------|-----------
<nobr>`fu/cr`</nobr> | Add underline | red text
<nobr>`cb/cr||f&bi`</nobr> | Make text blue | text that is red _**or**_ has both bold and italic attributes (and possibly others)
<nobr>`fu,kg/cr&&f|bi`</nobr> | Add underline and make background green | text that is red _**and**_ has _either_ bold or italic attributes (and possibly both)
<nobr>`fi/cr||f=bu`</nobr> | Add italic | text that is red _**or**_ bold-underline
<nobr>`cb kr f-i / fbi & !(cr & k-)`</nobr> | Make text blue on a red background and remove italic attribute | text that is bold-italic and _not_ red on a colorless background

**Tip:** Whitespace is mostly ignored by Txtfmt, and can be used to make the expressions (both highlighting and selector) easier to read.<br>
**Note:** Highlighting types missing from the selector expression are unconstrained: e.g., a selector expression of `fb` matches bold text of _any_ foreground/background color (including none).

# Manual Maps

Manual maps are used to insert specific highlighting tokens at specific locations in the buffer. Each token determines either the text color, background color, or format attributes in effect up until the subsequent token of the same type (possibly the end token `-`). With auto maps, you simply specify the desired highlighting changes, and Txtfmt automagically inserts/removes the requisite tokens at both the beginning and end of the selection. This task is actually more complicated than it sounds. Suppose you've selected some text and use an auto map with highlighting spec `fb,cr,kg` (add bold, text red, background green). You might assume that Txtfmt would insert 6 tokens as follows:
```
<kr><cb><fb> <selected text...> <k-><c-><f->
```
This is certainly one possibility, but the situation is actually more complicated... For one thing, there may be tokens _within_ the selected text that need to be removed. Leaving a `<cg>` token, for instance, would be harmful, as it would prematurely terminate the blue text region. A `<cb>` token within the region would be harmless but redundant with the one at the beginning of the region, so it should really be removed. But what about format attribute tokens within the region? They cannot simply be removed, since the highlighting spec requested only the _**addition**_ of bold, not the removal of anything else! An `<fi>` token within the region, therefore, would need to be _**changed**_ to an `<fbi>` token to satisfy the request without disturbing existing highlighting. Similarly, if the text just _past_ the selected region were originally underline-italic, the auto map would need to end the region with an `<fui>` token (_**not**_ `<f->`), to avoid changing the formatting of unselected text.

Auto maps provide an abstraction that allows you to think in terms of a desired highlighting change, shielding you from the often tedious details involved in effecting the change. So why would you ever want to use manual maps instead of auto maps? Most users won't, but there may be scenarios in which the explicit control afforded by manual maps is needed. Accordingly, here's a quick walkthrough...

You want to enter some green text, so you execute one of Txtfmt's Normal mode _insert-token_ mappings (e.g., `\i`, `\I`, `\a`, `\A`, `\o`), and enter `cg` at at the prompt (**_mnemonic:_** color green): 

The text you type now is green. 

While typing green text, you wish to emphasize a phrase by making it bold-italic. Still in insert mode, you execute _insert-token_ map `CTRL-\CTRL-\` and enter `fbi` (or `fib`) at the prompt (**_mmemonic:_** format bold italic).

The text you type now is green bold-italic.

Now you wish to enter some text with a blue background. Still in insert mode, you execute _insert-token_ map `CTRL-\CTRL-\` and enter `kb` at the prompt (**_mnemonic:_** bac _k_ ground blue). 

The text you type now is green bold-italic on a blue background. 

At some point, you wish to return to plain, unhighlighted text. You can terminate the 3 active regions by executing the _insert-token_ map `CTRL-\ CTRL-\` one last time and entering `c-,f-,k-` (or `c- f- k-`) at the prompt (**_mnemonic:_** no color, no format, no bac _k_ ground color).

The text you type now is plain, unhighlighted text.

## Jump to Token Maps
As long as you're using auto maps, you should never need to think about Txtfmt tokens in the buffer: the tokens are invisible as long as the Txtfmt syntax is active, and all token insertion/removal is handled automatically by the plugin. If you're using manual maps, however, you _**will**_ need to work with the tokens: specifically, you will need to insert, replace and delete them. Txtfmt provides maps to facilitate insertion (`\i`, `\I`, `\a`, `\A`, `\o`, `\O`) and replacement (`\s`). Vim's builtin delete operators (e.g., `x`, `X`, etc...) may be used to delete tokens, but since the tokens themselves are invisible, Txtfmt provides a handful of maps to help _**find**_ the tokens to be deleted/changed. Collectively, these maps are referred to as "Jump to Token" maps, and they can all be used in Operator-pending mode. There are quite a few variants, allowing you to specify direction of jump, type of token sought (text color, background color, format attributes), whether to land _on_ or _next to_ the target token, etc. Here are some examples...

Map | Description
---   -----------
]f  | forward to next format token
[c  | backward to previous text color token
]a  | forward to next token of _any_ type
]tf | forward to char _**before**_ next format token
[ec | backward to previous _end_ token of any type (i.e., `<f->`, `<c->` or `<k->`)
]bf | forward to next format _begin_ token (i.e., token that specifies format attributes, _**not**_ `<f->` )

Further details may be found in the help file:
`:help txtfmt-jump-to-tok`

TODO: Add something on \ga

## Cursor Positioning Note
As with auto map highlighting specs, multiple format/color specs can be concatenated in a comma or space-separated list. Moreover, you can replace one of the commas with a dot (`.`) to specify where the cursor should be positioned relative to the inserted tokens. These features provide a convenient way to enter both the start and end tokens of a region before beginning to type the highlighted text. If, for example, you wanted to enter red bold text on a yellow background, you could enter `cr fb ky . c- f- k-` to insert all the tokens at once, leaving the cursor between the start and end tokens.

# Leading Indent (highlighting vs page/paragraph fill)
If you've ever used a word-processor, you may have noticed that when a block of text is indented, attributes such as underline and background color are not visible in the indent area. When leading spaces or tabs are used to indent blocks of text in Vim, it generally looks best not to highlight them. But what is "leading indent", exactly? All leading whitespace? Spaces only? Tabs only? Some combination of tabs and spaces, as determined by options such as 'tabstop' and 'shiftwidth'? The value of Txtfmt's 'leadingindent' option provides the answer to this question, as shown in the table below:
Setting | Behavior
------- | --------
none | No special treatment of leading whitespace, which will be highlighted like any other text. Note that this was the default behavior up until Txtfmt version 3.1, which introduced this option.
space | Longest sequence of leading spaces (ASCII 32).
tab | Longest sequence of leading tabs (ASCII 9).
white (_default_) | Longest sequence of leading whitespace (spaces and/or tabs).
smart | Algorithm uses 'tabstop' and 'shiftwidth' to determine whether a sequence of leading tabs and/or spaces is considered leading indent.<br> **Heuristic:** Any sequence of leading whitespace that could be generated by a sequence of `CTRL-T`'s in insert mode on an empty line. Alternatively, any sequence of leading whitespace that could be generated by a sequence of right-shifts (`>>`) in normal mode.<br> **Caveat:** If you use the "smart" setting, you should consistently use mechanisms such as `CTRL-T` and `>>` to insert leading indent: in particular, inserting literal TABs for leading indent may yield unexpected results.

**Note:** The logic that recognizes leading indent considers only _actual_ whitespace, ignoring any embedded tokens.

## Delete Operator Override
Although Txtfmt tokens are invisible, to Vim they are characters in your buffer like any other: thus, deleting text can delete tokens, which in turn, can affect the highlighting of text _beyond_ the deletion. To see how this could be problematic, consider the following scenario... You're sitting on the first line of a multiline block of highlighted text (whose highlighting tokens are hidden at the start of the first line). You use `dd` to delete the first line. Although you intended only to delete a line of text, you have just removed the highlighting of the entire block! Of course, the problem is that the builtin `dd` operator deletes the highlighting tokens along with the rest of the line text. To mitigate these problem, Txtfmt provides its own "smart" delete operators, which should be used instead of the builtin delete on text that may contain highlighting tokens. Note that it is not sufficient for smart delete operators simply to avoid deleting highlighting tokens: they must be

hitting gg0. At this point you should be sitting on an underline-bold-italic format token. If you wish to verify this, hit the following map in normal mode:
    \ga
    :help txtfmt-get-tok-info

Because you wish to delete the first two words, enter the following in normal mode:
    d2w
Notice how the underline-bold-italic highlighting has been lost! The problem is that the command that deletes the first two words has also deleted the Txtfmt format token. In other words, the deletion had a visible effect on the text beyond the deletion. This is probably not what you wanted. To solve this problem, Txtfmt also provides a "smart delete" command, which is aware of Txtfmt highlighting. Like the auto maps highlighting commands, it can be used in both visual and operator modes. Its default mapping is \d. To use it with the previous example, you could simply have hit \d2w instead of d2w. Try it now (after hitting 'u' to undo the problematic delete, and '0' to be sure you're at the start of the line). Alternatively, you could have highlighted the first two words, then hit \d. The difference between Vim's and Txtfmt's delete commands is that, whereas Vim makes no distinction between text and Txtfmt tokens, Txtfmt's delete assumes your objective is to delete text only, and will do whatever is necessary (possibly inserting new tokens), to preserve the highlighting of text beyond the deletion.


## Shift/Indent Overrides
Although Txtfmt "tokens" are generally invisible to the user, Vim itself treats them as non-whitespace characters. To prevent problems when these tokens appear in leading indent, Txtfmt provides special overrides of builtin operators such as `<<`, `>>`, `CTRL-T` and `CTRL-D`. These overrides understand the special role of tokens in a Txtfmt buffer (as well as the implications of the various 'leadingindent' option settings), and will go to great lengths to ensure that "the right thing" happens when you perform a shift or indent.

# Txtfmt Options
Although the default settings of most Txtfmt options will be fine for the average user, there are 3 distinct mechanisms provided for customization:

Mechanism | Description | Motivation | Example
--------- | ----------- | ---------- | -------
Txtfmt "modelines" | analogous to Vim modelines | associate options with file | <nobr>`    txtfmt: li=smart nested`</nobr><br><nobr>`    vim: ft=txtfmt`
Buf-local variables | setting applies to current buffer only | set from an autocommand for specific file type | <nobr>`let b:txtfmtLeadingindent = 'white'`</nobr>
Global variables | setting applies to all Txtfmt buffers | set from vimrc | <nobr>`let g:txtfmtTokrange = '180X'`</nobr>


# Color Configuration

# Installation
Txtfmt can be installed using any of the standard Vim plugin mechanisms: e.g., Pathogen, Vundle, etc. If you're not using a plugin manager, simply drop the uncompressed Txtfmt distribution somewhere in your 'runtimepath', then run `helptags ALL` to prepare the Txtfmt help.
```
:help 'runtimepath'
:help txtfmt-installation
```
