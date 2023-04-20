Txtfmt (The Vim Highlighter)
======

### "Rich text" highlighting in Vim! (text color, highlighting, bold, underline, italic, etc...)

**Note:** If you wish to try the examples below, you will first need to install the plugin. If you're unfamiliar with plugin installation, read [Installation](./README.md#Installation). The section on [Plugin Loading](./README.md#plugin-loading) may be useful if you'd like to ensure that the Txtfmt plugin is loaded automatically every time you open certain files or types of file. If you're in a hurry to try the examples, you can execute the following at the Vim command line to enable highlighting in the current buffer.

`:set ft=txtfmt`

Alternatively, you can have Txtfmt create a "test page" for you:

`:MakeTestPage`

The test page offers a quick way to verify that the plugin is installed properly, as well as a convenient mechanism for visualizing the effects of configuration changes. When executed with no arguments, `:MakeTestPage` uses default settings, taking into account any Txtfmt option variables you've set in your vimrc. But if you supply arguments, `:MakeTestPage` will process them as though they were part of a "Txtfmt modeline" within the test page itself.

`:help txtfmt-:MakeTestPage`

# Introduction

Txtfmt brings rich text highlighting to plain text files. The commands used to accomplish highlighting fall into 3 basic categories:

* **Auto maps:** Change the highlighting of a range of text. There are currently 2 types of auto map:
  * Visual: Operate on the visually selected text 
  * Operator: Operate on the text moved over (or included in a "text object") 
* **Shortcut maps:** Auto map "presets" that associate a specific combination of formats and colors with a keystroke mapping.
* **Manual maps:** Insert one or more _highlighting tokens_, which affect all subsequent text up to the next token.

**Note:** Txtfmt's highlighting relies upon invisible characters ("tokens") in the text, but you needn't know or care about this if you're using auto maps, which completely automate token insertion/removal. Manual maps (the _only_ type available until Txtfmt 3.0) do not shield the user nearly so well from this implementation detail, and hence, are provided mostly for backwards compatibility.

**Note:** All examples on this page assume that your `<LocalLeader>` is at the default value (i.e., backslash). If you've set `<LocalLeader>` to something other than the default, replace the backslash in the examples with the appropriate key sequence.

`:help <LocalLeader>`

# Auto Maps

Before diving into the details of auto maps, I'll present a few examples that show how easy they are to use.

## Visual Auto Maps

**Objective:** Make selected text blue, bold-italic. 

1. Select the text to be highlighted using either mouse or visual mode (e.g., Normal mode `v` or `V` command).
2. Type `\h`
3. At the prompt, type `f=bi,cb`
4. Hit Enter to apply highlighting
   <br>**Note:** You could also have typed `fbi,cb` (without the `=`) or even `f+bi,cb`, either of which would have _added_ bold-italic on top of any existing format attributes.

## Operator Auto Maps

**Objective (part 1):** Make the word under the cursor bold red.

1. In Normal mode, position cursor on word to be highlighted
2. Type `\h` (enters "operator-pending" mode)
3. Type `iw` (specifies "inner word" text object)
4. At the prompt, type `cr,f=b`
5. Hit Enter to apply highlighting

**Objective (part 2):** Italicize and underline the current and subsequent 2 lines, highlighting the background green (without losing the highlighting applied to the word in part 1).

1. In Normal mode, with cursor still in the line from the preceding example...<br>
   Type `\h` (enters "operator-pending" mode)
2. Hit `2j` (includes current and next 2 lines in range)
3. At the prompt, type `fui,kg`
4. Hit Enter to apply highlighting
   <br>**Note:** `fui` is short for `f+ui`, which _adds_ the underline and italic attributes to the existing highlighting. Thus, the word you highlighted in the preceding example has retained the bold attribute, and is now bold-underline-italic. If you'd used `f=` instead of `f+`, the bold attribute would have been lost, as `f=` _replaces_ or _overwrites_ existing highlighting.

## Highlighting Spec Basics

Strings such as `f=bi,cb` and `fui` are known as _highlighting specs_. Each spec can be a comma or space-separated list of format/color components, so it's possible to alter foreground color, background color and format attributes with a single command. The first letter of each component determines the type of highlighting affected:

| Letter | Type                            |
| ------ | ------------------------------- |
| f      | format attributes               |
| c      | foreground (text) color         |
| k      | background (highlighting) color |

**Note:** Because the 3 types of highlighting are completely orthogonal, there is no need to specify components whose highlighting you do not wish to change: e.g., `fb` adds bold attribute without affecting foreground or background color in any way.

### Color Specs

For foreground/background colors, either a color name or hyphen (`'-'`) must follow the `c` or `k`. The color names are configurable, with the defaults allowing common colors to be abbreviated to a single letter: e.g., `r`=red, `b`=blue, `g`=green, etc... A `'-'` specifies "no color": i.e., it removes color from the region. Both the color RGB values and the color names are completely configurable. See [Color Configuration](./README.md#color-configuration) for details...

### Format Attribute Specs

The `f` of a format specification is followed by a string of operators and single character format attribute flags:

#### Format Attribute Operators

| Operator            | Description                                  |
| ------------------- | -------------------------------------------- |
| `+` (**_default_**) | Add to existing format attributes            |
| `-`                 | Remove from existing format attributes       |
| `=`                 | Replace/overwrite existing format attributes |

Each operator applies to all subsequent format attribute flags until the next operator.

#### Format Attribute Flags

| Attributes | Description | Availability                              |
| ---------- | ----------- | ----------------------------------------- |
| u          | underline   |                                           |
| b          | bold        |                                           |
| i          | italic      |                                           |
| s          | standout    | "long" 'tokrange' only                    |
| r          | reverse     | "long" 'tokrange' only                    |
| c          | undercurl   | "long" 'tokrange' only (Vim version >= 7) |

**Note:** The default configuration provides only underline, bold and italic. If you wish to enable standout, reverse and undercurl, see section [Token Range](./README.md#token-range) below.

### Highlighting Spec Examples

| Spec         | Result                                                                       |
| ------------ | ---------------------------------------------------------------------------- |
| `fbi`        | Add bold-italic                                                              |
| `f+bi`       | _same as previous_                                                           |
| `f+bi-u`     | Add bold-italic and remove underline                                         |
| `f-u+bi`     | _same as previous_                                                           |
| `f=bi`       | Replace any existing format attributes with bold-italic                      |
| `f-,c-`      | Remove all format attributes and any foreground color                        |
| `fu-b,cb,kr` | Add underline and remove bold, make text color blue and background color red |
| `fu-b cb kr` | _same as previous_                                                           |

<div id=selective-highlighting />

## Selective (Pattern-Based) Highlighting

Up to this point, we've been applying highlighting to _all_ of the text in a range. It is also possible to target specific _sub-regions_ within the visually-selected or operated-on text. To apply highlighting selectively, append a `/` to the highlighting spec, followed by a "selector pattern expression": e.g.,

&nbsp;&nbsp;&nbsp;&nbsp;_highlighting-spec_ `/` _selector-pattern_

**Usage Example:** Suppose after highlighting many words and phrases in your document with `f=b,cr,kg` (bold, red text on green background), you decide that `f=bi,cb` (bold-italic, blue text) would have been a better choice. Making the change manually would be tedious; fortunately, selector patterns provide a better way:

1. Select a range of lines containing all the text you wish to change.
2. Type `\h`
3. At the prompt, type `f+i,cb,k- / fb & cr & kg`<br>
   **Note:** `f=bi,cb,k- / fb & cr & kg` would have accomplished the same thing.
4. Hit Enter to apply highlighting

**Explanation:** The highlighting spec adds italic, changes text color to blue and removes background highlighting altogether, but affects **_only_** bold red text on a green background.

Selector patterns are essentially boolean expressions combining format/color specs with standard logical operators:

| Logical Operators | Description                             |
| ----------------- | --------------------------------------- |
| `\|[\|]`          | Logical OR (may be abbreviated as `\|`) |
| `&[&]`            | Logical AND (may be abbreviated as `&`) |
| `!`               | Logical negation                        |
| `(` ... `)`       | Groups sub-patterns                     |

**Tip:** `|` and `&` are equivalent to `||` and `&&`, respectively, but to avoid confusion with the special `f|` and `f&` primitives, some users may prefer the long forms. 

The color specs used in selector patterns are identical to the ones used for highlighting. As with format highlighting specs, format selector specs begin with special operators that modify their behavior:

| Modifier            | Description                                                                          |
| ------------------- | ------------------------------------------------------------------------------------ |
| `\|`                | Matches regions containing _any_ of the subsequent format attributes                 |
| `&`                 | Matches regions containing _all_ of the subsequent format attributes                 |
| `=` (**_default_**) | Matches regions containing _exactly_ (all and only) the subsequent format attributes |

**Note:** `f&...`, `f|...` and `f=...` are atomic constructs, which do not allow embedded spaces.

**Tip:** Because it's the default modifier, you can drop the `=` from `f=<attrs>` in a selector expression. 

### Selector Pattern Examples

The following examples illustrate the use of highlighting specs with attached selector patterns:

| Spec/Pattern             | Action                                                         | Applies To                                                                              |
| ------------------------ | -------------------------------------------------------------- | --------------------------------------------------------------------------------------- |
| `fu/cr`                  | Add underline                                                  | red text                                                                                |
| `cb/cr\|\|f&bi`          | Make text blue                                                 | text that is red _**or**_ has both bold and italic attributes (and possibly others)     |
| `fu,kg/cr&&f\|bi`        | Add underline and make background green                        | text that is red _**and**_ has _either_ bold _or_ italic attributes (and possibly both) |
| `fi/cr\|f=bu`            | Add italic                                                     | text that is red _**or**_ bold-underline                                                |
| `cb,kr,f-i/fbi&!(cr&k-)` | Make text blue on a red background and remove italic attribute | text that is bold-italic **_and not_** red on a colorless background                    |

**Tip:** Whitespace is mostly ignored by Txtfmt, and can be used to make the expressions easier to read.<br>
**Note:** Highlighting types omitted from the selector expression are unconstrained: e.g., a selector expression of `fb` matches bold text of _any_ foreground/background color (including none).

## "Smart" Delete Operator Override

Although Txtfmt renders the highlighting tokens invisible, to Vim they are simply characters in your buffer like any other: thus, Vim's builtin delete operators make no distinction between text and tokens. To see how this can be problematic, consider the following scenario... You execute `dd` to delete the first line of a block of highlighted text. Although you intended to delete only text, the `dd` also removed the hidden highlighting tokens at the start of the block; thus, you have inadvertently removed the highlighting of the entire block! To mitigate this issue, Txtfmt provides a "smart" delete operator (`\d`) in both Normal and Visual mode, which will add, remove and replace tokens as needed to prevent changes to the highlighting that follows the deleted text.

# Leading Indent (highlighting vs page/paragraph fill)

If you've ever used a word-processor, you may have noticed that when a block of text is indented, attributes such as underline and background color are not visible in the margin. When leading spaces or tabs are used to indent blocks of text in Vim, it generally looks best not to highlight them. But what is "leading indent", exactly? All leading whitespace? Spaces only? Tabs only? Some combination of tabs and spaces, as determined by options such as 'tabstop' and 'shiftwidth'? The value of Txtfmt's `'leadingindent'` option provides the answer to this question, as shown in the table below:

| Setting               | Definition of "Leading Indent"                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| --------------------- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| none                  | No special treatment of leading whitespace, which will be highlighted like any other text. Note that this was the default behavior up until Txtfmt version 4.0, which introduced this option.                                                                                                                                                                                                                                                                                                                                                                |
| space                 | Longest sequence of leading spaces (ASCII 32).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| tab                   | Longest sequence of leading tabs (ASCII 9).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| white (**_default_**) | Longest sequence of leading whitespace (spaces and/or tabs).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| smart                 | Complex logic uses 'tabstop' and 'shiftwidth' to determine whether a sequence of leading tabs and/or spaces is considered leading indent.<br> **Heuristic:** Any sequence of leading whitespace that could be generated on an empty line by a sequence of `CTRL-T`'s in Insert mode or right-shifts (`>>`) in Normal mode.<br> **Caveat:** If you use the "smart" setting, you should consistently use mechanisms such as `CTRL-T` and `>>` to insert leading indent: in particular, inserting literal TABs for leading indent may yield unexpected results. |

**Note:** The logic that recognizes leading indent considers only _actual_ whitespace, ignoring any embedded highlighting tokens.

`:help txtfmt-'leadingindent'`

## "Smart" Shift/Indent Overrides

Because highlighting tokens are invisible, yet appear as normal text to Vim, their presence in leading indent could lead to unexpected shift/indent behavior if Txtfmt did not override the builtin shift/indent commands `<<`, `>>`, `CTRL-T` and `CTRL-D`. Txtfmt's overrides understand the special role of tokens in a Txtfmt buffer (as well as the implications of the various `'leadingindent'` option settings), and will go to great lengths to ensure that various shift/indent commands "do the right thing".

## "Smart" :Retab Command

For reasons described in the preceding section, the presence of highlighting tokens in leading indent tends to break the behavior of Vim's builtin `:retab` command. Accordingly, Txtfmt provides a token-aware `:Retab`, invoked just like the builtin :retab, which attempts to make retabbing work sensibly when leading indent contains Txtfmt tokens, taking into account the buffer's `'leadingindent'` option.

# Shortcut Maps

The only drawback to auto maps is that each highlighting operation requires the user to enter a highlighting spec. Although the highlighting specs are concise and intuitive, most users find themselves using a few "favorites" far more frequently than others. Having to enter the same specs over and over can be tedious and annoying. Shortcut maps allow such favorites to be assigned to keystroke mappings, thereby reducing the tedium and cognitive load experienced by the user.
**Note:** Early versions of Txtfmt provided a feature known as "user-maps", which allowed Txtfmt manual maps (discussed in a later section) to be embedded in keystroke mappings. Although this feature is still supported for backwards compatibility, there is really no longer a reason to use it: shortcut maps are **_much_** easier to use and **_much_** more powerful.

Shortcut maps are defined by adding entries to a List named `txtfmtShortcuts` defined at either global or buf-local scope. Each element in the list is either a string or a Dictionary that defines a single highlighting spec, along with one or more (potentially mode-specific) keystroke sequences used to trigger the highlighting action. The following Vim script snippet illustrates the use of both string and Dictionary syntax to define shortcuts. You could place something like this in your .vimrc; alternatively, you could change the `g:` prefix to `b:` and put the assignments in a function called from an autocommand. (This approach might be useful if you use Txtfmt in different types of files (denoted by file extension), and would like to define a different set of mappings for each file type.)



## Shortcut Map Configuration

```vim
" Define some Txtfmt shortcut maps
" Note: Using call add(...) syntax makes it easier to comment individual entries
let g:txtfmtShortcuts = []
" bold (\b for both Visual and Operator auto maps)
call add(g:txtfmtShortcuts, '\b fb')
" bold-italic with green background (,bg for both Visual and Operator auto maps)
" Note the use of Dictionary syntax.
call add(g:txtfmtShortcuts, {'lhs': ',bg', 'rhs': 'fbi kg'})
" bold with red background and blue foreground' (\b for Visual, ,b for Operator)
call add(g:txtfmtShortcuts, 'v:\b o:,b fb kr cb')
" bold with red foreground (<LocalLeader>r for Visual, <F8> for Operator)
call add(g:txtfmtShortcuts, {'lhs': {'v': '<LocalLeader>r', 'o': '<F8>'}, 'rhs': 'fb cr'})
" Same as previous, but using <F8> for both the operator and select mode invocation
" Rationale: Some users may prefer different mappings in visual and select mode.
call add(g:txtfmtShortcuts, {'lhs': {'x': '<LocalLeader>r', 'so': '<F8>'}, 'rhs': 'fb cr'})
" .
" .
```



## Shortcut Map Sample Descriptions

The following table contains sample List entries, along with a description of the map(s) created:

| List Entry                                                   | Result                                                                                                                                  |
| ------------------------------------------------------------ | --------------------------------------------------------------------------------------------------------------------------------------- |
| `'\b fbi'`                                                   | Create a "bold-italic" preset and use `\b` for both Visual and Operator maps                                                            |
| `'v:\b o:,b fb'`                                             | Like the preceding, but use `\b` for the Visual map and `,b` for the Operator map                                                       |
| `{'lhs': {'v': '\b', 'o': ',b'}, 'rhs': 'fbi'}`              | Like the preceding, but using Dictionary syntax                                                                                         |
| `{'lhs': {'x': '\b', 's': '<F8>', 'o': ',b'}, 'rhs': 'fbi'}` | Like the preceding, but with different keystroke sequences forVisual and Select maps                                                    |
| `'<LocalLeader>? fi/fb (cr \| kr)'`                          | Create a preset mapped to `<LocalLeader>?i` which adds the italic attribute to any bold region with red in its foreground or background |

### 

### Points to Note...

- The rhs of a shortcut definition can contain anything you would enter at a Txtfmt prompt after executing a Visual or Operator auto map, including the _selector patterns_ discussed in [Selective (Pattern-Based) Highlighting](./README.md#selective-highlighting).

- String and Dictionary style entries may be freely intermixed throughout the `txtfmtShortcuts` list. The only caveat is that Dictionary style supports literal whitespace in the lhs key sequence, whereas string style requires the use of Vim's special key notation (e.g., `<space>` for the space key).
  
  

## Default Shortcut Maps

If you set `g:txtfmtEnableDefaultShortcuts` (or its buf-local counterpart) to 1, the plugin will define a handful of potentially useful shortcuts before processing the contents of the `g:txtfmtShortcuts[]` and `b:txtfmtShortcuts[]` lists. The current defaults are provided below in a format suitable for pasting into a .vimrc, in case you wish to use it as a starting point for your own shortcuts.



```vim
let g:txtfmtShortcuts = [
			\ '-f f-',
			\ '-c c-',
			\ '-k k-',
			\ '-- f- c- k-',
			\ ',b fb',
			\ ',i fi',
			\ ',u fu',
			\ '_r cr',
			\ '_g cg',
			\ '_b cb',
			\ '_R kr',
			\ '_G kg',
			\ '_B kb',
\]
```

**Caveat:** These default shortcuts are subject to change in future versions of Txtfmt. Moreover, the colors used in the foreground/background color specs assume the default color names. If you have invalidated a color spec by renaming or disabling the corresponding color (using techniques discussed in [Color Configuration](./README.md#color-configuration)), the corresponding map will be skipped.



**Note:** If you wish to use the default maps, but with different map leaders, you can use global or buf-local Dictionary option `txtfmtDefaultShortcutLeaders{}` to change the leading portion of the default maps. The keys of `txtfmtDefaultShortcutLeaders{}` are used as Vim "magic" regular expressions anchored to the start of the map lhs; the corresponding values are used as replacement strings. Swapping the leaders used by the default format and color maps is as simple as this:

```vim
let g:txtfmtDefaultShortcutLeaders = {',': '_', '_': ','}
```



But Vim's pattern syntax can be used to accomplish more complicated remappings: e.g. the following setting could be used to change comma maps to the corresponding "metafied" form (e.g., `,b` =>`<M-b>`):

```vim
let g:txtfmtDefaultShortcutLeaders = {',\(.\)': '<M-\1>'}
```

**Note:** The substitutions performed are strictly textual; thus, when a key may be specified in multiple ways (e.g., `<Bslash> or `\\`), you must select the form used by the default maps.

# Manual Maps

**Note:** The typical user will probably never need to use "manual maps", but they're presented here for the sake of completeness.

Manual maps are used to insert specific highlighting tokens at specific locations in the buffer. Each token determines either the text color, background color, or format attributes in effect up until the subsequent token of the same type (possibly the end token `'-'`). With auto maps, you simply specify the desired highlighting changes, and Txtfmt _automagically_ performs the required token insertions and removals. This task is actually far more complex than it sounds. To see why, suppose that after selecting some text, you use an auto map with highlighting spec `fb,cr,kg` (add bold, text red, background green). You might assume that Txtfmt would insert 6 tokens as follows:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
_`<kr><cb><fb> <selected text...> <k-><c-><f->`_

But this is only one of many possibilities. Consider that there may be tokens _within_ the selected text that need to be removed. Leaving a `<cg>` token, for instance, would be harmful, as it would prematurely terminate the blue text region. A `<cb>` token within the region would be harmless but redundant with the one at the beginning of the region, so it should really be removed. But what about format attribute tokens within the region? They cannot simply be removed, since the highlighting spec requested only the _**addition**_ of bold, not the removal of anything else! An `<fi>` token within the region, therefore, would need to be _**replaced**_ with an `<fbi>` token to satisfy the request without disturbing existing highlighting. Similarly, if the text just _past_ the selected region were originally underline-italic, the auto map would need to end the region with an `<fui>` token ( _**not**_ `<f->` ), to avoid changing the highlighting of unselected text.

Auto maps provide an abstraction that lets you think in terms of a desired highlighting change, shielding you from the often tedious details involved in effecting the change. So why would you ever want to use manual maps instead of auto maps? Most users won't, but there may be scenarios in which the explicit control afforded by manual maps is useful. Accordingly, here's a quick walkthrough...

Suppose you want to enter some green text... Execute `\i`, and enter `cg` at the prompt (**_mnemonic:_** color green): 

The text you type now is green. 

While typing green text, you wish to emphasize a phrase by making it bold-italic. Still in Insert mode, execute `CTRL-\CTRL-\` and enter `fbi` (or `fib`) at the prompt (**_mmemonic:_** format bold italic).

The text you type now is green bold-italic.

Now you wish to enter some text with a blue background. Still in Insert mode, execute `CTRL-\CTRL-\` again and enter `kb` at the prompt (**_mnemonic:_** bac _k_ ground blue). 

The text you type now is green bold-italic on a blue background. 

At this point, any of the open highlighting regions can be terminated with the corresponding terminator token (`f-`, `c-` or `k-`). Alternatively, you can enter a non-terminator highlighting spec to change subsequent highlighting without explicitly terminating the preceding region. Inserting a `cr` token, for instance, would switch from green text color to red.

To finish the example, let's terminate the 3 active regions by executing `CTRL-\ CTRL-\` from Insert mode one last time and entering `c-,f-,k-` (or `c- f- k-`) at the prompt (**_mnemonic:_** no color, no format, no bac _k_ ground color).

The text you type now is plain, unhighlighted text.

**Note:** The preceding example used _insert token_ maps `\i` and `CTRL-\CTRL-\` to insert highlighting tokens. There are actually 15 such maps in all, but they're easy for a Vim user to remember by analogy with Vim's builtin commands for entering insert mode: e.g., `\i` inserts before cursor; `\a` inserts after cursor; `\I` inserts at beginning of line; etc... Moreover, each of those commands has a variant beginning with `\v` (e.g., `\vi`), which returns to Normal mode after inserting the token. Finally, `CTRL-\CTRL-\` allows you to insert a token without leaving Insert mode.

`:help txtfmt-ins-tok-maps`

## Cursor Positioning with Dot (`.`)

As with auto map highlighting specs, multiple format/color specs can be concatenated in a comma or space-separated list. Moreover, you can replace one of the commas with a dot (`.`) to specify where the cursor should be positioned relative to the inserted tokens. This feature provides a convenient way to enter both the start and end tokens of a region before beginning to type the highlighted text. If, for example, you wanted to enter red bold text on a yellow background, you could enter <nobr>`cr fb ky . c- f- k-`</nobr> to insert all the tokens at once, leaving the cursor between the start and end tokens, ready to enter the highlighted text.

## Jump to Token Maps

As long as you're using auto maps, you shouldn't need to think much about Txtfmt highlighting tokens: the tokens are invisible as long as the Txtfmt syntax is active, and all token insertion/removal is handled automatically by the plugin. If you're using manual maps, however, you _**will**_ need to work with the tokens: specifically, you will need to insert, replace and delete them. Txtfmt provides maps to facilitate insertion (`\i`, `\I`, `\a`, `\A`, `\o`, `\O`) and replacement (`\s`). Vim's builtin delete operators (e.g., `x`, `X`, etc...) may be used to delete tokens, but since the tokens themselves are invisible, Txtfmt provides a handful of maps to help you _**find**_ the tokens you want to delete/change. Collectively, these maps are referred to as "Jump to Token" maps, and they can all be used in both Normal and Operator-pending mode. There are quite a few variants, allowing you to specify direction of jump, type of token sought (text color, background color, format attributes), and whether to land _on_ or _next to_ the target token.

### Jump to Token Map Examples

| Map | Description                                                                                            |
| --- | ------------------------------------------------------------------------------------------------------ |
| ]f  | forward to next format token                                                                           |
| [c  | backward to previous text color token                                                                  |
| ]a  | forward to next token of _any_ type                                                                    |
| ]tf | forward to char _**before**_ next format token (**_mnemonic_** "till")                                 |
| [ea | backward to previous _end_ token of any type (i.e., `<f->`, `<c->` or `<k->`)                          |
| ]bf | forward to next format _begin_ token (i.e., token that specifies format attributes, _**not**_ `<f->` ) |

`:help txtfmt-jump-to-tok`

## Token Inspection

If you ever need to know a token's type, simply position your cursor on it (e.g., using one of the "jump to token" maps described above) and execute...
    `\ga`
Like builtin `ga` and `g8`, this command displays information in the statusline about the character under the cursor, but instead of a character code, it displays a short string indicating the token type.

### \ga Examples

| Statusline | Meaning                      |
| ---------- | ---------------------------- |
| fibu       | Format italic-bold-underline |
| c1         | Foreground color #1          |
| k3         | Background color #3          |
| f-         | Format end token             |

`:help txtfmt-get-tok-info`

# Configuration

## Options

Although the default settings of most Txtfmt options will be fine for the average user, there are 3 distinct mechanisms provided for customization:

| Mechanism              | Description                            | Motivation                                     | Example                                                                  |
| ---------------------- | -------------------------------------- | ---------------------------------------------- | ------------------------------------------------------------------------ |
| Txtfmt "modelines"     | analogous to Vim modelines             | associate options with file                    | <nobr>`    txtfmt: li=smart nested`</nobr><br><nobr>`    vim: ft=txtfmt` |
| Buffer-local variables | setting applies to current buffer only | set from an autocommand for specific file type | <nobr>`let b:txtfmtLeadingindent = 'white'`</nobr>                       |
| Global variables       | setting applies to all Txtfmt buffers  | set from vimrc                                 | <nobr>`let g:txtfmtTokrange = '180X'`</nobr>                             |

`:help txtfmt-options`

For a list of all supported options...

`:help txtfmt-opt-list`

<div id=color-configuration />

## Color Configuration

Txtfmt's default configuration provides 8 foreground colors and 3 background colors:

| Color     | Color # | Abbrev | Terminal    | GUI     | Available in Background? |
| --------- | ------- | ------ | ----------- | ------- | ------------------------ |
| Black     | 1       | k      | Black       | #000000 | No                       |
| Blue      | 2       | b      | DarkBlue    | #0000FF | Yes                      |
| Green     | 3       | g      | DarkGreen   | #00FF00 | Yes                      |
| Turquoise | 4       | t      | DarkCyan    | #00FFFF | No                       |
| Red       | 5       | r      | DarkRed     | #FF0000 | Yes                      |
| Violet    | 6       | v      | DarkMagenta | #FF00FF | No                       |
| Yellow    | 7       | y      | DarkYellow  | #FFFF00 | No                       |
| White     | 8       | w      | White       | #FFFFFF | No                       |

But everything about these defaults is configurable. For instance, if you have a relatively modern computer, you might wish to activate all 8 colors for background highlighting. You could do so with the following override of `'bgcolormask'` in your vimrc:

```vim
" Unmask background colors disabled by default
let g:txtfmtBgcolormask = "11111111"
```

You could accomplish the same thing _for a single file_ by adding the following "Txtfmt modeline" to the beginning or end of the file:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`txtfmt:bcm=11111111`

**Note:** Foreground and background colors can be enabled/disabled independently.

`:help txtfmt-'fgcolormask'`<br>
`:help txtfmt-'bgcolormask'`

The color definitions themselves are also customizable. If black is the default text color in your preferred colorscheme, you may not want to waste 1 of 8 color slots on it. You could change color #1 from black to slate gray by adding the following line to your vimrc:

`let g:txtfmtColor{1} = '^G$,c:Gray,g:#708090'`

Similarly, if the default for color #6 (violet) seems a bit too bright in the GUI, you could tone it down and change its abbreviation from "v" (violet) to "p" (purple) by adding the following to your vimrc:

`let g:txtfmtColor{6} = '^p$,c:DarkMagenta,g:#800080'`

**Note:** You don't actually specify a _name_ for a color, but a regex that matches any string you might use to specify it in a highlighting spec. The `\%[]` construct is used in the default regexes to permit names to be abbreviated or typed out in part or full, but most users will probably want to use single-letter abbreviations.

**Note:** The `c:` and `g:` in the color definition strings refer to "color terminal" and "gui", respectively. The full color string syntax allows you to define different colors for different terminal/GUI types.

`:help txtfmt-color-config`

<div id=token-range />

## Token Range

Txtfmt requires a block of contiguous characters for use as highlighting tokens. Both the location and size of the block are determined by the value of the `'tokrange'` option, which can be set via Txtfmt option variable or Txtfmt modeline. The `'tokrange'` option is a string consisting of a decimal or hexadecimal number with a trailing suffix: e.g., `"0xE000X"` or `"180S"`. The numeric value is the character code of the first character in the block, and the (case-insensitive) suffix determines the available highlighting, as shown in the following table:

| 'tokrange' Suffix                  | Supported Highlighting                                                        | Number of Tokens in Range      |
| ---------------------------------- | ----------------------------------------------------------------------------- | ------------------------------ |
| `S` (short)                        | underline,bold,italic + fg colors                                             | 17                             |
| `X` (extended)<br> (**_default_**) | underline,bold,italic + fg colors + bg colors                                 | 26                             |
| `L` (long)                         | underline,bold,italic,standout,reverse (and optionally undercurl) + fg colors | 41 (73 if "undercurl" enabled) |

`:help txtfmt-'tokrange'`<br>
`:help txtfmt-'undercurl'`

If you don't set the 'tokrange' option explicitly, Txtfmt will choose a default that makes sense for the encoding in use. This default should be fine for most users, but you can easily change it by adding something like this to your vimrc...

```vim
" Place Txtfmt tokens at 0xE100 (256 bytes past start of Unicode 'Private Use Area')
" and enable 'Long' formats for standout, reverse, etc.
let g:txtfmtTokrange = '0xE100L'
```

Alternatively, you could customize `'tokrange'` for a single file using a Txtfmt modeline: e.g.,

&nbsp;&nbsp;&nbsp;&nbsp;`txtfmt:tokrange=0xE100X`

**Caveat:** Changes to g:txtfmtTokrange impact highlighting in **_all_** existing Txtfmt files without a modeline; thus, any change to g:txtfmtTokrange globally should be accompanied by a migration of existing files. Txtfmt provides the `:MoveStartTok` command to help with this.

`:help txtfmt-'MoveStartTok'`

### Displaying Token Range

The `:ShowTokenMap` command provides a tabular illustration of the 'tokrange' in effect for the current buffer. Each row of the table lists a token's character code along with its meaning (e.g., "underline-bold" or "Color3"). Additional information is displayed for a color token: its name pattern, RGB value (either as hex value or terminal-specific color name) and enabled/disabled status.

`:help txtfmt-:ShowTokenMap`

# Installation

Txtfmt can be installed using any of the standard Vim plugin mechanisms: e.g., Pathogen, Vundle, etc. If you're not using a plugin manager, simply uncompress the Txtfmt distribution somewhere in your 'runtimepath' (e.g., ~/.vim) and run <nobr>`:helptags ALL`</nobr> to prepare the Txtfmt help.

<div id=plugin-loading />

# Plugin Loading

**_Caveat:_** Like most plugins, Txtfmt requires both filetype detection and syntax highlighting to be enabled, yet neither is enabled in Vim's default configuration; thus, unless you copied the commands from the example vimrc (or source the example vimrc from your own vimrc), you will probably need to add something like the following to your vimrc:

```vim
syntax on
filetype plugin on
```

Assuming the plugin has been properly installed, you can load it manually by executing the following from the Vim command line:

`:set ft=txtfmt`

A better approach, however, is to ensure that Txtfmt is loaded automatically whenever you open certain files or types of file. Several approaches are outlined below...

## Custom filetype.vim

If it doesn't already exist, create your own filetype.vim in the first directory of 'runtimepath' (e.g., ~/.vim) and add an autocommand that associates txtfmt with the desired file extensions.

**Example:**<br>
-- _filetype.vim_ --<br>

```vim
augroup filetypedetect
  " Treat files with the following extensions as Txtfmt files: .txt, .tf, .jnl
  au! BufRead,BufNewFile *.txt,*.tf,*.jnl    setfiletype txtfmt
augroup END
```

## Plugin-specific file in ftdetect

If it doesn't already exist, create your own "ftdetect" folder in the first directory of 'runtimepath' (e.g., ~/.vim). In ftdetect, create a file called "txtfmt.vim" containing an autocommand that associates txtfmt with the desired file extensions.

**Example:**<br>
-- _ftdetect/txtfmt.vim_ --<br>

```vim
" Treat files with the following extensions as Txtfmt files: .txt, .tf, .jnl
au! BufRead,BufNewFile *.txt,*.tf,*.jnl    set filetype=txtfmt
```

**Note:** When ftdetect is used, there's no need to wrap your autocommands in an `augroup` block, as your script is sourced from within an autocommand group.

`:help new-filetype`

## Modeline

Modelines can be used to set `'filetype=txtfmt'` when the files you wish to highlight do not share a common extension. Simply add a line like the following to the beginning or end of a file you wish to highlight:

&nbsp;&nbsp;&nbsp;&nbsp;`vim:ft=txtfmt`

**Note:** You'll need to close and reopen the file to cause the modeline to be processed.

## Combining Txtfmt with Other Filetypes

It is quite possible to use Txtfmt highlighting in a file associated with another filetype plugin. For instance, if you're using a plugin that facilitates note-taking or journaling, you may wish to use Txtfmt to highlight the entries. It is even possible to "nest" Txtfmt highlighting regions within syntax regions defined by other plugins, but nesting must be enabled explicitly by setting the `txtfmt-'nested'` option.

`:help txtfmt-combining`<br>
`:help txtfmt-nesting`
