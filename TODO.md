# TODO

- `anchor`
- `image`
- `inline_figure`
- `internal_reference`
- `footnote`
- `def-prop` environment
- add spacer to provide multiple arguments to an env (like exercise and solution), for example :::

# DOING


# DONE

## Display
  - `headings`
  - `hr`
  - `lists`
  - `environment`
  - `math_display`
  - `code_block`

## Inline
  - `math_inline`
  - `code_inline`
  - `compact_list`
  - `nbsp`
  - `link` (fixed math, code, parenthesis, brackets)
  - `sidenote` (fixed math, code, parenthesis)
  - `bold` (fixed math, code)
  - `italic` (fixed math, code)
  - `underline` (fixed math, code)
  - `highlight` (fixed math, code)
  - `strikethrough` (fixed math, code)

# FIXES

- bold = ** and italic = _
- common base for feature (with fixes for math, code…)

# IDEAS

- References: add <ref_label> before any node; the renderer will be called with a special argument if a label is placed before, will add an id attr and return the reference text (displayed between <a></a>); the `render_document` function will then store this text and make it accessible to called renderers.
  ```md
    # Heading

    <head_1>## Subheading

    <p1>A paragraph and <it>_italic text_. Go back to #[head_1] or jump forward to #[thm_1].

    <list>- item
    - item
    <third_item>- item
    - item

    The following <ref> will do nothing as it is in the middle of a node.

    <thm_1>%thm Name of the theorem
      A nice result
    %
  ```
