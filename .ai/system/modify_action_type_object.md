## How to Apply Instructions to `<modify-action-object>`

Used for manipulating a fragment of textual information located between the `<selection>` or `<file-context>` tags — such as simplifying, refactoring, fixing, or translating.

### Modify action type object structural format

    <example>
      <modify-action-object>
        <preceding-context ...>...context before...</preceding-context>
        <selection ...>...text fragment...</selection>
        <following-context ...>...context after...</following-context>
        <!-- or -->
        <file-context file="...">...text fragment...</file-context>
        <!-- or for multiple files -->
        <multi-file-context>
          <file-context file="...">...text fragment...</file-context>
          <file-context file="...">...text fragment...</file-context>
        </multi-file-context>
      </modify-action-object>
    </example>

### Modify action object rules

<modify-action-object-rules>
1. Rewrite the content between the tags (`<selection>`, `<file-context>`, or `<multi-file-context>`) based on the instruction.
2. Use exactly one of the following to identify the scope:
   - `<selection ...>...</selection>`
   - `<file-context ...>...</file-context>`
   - `<multi-file-context>...</multi-file-context>`
3. Context blocks `<preceding-context ...>...</preceding-context>` and `<following-context ...>...</following-context>` provide surrounding file content for reference only — they are **read-only** and should **never be included** in your output.
4. Use the context blocks to better understand the code structure, maintain consistency with existing patterns, and ensure your modifications fit seamlessly with the surrounding code.
5. Always preserve proper indentation and formatting as consistent with the surrounding file or project.
6. For multi-file modifications, ensure all changes are coordinated and maintain consistency across files (e.g., function signatures, import statements, variable names).
</modify-action-object-rules>
