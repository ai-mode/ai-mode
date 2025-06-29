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
2. Your output must replace only the content inside the container, preserving the defined scope.
3. Do not add any metadata, XML structure, backticks, or comments — return only raw code or text.
4. Use exactly one of the following to identify the scope:
   - `<selection ...>...</selection>`
   - `<file-context ...>...</file-context>`
   - `<multi-file-context>...</multi-file-context>`
5. If the <modify-action-object> contains `<selection ...>...</selection>` then return **only** the fully rewritten fragment inside selection container. The editor infrastructure already knows what region to replace — your job is to generate only the **replacement content** for that region.

6. If the <modify-action-object> contains `<file-context ...>...</file-context>`, then your output **MUST be the entire modified file, including ALL original and changed content**.
7. If the <modify-action-object> contains `<multi-file-context>`, then your output **MUST be structured as follows**:
   ```
   FILE: path/to/file1.ext
   entire modified content of file1

   FILE: path/to/file2.ext
   entire modified content of file2
   ```
8. Always preserve proper indentation and formatting as consistent with the surrounding file or project.
9. Context blocks `<preceding-context ...>...</preceding-context>` and `<following-context ...>...</following-context>` provide surrounding file content for reference only — they are **read-only** and should **never be included** in your output.
10. Use the context blocks to better understand the code structure, maintain consistency with existing patterns, and ensure your modifications fit seamlessly with the surrounding code.
11. For multi-file modifications, ensure all changes are coordinated and maintain consistency across files (e.g., function signatures, import statements, variable names).
</modify-action-object-rules>
