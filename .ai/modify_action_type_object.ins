## MODIFY_ACTION_OBJECT

Used for manipulating a fragment of textual information located between the `<selection>` or `<file-context>` tags — such as simplifying, refactoring, fixing, or translating.

**Modify action type object structural format:**

<modify-action-object-rules>
1. Rewrite the content between the tags (`<selection>` or `<file-context>`) based on the instruction.
2. Your output must replace only the content inside the container, preserving the defined scope.
3. Do not add any metadata, XML structure, backticks, or comments — return only raw code or text.
4. Use exactly one of the following to identify the scope:
   - `<selection ...>...</selection>`
   - `<file-context ...>...</file-context>`
5. If the <modify-action-object> contains `<selection ...>...</selection>` then return **only** the fully rewritten fragment inside selection container.
6. If the <modify-action-object> contains <file-context ...>...</file-context>, then your output **MUST be the entire modified file, including ALL original and changed content**.
7. Always preserve proper indentation and formatting as consistent with the surrounding file or project.
8. The editor infrastructure already knows what region to replace — your job is to generate only the **replacement content** for that region.
</modify-action-object-rules>


**Modify action type object structural format:**

    <example>
      <modify-action-object>
        <selection ...>...text fragment...</selection>
        <!-- or -->
        <file-context file="...">...text fragment...</file-context>
      </modify-action-object>
    </example>

**Example 1:**

    <example>
      <task>Refactor to remove duplication and improve readability</task>
      <modify-action-object>
        <selection ...>
          ...original text...
        </selection>
      </modify-action-object>
    </example>

**Example 2:**

    <example>
      <task>Refactor to remove duplication and improve readability</task>
      <modify-action-object>
        <file-context file="src/main.py">...full file content...</file-context>
      </modify-action-object>
    </example>

### Example 3

    <example>
      <task>Update file comments.</task>
      <modify-action-object>
        <file-context file="src/my_file.txt">
# File: MyFile
# Version: 1.0

Content line 1.
Content line 2.
        </file-context>
      </modify-action-object>

      <output>
# File: MyFile (Revised)
# Version: 2.0

Content line 1.
Content line 2.
      </output>
    </example>
