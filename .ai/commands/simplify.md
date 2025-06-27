# Instructions for **simplify** command

<task>
Rewrite the code to make it simpler and easier to understand. Remove unnecessary complexity, reduce nesting, and replace verbose constructs with cleaner alternatives — all while preserving full functionality.
</task>

<task-rules>
### 1. Read and understand the entire source code snippet
   - Examine code, comments, and doc‑strings before you begin.
   - Identify the public API (function signatures, class names, constants) that **must not change**.

### 2. Preserve external behaviour and interfaces
   - The simplified version must produce identical results, side‑effects, and error handling.
   - Do **not** alter public names, return types, accepted arguments, or overall control flow visible to callers.

### 3. Aim for clarity, brevity, and idiomatic style
   - Remove dead code, duplicate logic, unnecessary variables, and redundant conditionals.
   - Flatten deeply nested branches where reasonable.
   - Prefer built‑in functions, comprehensions, context managers, and standard‑library utilities already allowed by the original code base.
   - Keep line lengths reasonable and follow existing indentation, spacing, and naming conventions.

### 4. Maintain or improve readability
   - Retain meaningful comments and doc‑strings; update them if the code they describe changes.
   - Add short inline comments only when the logic is not obvious from the code itself.
   - Group related statements logically and keep similar transformations together.

###5. Keep the code safe, robust, and performant
   - Do not sacrifice input validation, error handling, or security checks for brevity.
   - If performance can be improved with no readability cost, apply the optimisation; otherwise prioritise clarity.

### 6. Introduce no new dependencies
   - Use only libraries already imported or part of the language’s standard library, unless explicitly instructed otherwise.
   - Do not introduce breaking changes to environments, configuration files, or tooling.

### 7. Output format rules
   - **Start your response immediately with the complete simplified code that will replace the highlighted block.**
   - **Do not include the original code anywhere in your response.**
   - **Do not wrap the simplified code in any markup** (no ``` or HTML).
   -  No commentary, headings, or formatting is allowed before the code; the code must come first.

### 8. Replacement semantics
   - The tooling selects a region of code between its internal markers; treat that region as **fully replaceable**.
   - Output exactly the replacement code for that region—nothing more, nothing less.

### 9. Avoid over‑simplification
    - Do not compress code into cryptic one‑liners if that hurts readability.
    - Respect explicit style guides present in the original snippet (e.g., type hints, explicit variable names).

</task-rules>
