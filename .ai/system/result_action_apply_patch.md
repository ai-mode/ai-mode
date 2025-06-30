## Output Format for Content Replacement - Unified Patch Format

**This request expects content that will be returned as a unified patch format ready for direct application.**

# Unified Diff Generation Instructions for AI Agents

## Mandatory Rules

### 1. Header Structure
```
--- path/to/original/file.ext	timestamp
+++ path/to/modified/file.ext	timestamp
```

**Critical requirements:**
- Use exactly **3 dashes** `---` for original file
- Use exactly **3 plus signs** `+++` for modified file
- Always include tab character between path and timestamp
- Timestamp format: `YYYY-MM-DD HH:MM:SS.000000000 +TIMEZONE`
- **Always use relative paths from project root** (e.g., `src/main.js`, `docs/README.md`, not `/absolute/path/to/src/main.js`)

### 2. Hunk Structure
```
@@ -start_line,count +start_line,count @@
```

**Hunk header rules:**
- Start with `@@`, end with `@@`
- Single space after first `@@` and before last `@@`
- `-start_line,count` for original file
- `+start_line,count` for modified file
- If `count = 1`, you may omit `,1`

### 3. Line Prefixes
- ` ` (space) - unchanged line (context)
- `-` - deleted line
- `+` - added line

**CRITICAL:** Never use tabs at the beginning of diff content lines - only single character prefix!

## Step-by-Step Generation Algorithm

### Step 1: Analyze Changes
1. Identify exact line numbers that changed
2. Select context (typically 3 lines before and after changes)
3. Calculate start_line and count for original and modified files

### Step 2: Generate Headers
```
--- a/filename.ext	2024-01-15 10:30:00.000000000 +0000
+++ b/filename.ext	2024-01-15 10:35:00.000000000 +0000
```

**Path Rules:**
- Use relative paths from the project root directory
- Strip any absolute path prefixes
- Maintain directory structure relative to project root
- Examples of correct paths:
  - `src/components/Button.js`
  - `tests/unit/helper.test.js`
  - `docs/api/README.md`
  - `package.json`

### Step 3: Create Hunk
1. Calculate hunk parameters:
   - `old_start`: line number of first line in original file
   - `old_count`: number of lines in original file (context + deleted)
   - `new_start`: line number of first line in modified file
   - `new_count`: number of lines in modified file (context + added)

2. Create hunk header:
   ```
   @@ -old_start,old_count +new_start,new_count @@
   ```

### Step 4: Generate Content
1. Add top context lines with ` ` prefix
2. Add deleted lines with `-` prefix
3. Add added lines with `+` prefix
4. Add bottom context lines with ` ` prefix

## Examples with Explanations

### Example 1: Simple Replacement
**Original file (lines 1-4):**
```
line 1
old line 2
line 3
line 4
```

**Modified file (lines 1-4):**
```
line 1
new line 2
line 3
line 4
```

**Correct diff:**
```diff
--- a/src/utils/helper.js	2024-01-15 10:30:00.000000000 +0000
+++ b/src/utils/helper.js	2024-01-15 10:35:00.000000000 +0000
@@ -1,4 +1,4 @@
 line 1
-old line 2
+new line 2
 line 3
 line 4
```

**Explanation:**
- `@@ -1,4 +1,4 @@`: start at line 1, show 4 lines in both versions
- old_count = new_count = 4 (1 context + 1 deleted + 1 added + 1 context)
- Path `src/utils/helper.js` is relative to project root

### Example 2: Adding Lines
**Original file:**
```
line 1
line 2
line 4
```

**Modified file:**
```
line 1
line 2
line 3
line 4
```

**Correct diff:**
```diff
--- a/components/App.vue	2024-01-15 10:30:00.000000000 +0000
+++ b/components/App.vue	2024-01-15 10:35:00.000000000 +0000
@@ -1,3 +1,4 @@
 line 1
 line 2
+line 3
 line 4
```

### Example 3: Deleting Lines
**Original file:**
```
line 1
line 2
line 3
line 4
```

**Modified file:**
```
line 1
line 4
```

**Correct diff:**
```diff
--- a/config/database.yml	2024-01-15 10:30:00.000000000 +0000
+++ b/config/database.yml	2024-01-15 10:35:00.000000000 +0000
@@ -1,4 +1,2 @@
 line 1
-line 2
-line 3
 line 4
```

### Example 4: Multiple Hunks
**Original file:**
```
function foo() {
    old code here
    return 1;
}

function bar() {
    old implementation
    return 2;
}
```

**Modified file:**
```
function foo() {
    new code here
    return 1;
}

function bar() {
    new implementation
    return 2;
}
```

**Correct diff:**
```diff
--- a/lib/core/functions.js	2024-01-15 10:30:00.000000000 +0000
+++ b/lib/core/functions.js	2024-01-15 10:35:00.000000000 +0000
@@ -1,4 +1,4 @@
 function foo() {
-    old code here
+    new code here
     return 1;
 }
@@ -6,4 +6,4 @@ function foo() {

 function bar() {
-    old implementation
+    new implementation
     return 2;
 }
```

## Common Mistakes and How to Avoid Them

### ❌ Wrong:
```diff
@@ -1,3 +1,3@@  // Missing space before last @@
	line 1       // Tab instead of space
--line 2      // Double prefix
+ line 3      // Space after prefix
```

### ✅ Correct:
```diff
@@ -1,3 +1,3 @@
 line 1
-line 2
+line 3
```

### ❌ Wrong line counting:
```diff
@@ -1,2 +1,3 @@  // Says 2 old lines, 3 new lines
 line 1         // context (counts in both)
-old line       // deleted (counts in old only)
+new line 1     // added (counts in new only)
+new line 2     // added (counts in new only)
 line 3         // context (counts in both)
```
**Problem:** Actually shows 3 old lines and 4 new lines!

### ✅ Correct counting:
```diff
@@ -1,3 +1,4 @@
 line 1
-old line
+new line 1
+new line 2
 line 3
```

### ❌ Wrong paths:
```diff
--- /Users/john/myproject/src/main.js	2024-01-15 10:30:00.000000000 +0000
+++ /Users/john/myproject/src/main.js	2024-01-15 10:35:00.000000000 +0000
```

### ✅ Correct paths:
```diff
--- a/src/main.js	2024-01-15 10:30:00.000000000 +0000
+++ b/src/main.js	2024-01-15 10:35:00.000000000 +0000
```

## Validation Checklist

Before outputting, verify:
1. ✅ Headers start with `---` and `+++`
2. ✅ Hunk header has format `@@ -a,b +c,d @@`
3. ✅ Every content line starts with ` `, `-`, or `+`
4. ✅ Line counts match actual lines in hunk
5. ✅ No tabs at beginning of content lines
6. ✅ Context lines are identical in both versions
7. ✅ Deleted lines only appear with `-` prefix
8. ✅ Added lines only appear with `+` prefix
9. ✅ **File paths are relative to project root**

## Advanced Guidelines

### For Multiple Changes:
- Create separate hunks for distinct change groups
- Leave at least 3 lines of context between hunks
- If changes are close together, merge into single hunk

### For Large Files:
- Limit context to reasonable number of lines
- Use `@@` with function names for better readability:
  ```diff
  @@ -45,8 +45,9 @@ function myFunction() {
  ```

### For Binary Files:
```diff
Binary files a/assets/images/logo.png and b/assets/images/logo.png differ
```

### For New Files:
```diff
--- /dev/null	1970-01-01 00:00:00.000000000 +0000
+++ b/src/new-feature.js	2024-01-15 10:35:00.000000000 +0000
@@ -0,0 +1,3 @@
+line 1
+line 2
+line 3
```

### For Deleted Files:
```diff
--- a/deprecated/old-module.js	2024-01-15 10:30:00.000000000 +0000
+++ /dev/null	1970-01-01 00:00:00.000000000 +0000
@@ -1,3 +0,0 @@
-line 1
-line 2
-line 3
```

## Counting Algorithm

**For each hunk:**
1. Count context lines that appear in original → add to old_count
2. Count deleted lines (with `-` prefix) → add to old_count only
3. Count added lines (with `+` prefix) → add to new_count only
4. Count context lines that appear in modified → add to new_count

**Formula:**
- `old_count = context_lines + deleted_lines`
- `new_count = context_lines + added_lines`

## Auto-Verification

Every generated diff should:
- Apply without errors using `patch` command
- Produce expected result when applied
- Be readable and understandable by humans
- Pass syntax validation tools

## Output Format Template

```diff
--- a/{relative_path_from_project_root}	{timestamp}
+++ b/{relative_path_from_project_root}	{timestamp}
@@ -{old_start},{old_count} +{new_start},{new_count} @@
{context_line_with_space_prefix}
{deleted_line_with_minus_prefix}
{added_line_with_plus_prefix}
{context_line_with_space_prefix}
```

**Remember:** Every character matters in diff format. One wrong space or tab can make the diff invalid! Always use relative paths from the project root to ensure portability and compatibility across different development environments.
