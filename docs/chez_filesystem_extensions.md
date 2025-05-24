# Chez Scheme File System Extensions

## Directory Operations

### `directory-list`
```scheme
(directory-list path)
```
Returns a list of filenames in the specified directory.

### `current-directory`
```scheme
(current-directory)          ; get current working directory
(current-directory path)     ; set current working directory
```

### `mkdir`
```scheme
(mkdir path)                 ; create directory
(mkdir path mode)            ; create directory with specific permissions
```

### `rmdir`
```scheme
(rmdir path)                 ; remove empty directory
```

## File Type Testing

### `file-exists?`
```scheme
(file-exists? path)          ; true if file or directory exists
```

### `file-directory?`
```scheme
(file-directory? path)       ; true if path is a directory
```

### `file-regular?`
```scheme
(file-regular? path)         ; true if path is a regular file
```

### `file-symbolic-link?`
```scheme
(file-symbolic-link? path)   ; true if path is a symbolic link
```

## File Information

### `file-stat`
```scheme
(file-stat path)             ; returns file statistics record
```
Returns a record with fields like:
- `type` (regular, directory, symbolic-link, etc.)
- `mode` (permissions)
- `size`
- `atime`, `mtime`, `ctime` (access, modify, change times)
- `uid`, `gid` (user and group IDs)

### `file-stat-*` accessors
```scheme
(file-stat-type stat)
(file-stat-mode stat)
(file-stat-size stat)
(file-stat-atime stat)
(file-stat-mtime stat)
(file-stat-ctime stat)
(file-stat-uid stat)
(file-stat-gid stat)
```

## File Operations

### `rename-file`
```scheme
(rename-file old-path new-path)  ; rename/move file
```

### `chmod`
```scheme
(chmod path mode)            ; change file permissions
```

### `truncate-file`
```scheme
(truncate-file path length)  ; truncate file to specified length
```

## Path Manipulation

### `path-absolute?`
```scheme
(path-absolute? path)        ; true if path is absolute
```

### `path-parent`
```scheme
(path-parent path)           ; return parent directory
```

### `path-extension`
```scheme
(path-extension path)        ; return file extension
```

### `path-root`
```scheme
(path-root path)             ; return root component
```

### `path-first`, `path-rest`
```scheme
(path-first path)            ; first component of path
(path-rest path)             ; remaining components
```

## Environment and System

### `getenv`
```scheme
(getenv variable)            ; get environment variable
```

### `putenv`
```scheme
(putenv variable value)      ; set environment variable
```

### `system`
```scheme
(system command)             ; execute shell command
```

## Example Usage

```scheme
;; Check if a path is a directory and list its contents
(when (file-directory? "/home/user")
  (display "Contents:")
  (newline)
  (for-each display (directory-list "/home/user")))

;; Get file information
(let ((stat (file-stat "myfile.txt")))
  (printf "Size: ~a bytes~n" (file-stat-size stat))
  (printf "Modified: ~a~n" (file-stat-mtime stat)))

;; Create directory structure
(mkdir "project")
(current-directory "project")
(mkdir "src")
(mkdir "docs")

;; Work with paths
(let ((path "/home/user/document.txt"))
  (printf "Parent: ~a~n" (path-parent path))
  (printf "Extension: ~a~n" (path-extension path))
  (printf "Absolute? ~a~n" (path-absolute? path)))
```

## Notes

- These are Chez Scheme-specific extensions, not part of standard Scheme
- Some procedures may behave differently on different operating systems
- Error handling typically uses exceptions for invalid operations
- File permissions and modes are system-dependent (Unix-style on Unix systems)
- Some procedures may not be available on all platforms (e.g., symbolic links on Windows)