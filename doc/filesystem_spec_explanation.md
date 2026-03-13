# Filesystem Specification Explanation & Weakened Variants

This document provides a detailed explanation of the uHAT specifications and global properties found in `benchmarks/ADT/filesystem_spec.ml`. Additionally, it proposes "weakened" versions of these specifications suitable for evaluating verification tools under conditions of low-quality or imprecise specifications.

## 1. `initReq` (Initialization)

### Original Specification
```ocaml
let initReq = (epsilonA, InitReq true, starA (anyA - InitReq true))
```

*   **Explanation**:
    *   **History (`epsilonA`)**: The `InitReq` operation can only occur at the very beginning of the trace (when the history is empty).
    *   **Event**: The `InitReq` event occurs.
    *   **Future**: After initialization, any sequence of events can occur (`starA ...`), *except* another `InitReq`.
    *   **Meaning**: The system must be initialized exactly once, and it must be the first operation.

*   **Weakened Version**:
    *   *Note*: As per the guidelines, simple structural constraints like "happen once" or "start of trace" are generally preserved even in lower-quality specs, as they are fundamental to the lifecycle. We do not simplify this uHAT.

## 2. `createReq` (Create File/Directory)

### Original Specification
The original specification uses an intersection type (`[| ... |]`) to handle four distinct logical cases based on the path and the state of the file system.

1.  **Case 1 (Root Child)**: If `p` is a child of root, and we haven't created it yet (implied), it succeeds.
2.  **Case 2 (Already Exists)**: If `p` (non-root) was previously created and not deleted, the request returns `success == false`.
3.  **Case 3 (Parent Exists)**: If `p`'s parent was created and not deleted, the request returns `success == true`.
4.  **Case 4 (Root Child Fallback)**: A general success case for root children.

### Weakened Specification
We simplify this by merging the complex control flow into two broad cases: **Success** and **Failure**. We remove the precise history checks (e.g., checking if the parent exists or if the file already exists).

```ocaml
let createReq_weak =
  [|
    (* Case A: Generic Failure *)
    (fun ?l:(p = (true : [%v: Path.t])) 
         ?l:(c = (true : [%v: Byte.t])) ->
      ( allA, 
        CreateReq (path == p && content == c),
        (CreateResp (success == false); allA) 
      ));

    (* Case B: Generic Success *)
    (fun ?l:(p = (true : [%v: Path.t])) 
         ?l:(c = (true : [%v: Byte.t])) ->
      ( allA, 
        CreateReq (path == p && content == c),
        (CreateResp (success == true); 
         (* We still maintain the lifecycle that a successful create 
            might be followed by a delete *)
         allA; 
         DeleteReq (path == p); 
         allA) 
      ))
  |]
```

*   **Explanation of Simplification**:
    *   **Merged Control Flow**: Instead of distinguishing between "Parent is Root" vs "Parent is Normal Directory" vs "File Already Exists", we simply state that `createReq` can result in either `true` or `false`.
    *   **Simplified Qualifiers**: We replaced specific path constraints (e.g., `not (is_root (parent v))`) with `true`.
    *   **Removed Preconditions**: The "Success" case no longer requires the parent directory to exist in the history. This is an over-approximation: it allows the tool to explore traces where "orphan" files are created, which the original spec would forbid.

## 3. `deleteReq` (Delete File/Directory)

### Original Specification
The original spec handles three cases:
1.  **Root**: Deleting root always fails.
2.  **Failure**: Deleting a file that exists might fail (e.g. if it's a non-empty directory, though implicit).
3.  **Success**: Deleting a file that exists succeeds.

### Weakened Specification
We merge these into generic Success and Failure cases, removing the requirement that the file must exist in history to be deleted.

```ocaml
let deleteReq_weak =
  [|
    (* Case A: Generic Failure *)
    (fun ?l:(p = (true : [%v: Path.t])) ->
      ( allA,
        DeleteReq (path == p),
        (DeleteResp (success == false); allA) 
      ));

    (* Case B: Generic Success *)
    (fun ?l:(p = (true : [%v: Path.t])) ->
      ( allA,
        DeleteReq (path == p),
        (DeleteResp (success == true); 
         (* Future: It might be created again *)
         starA (anyA - CreateReq true)) 
      ))
  |]
```

*   **Explanation of Simplification**:
    *   **Merged Control Flow**: We no longer distinguish between deleting Root (which should always fail) and deleting normal files.
    *   **Removed Preconditions**: The "Success" case no longer checks `InitReq ... CreateReq (path == p)` in the history. This allows the model to simulate deleting a file that was never created (a "resource leak" or "double free" style behavior in the abstract model).

## 4. `existsReq` (Check Existence)

### Original Specification
1.  **Root**: Always exists.
2.  **Not Exists**: If file was deleted or not created, returns `false`.
3.  **Exists**: If file was created and not deleted, returns `true`.

### Weakened Specification
We merge this into a non-deterministic boolean return.

```ocaml
let existsReq_weak =
  [|
    (* Case A: Returns False *)
    (fun ?l:(p = (true : [%v: Path.t])) ->
      ( allA,
        ExistsReq (path == p),
        (ExistsResp (exists == false); allA) 
      ));

    (* Case B: Returns True *)
    (fun ?l:(p = (true : [%v: Path.t])) ->
      ( allA,
        ExistsReq (path == p),
        (ExistsResp (exists == true); allA) 
      ))
  |]
```

*   **Explanation of Simplification**:
    *   **Decoupled State**: The result of `existsReq` is now completely independent of the actual history of `Create` and `Delete` operations. The tool must handle the possibility that `exists` returns true for a non-existent file, or false for an existing one.

## 5. Global Property (Goal)

### Original Property
```ocaml
(* delete a none empty directory *)
let[@goal] filesystem (chp : Path.t) =
  allA;
  DeleteReq (path == parent chp);
  DeleteResp (success == true);
  allA;
  ExistsReq (path == chp);
  ExistsResp (exists == true);
  allA
```

*   **Explanation**:
    *   This property describes a **Bug Trace** (Counter-example).
    *   It looks for a sequence where:
        1.  We successfully delete a directory (`parent chp`).
        2.  Later, we check if its child (`chp`) exists.
        3.  The system reports that the child *does* still exist.
    *   **Invariant Violation**: In a correct filesystem, deleting a directory should either fail if it's not empty, or recursively delete children. It should not leave "dangling" children. If this trace is feasible, the implementation is incorrect.

*   **Note**: Global properties are typically not "weakened" in the same way as uHATs, because they define *what* we are looking for. Weakening the uHATs (as done above) makes it *easier* for the verification tool to find spurious counter-examples that match this goal (e.g., because `existsReq_weak` allows returning `true` arbitrarily, even if the parent was deleted).
