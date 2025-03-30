return {
    cmd = {"tflint", "--langserver"},
    root_markers = {".terraform", ".git", ".tflint.hcl"},
    filetypes = {"terraform"},
}
