return {
    cmd = {'clangd', '--background-index', '--suggest-missing-includes'},
    root_markers = {'compile_commands.json', 'compile_flags.txt'},
    filetypes = {'c', 'cpp'},
}
