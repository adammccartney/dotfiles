local R = {}

R.todo = function() print("another message") end


-- Extract the visually selected block contained in body, 
-- place it in the newly created function called "name".
local function get_visual_selection()
    local s_start = vim.fn.getpos("'<")
    local s_end = vim.fn.getpos("'>'")
    local n_lines = math.abs(s_end[2] - s_start[2]) + 1
    local lines = vim.api.nvim_buf_get_lines(0, s_start[2] - 1, s_end[2], false)
    lines[1] = string.sub(lines[1], s_start[3], - 1)
    if n_lines == 1 then
        lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3] - s_start[3] + 1)
    else
        lines[n_lines] = string.sub(lines[n_lines], 1, s_end[3])
    end
    return table.concat(lines, '\n')
end

-- Assume that we select the function body visually in the open buffer.
R.get_body = function ()
    local text_table = get_visual_selection()
    print(text_table)
end


-- Copy the selection to an empty slot above the current function
-- Find the current node using the tresitter api
--
-- In all cases I can think of we're going to try to extract part of the body of
-- a function to another function. We need to consider the following cases:
-- 1. Simple function
-- 2. Class method
-- 3. nested function
--
-- Delete the selection 

return R
