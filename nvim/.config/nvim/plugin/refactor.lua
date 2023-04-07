-- refactor.lua
-- A module containing some handy functions for refactoring
-- Maintainer: Adam McCartney <adam@mur.at>



local R = {}
--
--
---- Extract the visually selected block contained in body, 
---- place it in the newly created function called "name".
--function M.extract_function(body, name)
--    --buf = nvim_get_current_buf() 
--    --start, end = nvim_buf_get_mark(buf)
--    --lines = nvim_buf_get_lines(buf, start, end) 
--    --nvim_command("echo", lines)
--end

function R.ToDo() 
    print("Hello world!") 
end

return R

