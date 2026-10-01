options.timeout = 120
options.subscribe = true

function get_pwd()
    local cmd = "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/Email/tuw.gpg"
    local handle = io.popen(cmd)
    local pwd = handle:read("*l")
    handle:close()
    return pwd
end

account1 = IMAP {
    server = 'mail.intern.tuwien.ac.at',
    ssl = 'auto',
    port = 993,
    username = 'amccartn',
    password = get_pwd(),
}

local doomed = account1.vsc_admin:select_all()
print(#doomed .. " messages in vsc_admin; deleting")
doomed:delete_messages()
