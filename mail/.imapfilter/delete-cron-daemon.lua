
---------------
--  Options  --
---------------

options.timeout = 600
options.subscribe = true


-- Connect to tuw mail server

function get_pwd()
    local cmd = "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/Email/tuw.gpg"
    local handle = io.popen(cmd)
    local pwd = handle:read("*l")
    handle:close()
    return pwd
end

pwd = get_pwd()

account1 = IMAP {
    server = 'mail.intern.tuwien.ac.at',
    ssl = 'auto',
    port = 993,
    username = 'amccartn',
    -- FIXME: figure out why this wont accept a string from the above function
    password = get_pwd(), 
}

mailboxes, folders = account1:list_all()

results = account1.INBOX:contain_from('(Cron Daemon)')
print(#results .. " to delete")
results:delete_messages()

