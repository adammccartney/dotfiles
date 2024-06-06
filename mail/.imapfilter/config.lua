---------------
--  Options  --
---------------

options.timeout = 120
options.subscribe = true


-- Connect to tuw mail server

function get_pwd()
    local cmd = "gpg -q --for-your-eyes-only --no-tty -d ~/.password-store/Email/tuw.gpg"
    local handle = io.popen(cmd)
    local pwd = handle:read("*a")

    return pwd
end

pwd = get_pwd()

account1 = IMAP {
    server = 'mail.intern.tuwien.ac.at',
    username = 'amccartn',
    -- FIXME: figure out why this wont accept a string from the above function
    password = nil 
}

mailboxes, folders = account1:list_all()

results = account1.INBOX:contain_to('admin@vsc.ac.at')
results:move_messages(account1.vsc_admin)

