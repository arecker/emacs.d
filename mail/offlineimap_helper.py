import os
import subprocess


FOLDER_MAP = {
    'archive': '[Gmail]/All Mail',
    'drafts': '[Gmail]/Drafts',
    'important': '[Gmail]/Important',
    'inbox': 'INBOX',
    'sent': '[Gmail]/Sent Mail',
    'spam': '[Gmail]/Spam',
    'starred': '[Gmail]/Starred',
    'trash': '[Gmail]/Trash',
}

INVERTED_FOLDER_MAP = dict([
    (v, k) for k, v
    in FOLDER_MAP.items()
])


def read_password(account):
    home = os.environ['HOME']
    target = os.path.join(
        home,
        '.emacs.d/mail/passwords/{}.txt.gpg'.format(account)
    )
    args = ["gpg", "--use-agent", "--quiet", "--batch", "-d", target]
    return subprocess.check_output(args).strip()


def to_remote(local):
    try:
        return FOLDER_MAP[local]
    except KeyError:
        return local


def to_local(remote):
    try:
        return INVERTED_FOLDER_MAP[remote]
    except KeyError:
        return remote
