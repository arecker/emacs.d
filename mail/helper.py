FOLDER_MAP = {
    'INBOX': 'personal-inbox',
    '[Gmail]/All Mail': 'personal-archives',
    '[Gmail]/Drafts': 'personal-drafts',
    '[Gmail]/Sent Mail': 'personal-sent',
    '[Gmail]/Trash': 'personal-trash',
}

WORK_FOLDER_MAP = {
    'Archive': 'work-archive',
    'Deleted Items': 'work-trash',
    'Drafts': 'work-drafts',
    'INBOX': 'work-inbox',
    'Sent Items': 'work-sent',
}


def filter_folders(folder):
    return folder in FOLDER_MAP.keys()


def filter_folders_work(folder):
    return folder in WORK_FOLDER_MAP.keys()


def trans_local_to_remote(folder):
    inverted = {v: k for k, v in FOLDER_MAP.items()}
    return inverted.get(folder, folder)


def trans_local_to_remote_work(folder):
    inverted = {v: k for k, v in WORK_FOLDER_MAP.items()}
    return inverted.get(folder, folder)


def trans_remote_to_local(folder):
    return FOLDER_MAP.get(folder, folder)


def trans_remote_to_local_work(folder):
    return WORK_FOLDER_MAP.get(folder, folder)
