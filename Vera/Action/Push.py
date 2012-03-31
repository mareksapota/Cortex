from __future__ import print_function

import tempfile
import os
import os.path
import subprocess
import hashlib
import sys

import Config
import Miranda

supported_repo_types = [
    'git',
    'hg',
    'bzr',
    'plain'
]

def perform(repo_type):
    # Perform repository type discovery if needed.
    if repo_type == 'auto':
        if os.path.exists('.git') and os.path.isdir('.git'):
            print('Discovered Git repository')
            repo_type = 'git'
        elif os.path.exists('.hg') and os.path.isdir('.hg'):
            print('Discovered Mercurial repository')
            repo_type = 'hg'
        elif os.path.exists('.bzr') and os.path.isdir('.bzr'):
            print('Discovered Bazaar repository')
            repo_type = 'bzr'
        else:
            print('Could not determine repository type, using plain')
            repo_type = 'plain'

    # Abandon if repository type is not supported.
    if repo_type not in supported_repo_types:
        print('Repository type "{0}" is not supported.'.format(repo_type))
        print('Supported repository types:')
        for r in supported_repo_types:
            print('  {0}'.format(r))
        sys.exit(1)

    # Create a temporary file to store the repo.
    (fd, tmp, ) = tempfile.mkstemp(suffix = '.tar')
    os.close(fd)

    if repo_type == 'git':
        subprocess.check_call(
            'git archive --format=tar --prefix=repo/ HEAD > "{0}"'.format(tmp),
            shell = True
        )
    elif repo_type == 'hg':
        subprocess.check_call(
            'hg archive --prefix=repo/ -r tip "{0}"'.format(tmp),
            shell = True
        )
    elif repo_type == 'bzr':
        subprocess.check_call(
            'bzr export --root=repo/ "{0}"'.format(tmp),
            shell = True
        )
    elif repo_type == 'plain':
        subprocess.check_call(
            'tar --transform "s,^,repo/," -cf "{0}" .'.format(tmp),
            shell = True
        )

    subprocess.check_call(
        'base64 -w 0 "{0}" > "{0}.base64"'.format(tmp),
        shell = True
    )

    sha1 = hashlib.sha1()
    f = open('{0}.base64'.format(tmp), 'r')
    while True:
        data = f.read(4096)
        sha1.update(data)
        if not data:
            break
    f.close()
    sha1 = sha1.hexdigest()

    try:
        key = 'app::source::{0}'.format(Config.name)

        m = Miranda.connect()
        m.send('lookup hash')
        m.send(key)
        response = m.recv()
        m.close()

        if response == 'Just {0}'.format(sha1):
            print('Miranda already has this repository version.')
            sys.exit(0)

        m = Miranda.connect()
        m.send('set')
        m.send(key)
        f = open('{0}.base64'.format(tmp), 'r')
        while True:
            data = f.read(4096)
            m.send_part(data)
            if not data:
                break
        m.send_part_finish()
        f.close()
        m.close()

        print('Uploaded new repository version to Miranda.')
    except Exception as e:
        print('Miranda communication error: {0}'.format(e))
    finally:
        # Remove the temporary file.
        os.unlink(tmp)
        os.unlink('{0}.base64'.format(tmp))
