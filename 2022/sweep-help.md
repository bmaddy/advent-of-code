# Help getting sweep running (Failed to locate ‘sweep-module’)

I'm trying to get sweep working in Doom Emacs and am getting `sweeprolog--ensure-module: Failed to locate ‘sweep-module’`

I'm on linux and am running emacs from the terminal with the `LD_PRELOAD` environment variable. `ldd` suggests it will load first as expected:
```
$ LD_PRELOAD=/home/bmaddy/.asdf/installs/swiprolog/9.0.0/lib/swipl/lib/x86_64-linux/libswipl.so ldd $(which emacs) | head
	linux-vdso.so.1 (0x00007ffe87742000)
	/home/bmaddy/.asdf/installs/swiprolog/9.0.0/lib/swipl/lib/x86_64-linux/libswipl.so (0x00007f619ae1c000)
	libtiff.so.5 => /lib/x86_64-linux-gnu/libtiff.so.5 (0x00007f619ad7e000)
	libjpeg.so.8 => /lib/x86_64-linux-gnu/libjpeg.so.8 (0x00007f619acfd000)
	libpng16.so.16 => /lib/x86_64-linux-gnu/libpng16.so.16 (0x00007f619acc2000)
	libz.so.1 => /lib/x86_64-linux-gnu/libz.so.1 (0x00007f619aca6000)
	libgif.so.7 => /lib/x86_64-linux-gnu/libgif.so.7 (0x00007f619ac99000)
	libgtk-3.so.0 => /lib/x86_64-linux-gnu/libgtk-3.so.0 (0x00007f619a470000)
	libgdk-3.so.0 => /lib/x86_64-linux-gnu/libgdk-3.so.0 (0x00007f619a369000)
	libpango-1.0.so.0 => /lib/x86_64-linux-gnu/libpango-1.0.so.0 (0x00007f619a302000)
```

When I look at the running process, I can see that it is loaded:
```
$ # start emacs
$ LD_PRELOAD=/home/bmaddy/.asdf/installs/swiprolog/9.0.0/lib/swipl/lib/x86_64-linux/libswipl.so emacs
$ # in a different terminal
$ lsof -wp $(pidof emacs) | grep swipl
emacs   843198 bmaddy  mem       REG              259,4  1984648  65152305 /home/bmaddy/.asdf/installs/swiprolog/9.0.0/lib/swipl/lib/x86_64-linux/libswipl.so.9.0.0
```

`M-x describe-variable` has `"/home/bmaddy/.asdf/shims"` first in the list of paths and using `M-x shell-command` to run `swipl --version` gives
> SWI-Prolog version 9.0.0 for x86_64-linux

I manually evaluate `(require 'sweeprolog)` and it prints
> Evaluating...
> sweeprolog

Then I open a prolog file and run `M-x sweeprolog-mode` and get this error:
> sweeprolog--ensure-module: Failed to locate ‘sweep-module’. Make sure SWI-Prolog is installed and up to dateError during redisplay: (jit-lock-function 1) signaled (error "Failed to locate ‘sweep-module’. Make sure SWI-Prolog is installed and up to date")
> Error during redisplay: (jit-lock-function 501) signaled (error "Failed to locate ‘sweep-module’. Make sure SWI-Prolog is installed and up to date")

Does anyone have tips on how to troubleshoot this? I'm not sure where to look next.

Some version info:
```
$ uname -a
Linux biz 5.15.0-53-generic #59-Ubuntu SMP Mon Oct 17 18:53:30 UTC 2022 x86_64 x86_64 x86_64 GNU/Linux

$ swipl --version
SWI-Prolog version 9.0.0 for x86_64-linux

$ emacs --version
GNU Emacs 27.1
Copyright (C) 2020 Free Software Foundation, Inc.
GNU Emacs comes with ABSOLUTELY NO WARRANTY.
You may redistribute copies of GNU Emacs
under the terms of the GNU General Public License.
For more information about these matters, see the file named COPYING.

$ ~/.emacs.d/bin/doom --version
GNU Emacs     v27.1            nil
Doom core     v3.0.0-dev       HEAD -> master, origin/master, origin/HEAD c44bc81a0 2022-08-19 11:24:34 +0200
Doom modules  v22.08.0-dev     HEAD -> master, origin/master, origin/HEAD c44bc81a0 2022-08-19 11:24:34 +0200

Copyright (c) 2014-2022 Henrik Lissner.

Doom Emacs uses the MIT license and is provided without warranty of
any kind. You may redistribute and modify copies if given proper
attribution. See the LICENSE file for details.
```









# Resolution

For anyone else running into this same issue, here's the setup that I ended up using.

In `.doom.d/packages.el`:
```
(package! sweeprolog
  :recipe
  (:type git
   :host nil
   :repo "https://git.sr.ht/~eshel/sweep"
   :branch "main"
   :files ("*.el" "sweep.pl")))
```

In `.doom.d/config.el`:
```
(use-package! sweeprolog
  :mode (("\\.pl\\'" . sweeprolog-mode)
         ("\\.plt\\'" . sweeprolog-mode)))
```

I also copied `/usr/share/applications/emacs27.desktop` to `~/.local/share/applications/` and changed
```
Exec=emacs27 %F
```
to
```
Exec=env LD_PRELOAD=/home/bmaddy/.asdf/installs/swiprolog/9.0.0/lib/swipl/lib/x86_64-linux/libswipl.so emacs27 %F
```

Thanks again everyone!
