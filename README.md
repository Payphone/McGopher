# McGopher
McGopher is a Gopher client written in Common Lisp using the
[McCLIM](https://github.com/robert-strandh/McCLIM) toolkit. It is currently only
available on platforms that support running an X server, as McCLIM currently
relies on [CLX](https://github.com/sharplispers/clx) as its primary backend.
## Running
Assuming you have [Quicklisp](https://www.quicklisp.org/beta/) installed clone
the repository into your quicklisp folder.
```
cd ~/quicklisp/local-projects/
git clone https://github.com/Payphone/McGopher
```
Then from your lisp implementation run
```
(ql:quickload :mcgopher)
(mcgopher:main)
```
## Configuring
Inside the src folder there is a file named config.lisp, this is used to
configure keybindings, colors, and other various settings. Change these as you
like, then reload McGopher.

## Screenshots
![McGopher](/screenshots/McGopher.gif?raw=true "McGopher")
