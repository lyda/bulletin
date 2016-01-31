# Recreating bulletin

Back in university we used to use a system called BULLETIN on the
local VAX/VMS cluster.  It eventually went away due to security issues.
But I was thinking it might be fun to recreate.  First though I'd like
an historic git repo that covers the early years.

This repo is not it.  It's where I attempt to recreate it.

## Who wrote it

First, git commits need an author.  It would be nice to figure out
the author.  The version I used I suspect was written by
[Mark London](http://web.mit.edu/london/www/home.html).

## Where to find BULLETIN

The place to get it seems to be the
[DECUS archives](http://decuslib.com/).
I tracked it down with help from Kent Brodie who I discovered via
[an old USENET post](https://groups.google.com/forum/#!search/bulletin$20vms/comp.os.vms/rzM2LQMl6Jo/y1BKhO7dv80J)
where he too was trying to track the software down. In 1994.

## Extracting from tape

To trawl through looking for bulletin source distros (using the
[zip files](http://decuslib.com/zips/) from decuslib:

```
for f in *.zip; do
  unzip -l $f 2> /dev/null | grep -qi /bulletin && echo $f;
done
```

That yields this list of zip files:

I did this on an internet server since my bandwidth is tiny. I only
copied down the relevant bits of the zip files which I extracted like so:

```
for f in *.zip; do
  unzip -l $f 2> /dev/null | grep -qi /bulletin \
    && unzip $f $(unzip -l $f | grep -i /bulletin | awk '{print $4}');
done
tar jcf decus.tar.bz2 decus
```

This was used to create `decus.tar.bz2` which was then extracted as
`decus/` in this archive.

## Creating the BULLETIN source repo (or branch)

Still trying to decide the end result here. I was going to do a separate
source repo but there's a lot of conversion steps I'd like to capture.
So the `master` branch might turn into a `decus2git` branch and then once
the gross conversion steps are done, each tape would get it's own orphan
commit branch (with the correct `GIT_AUTHOR` and commit info metadata).
Something like `tapes/DECUS_TAPE_NAME`. Then the `master` branch would
be created by merging in each tape branch in chronological order.

The BULLETIN git repo is created by a shell script, `mkBULLETIN.sh`
based on edits to the various versions of bulletin that were extracted
from the ZIP archives.

The files in the ZIP archives are not ready in their current state to
make the repo. A number of things had to be done to get them ready for
a proper historical source code archive.  The commit logs for this repo
cover those steps, but the following sections explain the steps in
more detail.

### Dependencies

The shell snippets assume zsh (the `**/*.ext` idiom gets used a lot).
Need a `gcc` toolchain.  Need the `unzip`, `unar` and `zoo` utilities.

### File conversions

Some files are in VMS record formats.  These are handled by
`convert-vms-record-fmt.c` which is compiled to `convert-vms-record-fmt`.

### .mai files

I think these are mail archives.  Might need a script to turn those
into mbox files.

### Unpacking archives.

Within the zip files there are sometimes other archive files.
These had to be extracted.

For .zoo files:

```
for f in **/*.zoo; do
  (cd ${f%\/*} && zoo -extract ${f##*\/} && rm ${f##*\/});
done
```

For .zip files:

```
for f in **/*.zip; do
  (cd ${f%\/*} && unzip ${f##*\/} && rm ${f##*\/});
done
```

For .lzh files:

```
for f in **/*.lzh; do
  (cd ${f%\/*} && unar ${f##*\/} && rm ${f##*\/});
done
```

### .com files

So it might be possible to run DCL on Linux.  A rather
[exhaustive list](http://jonesrh.info/dcll/dcll_why_i_use.html)
of the options seems to indicate that
[PC-DCL](http://users.skynet.be/michel.valentin/) might be an option.

### General cleanup

  * Removed exe and obj files: `git rm **/*.{exe,obj}`

### Committing to the BULLETIN git repo

To get the right dates and authors in git, need to tweak these environment
variables for each commit.

```
export GIT_AUTHOR_NAME="Mark London"
export GIT_AUTHOR_EMAIL="mrl%foo@bar"
export GIT_AUTHOR_DATE="Jan 28 20:52:53 1982 +0000"
```
