# Road Map

There are a few personal goals for this repo. If other people join in,
they might have more to add. For now the road map for this repo looks
like this:

  1. (currently here) Gather the files.  So far the only source seems
     to be decuslib.com.  However there might be others so I'll send
     some more emails over the next few weeks to see.
  2. Build a timeline from the various versions that have been acquired.
     Use that to populate a `historical` branch and the `master` branch
     as well.
  3. Try to document how bulletin looked. One way would be to get it
     running on an emulator.  So far I've only found VMS 7.3.  A copy
     of VMS 4 or 5 would be better.  Emulation is really only useful
     for getting an idea of what it looked like - for me I don't
     know VMS well enough to secure it for exposure to the net.
     Alternatively, it might be possible to port it.  This gets
     explored further down, but it would not be easy.
  4. Assuming a port isn't done (likely), reimplement in Go. Just the
     local bulletin functionality initially. And make it simple. Just use
     the local fs and setuid/gid-type functionality. Mimic the old ISAM
     VMS stores with protobufs and some sort of file locking.  Creating
     protobuf services to exchange boards could come later.  But generally
     no web, not client-server, just plain-old file-based IPC.

## Emulation

  * [Step by step guide](http://www.wherry.com/gadgets/retrocomputing/vax-simh.html) for getting OpenVMS 7.3 up and running on simh.
  * [Notes on networking](https://vanalboom.org/node/18) and clustering on simh. Might not need this.

## Port to Linux

While a rewrite is preferred, a port might be possible.  Some interesting
links on getting VMS FORTRAN running on a Linux/Unix system:

  * A [script](http://www.simconglobal.com/vms.html) to do this
    using a proprietary tool called FTP. Need to contact them about
    licensing.
  * Things to [look out for](http://labs.hoffmanlabs.com/node/225).

## Recreating the timeline

Some thoughts on this. Use announcements for the commit messages. Review
the code to figure out something like a changelog. Commit the
converted/unpacked raw code in one commit (with author set to Mark London,
given the right date, etc); add additional files like a README.md and
a ChangeLog in a follow-on commit with the author of those files.

The code commits should have the following elements.

  * Source: zip file name, decuslib url, original path in that zipfile.
  * File listing with date info.
  * Announce text.
  * Any additional notes.
