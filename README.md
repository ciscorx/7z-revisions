# 7z-revisions
This Emacs app is not really a version control system, but simply an incremental backup system that easily lets you browse and search past saves of a single file, keeping incremental diffs in a .7z archive.   useful when git is considered over-kill.
Also, provides word by word differential highlighting.  Also, provides syntax coloring when viewing raw diff files.
Compatible with windows and linux.

 Commentary:

 7z-revisions-mode is an Emacs minor mode that saves the current
 buffer to a 7-zip archive of the same name, whenever a
 save-buffer command is issued.  A timestamp in the form of
 MMDDYY-HHMMSS is appended to the archived file.  If the .7z
 archive file already exists then it incrementally saves the
 latest revision by adding a new patch to the archive.  The .7z
 extension can be altered to something else, such as .8z, by
 setting the global variable 7zr-archive-extension to ".8z".
 Additionally, the function 7z-revisions can be called
 interactively to view or consolidate past revisions in the
 archive.

 Some useful commands when editing your document:
     M-x 7zr-line-last-changed-on
     M-x 7zr-goto-line-of-last-revision 
     
 When 7z-revisions is called the following key bindings are enabled:
 Enter = view revision at point, 
 q = quit, 
 c = consolidate region, 
 g = goto date,  
 h = toggle highlight differences,
 j = view the raw diff file at point
 a = view all selected diff files in one buffer 
 \# = prompt input of sha1 checksum and search for it

 While viewing individual past revisions:
 q = quit, 
 n = next, 
 p = previous,  
 j = view the raw diff file

 When highlight changes is enabled in view mode:
 d = jump to next difference/change, 
 e = jump to previous change,

 While viewing a raw diff file:
 q = quit,
 n = next,
 p = previous,
 r = switch to revision view
 d = jump to next change hunk
 e = jump to previous change hunk

 There are also some functions in the menu which provide for
 consoldating the current days worth of changes, or last hour
 worth of changes, etc.

 Also, if you know part of a sha1hash value and dont know the
 revision, theres a menu option to goto the revision pertaining to the
 hash.

 Required features:
   hl-line+.el
   p7zip
   diffutils  ( just the patch and diff commands )

 When running on windows, access to additional dos commands are necessary, such as patch, diff, awk, fciv, and optionally grep.
   Install diffutils for windows: http://gnuwin32.sourceforge.net/packages/diffutils.htm and then append C:\Program Files\GnuWin32\bin, or whatever directory that happens to contain diff.exe, to the path variable in control panel -> system -> Advanced -> Environment Variables.  Alternatively, you could just throw all the files in c:\windows\system32
   Install patch.exe for windows:  http://gnuwin32.sourceforge.net/packages/patch.htm then put it in the same directory that contains diff.exe
   Download awk from http://gnuwin32.sourceforge.net/packages/gawk.htm   
   Download the sha1sum equivalent, fciv, https://www.microsoft.com/en-us/download/confirmation.aspx?id=11533
   Download 7zip https://www.7-zip.org/download.html and then put 7z.exe and 7z.dll in windows/system32 directory, or any directory listed in the path environment variable
   Download grep http://gnuwin32.sourceforge.net/packages/grep.htm (optional) and then follow the same instructions as above 

 
 Known Bugs:

 - File names must contain at least 1 alphabetical character or
 underscore or hyphen, and in this regard, cannot take the form of a
 real number, e.g. "1.0".  
 - Each archive can only track one file.  (let's call this a
 feature)
 - There's no way to add commit or revision notes.
 - buffer local variables arent working properly enough to allow for
     two archives to be opened at once.  More precisely, it appears
     that elisp has trouble with using a buffer local variable to
     store a vector; it only seems to store the first element.
     However, elisp seems to have no problem with buffer local lists.
 - When viewing some middle revision, followed by the original
     version, then followed by the first revision, it hangs
     indefinitely, where upon the C-g key must be invoked.
 - Words added to beginning of line additionally highlight following
     word green. In some cases highlighting is off by 1 word.

  This program was written using emacs 23.3.1 on ubuntu 12.04, but is
    compatible with windows-xp and probably windows 7
