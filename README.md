# tvshow-conv-rename

This simple tool will scan a directory and subdirectories, in order to rename
every video file by the conventional format:
```
<Shown>.S<season>E<episode>.<Quality>.<Source>.<Encoding>
```

At start, it will ask for the "meta" informations, and according to the
alphabetical orderding of files will prompt to rename each episode (this allows
to change the information for a specific episode).
It supports special seasons and episodes.

## Todo:
* Change season number when entering into new subdirectory.
* Using a specification of current file format, generate the correct metadata.
* Using this same specification, rename subtitle files, or simply create a mode
  that enter a subtitle folder, renames them and copy them in the directory of
  their associated video file.
* Unsafe mode: no prompt file by file, only for the metadata and possibly when
  entering subdirectories.
