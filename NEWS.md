# exifer 1.7.0
- `flatten_subject` now creates tags out of the ratings for export photos (instead of summing up 4 & 5 stars to 'Apple-Favourite). Tags may be then reused in Apple Photos to set ratings again (macOS >= 27).

# exifer 1.6.6
- added metadata for iPhone 17 Pro

# exifer 1.6.5
- added metadata for Pixel Pro phone

# exifer 1.6.4
- bugfix problem with NA values in location and other tags
- fix warning issues when updating subject tag
- fix warning issues for joins
- updated packages

# exifer 1.6.3
-  bugfix issue with `EXIF:CreateDate`

# exifer 1.6.2
-   bugfix issue with `EXIF:OffsetTimeDigitized`

# exifer 1.6.1
-   bugfix offset

# exifer 1.6.0

-   added time harmonization functionality for iphone pix (offsets etc.) in `exiftool_time()`
-   additional parameter for offset in `prepare_export()`
-   new helper function `convert_hours()` (for offsets)

# exifer 1.5.12

-   added Sigma 150-600mm to lens mapping database

# exifer 1.5.11

-   added crop factor information for Nikon D7200

# exifer 1.5.10

-   bugfix `after_export()`:
    -   added parameter for correct separation of keywords

# exifer 1.5.9

-   same as v1.5.7-8 but for after export and cropfactor

# exifer 1.5.8

-   added a iPhone 13 mini lens to mapping table

# exifer 1.5.7

-   bugfixes in `prepare_export()`:
    -   double qouting the system command with `find`
    -   added parameter for correct separation of keywords

# exifer 1.5.6

-   bugfix in `transfer_metadata()`:

    the delete_original param for exiftool was wrong (should be overwrite_original)

# exifer 1.5.5

-   introduced github workflow for automatic release

# exifer 1.5.4

-   bugfix in `transfer_metadata()`:

    one of the plausibility checks was wrong. Replaced by a new one

-   fixed README

# exifer 1.5.3

-   Initial NEWS file.
