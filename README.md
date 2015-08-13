NiceGrid
========



Description of NiceGrid /author Priyatna/

NiceGrid is a Delphi component that aimed to be a standard string grid replacement. It is written from scratch, not descended from TStringGrid. The main reason why I write this component is to have a grid component that nice and smooth. Here's some feature of NiceGrid:
  * Headers can be merged and or multilined. 
  * Smooth scrolling, not aligned to top left cell coordinate. 
  * All aspect of grid colors can be customized: header light color, header dark color, header color, grid color, text color, etc.; resulting a real nice looking grid. 
  * Alternate row color. 
  * Can be customized at design time. 
  * Each column can have its own horizontal and vertical alignment, color, and font. 
  * Each column can be hidden. 
  * Can be auto fit to width. 
  * Can be auto calculate column width. 
  * BeginUpdate and EndUpdate method for bulk cells access. 
Since it is a new component, there are several main differences between NiceGrid and TStringGrid:
  * Headers are excluded from cells, unlike TStringGrid that treats fixed rows as regular cells (Row 0, for example), Cells[0,0] in NiceGrid will access the top left editable cells, not fixed cell.

  * The only way to access the data is using Cells property or using direct array referencing style: NiceGrid1[0,0]. There are not (yet) Cols, or Rows property.

  * FixedRows -> Header, FixedCols -> Gutter. 

2014-10-15, author Nedko Ivanov

 * Added Options property (ngoMultiCellSelect, ngoThemed, ngoExcel)
   * ngoMultiCellSelect - allows multi cell select
   * ngoThemed - use Windows theme support instead of original owner drawn selection
   * ngoExcel - enable/disable excel like capabilities (the small dot on the right bottom selection corner)
 * Added property EditorType to Column : (ngetEdit, ngetEditInteger, ngetEditFloat, ngetCombo, ngetComboNoEditText)
 * Added key F2 for begin edit
 * Added event OnEditorCreating and OnEditorCreated (allows to fill in lines in combo box cell editor)
 * Removed component TNiceGridSync
 * Removed additinal support for caret
 * Added event OnFormatText to both grid and column objects
 * Added event OnGetCellColor
 * Added event OnDrawBackground - allows the app to draw the background itself instead by the default grid code
 * Added enumerator for enumerating grid rows with the "for-in" statement

For sample see BasicDemo
